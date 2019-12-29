port module Main exposing (..)

import BasicAuth
import Dict exposing (Dict)
import Dom.Scroll
import GoogleMap exposing (googleMap, googleMapMarker)
import GoogleMap.Attributes exposing (apiKey, dragEvents, latitude, longitude, zoom)
import GoogleMap.Events exposing (MapEvent, onDragend, onReady, onZoomChanged)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Navigation exposing (Location)
import Task
import Time exposing (Time, millisecond)



-- Downtown San Diego


defaultLatitude : Float
defaultLatitude =
    32.71143062


defaultLongitude : Float
defaultLongitude =
    -117.1600173


defaultZoom : Int
defaultZoom =
    15


defaultFilteredAssetType : String
defaultFilteredAssetType =
    "CAMERA"


type alias Bounds =
    { north : Float
    , south : Float
    , east : Float
    , west : Float
    }


type alias MapState =
    { latitude : Float
    , longitude : Float
    , zoom : Int
    , bounds : Bounds
    }


type alias CityIQAsset =
    { assetUid : String
    , parentAssetUid : Maybe String
    , eventTypes : List String
    , assetType : String
    , latitude : Float
    , longitude : Float
    }


type ConsoleMessage
    = Info String
    | Error String


type alias AuthenticatedModel =
    { accessToken : String
    , checkPointMapState : MapState
    , bounds : Bounds
    , filteredAssetType : String
    , assets : Dict String CityIQAsset
    , message : Maybe ConsoleMessage
    , ignoreLocationChange : Bool
    , dirty : Bool
    }


type Model
    = AwaitingMapBounds Float Float Int
    | AwaitingAuthentication MapState
    | FailedAuthentication String
    | Authenticated AuthenticatedModel


type Msg
    = MapReady MapEvent
    | MapPannedZoomed MapEvent
    | NewMapBounds MapState
    | NewAccessToken (Result Http.Error String)
    | NewLocation Location
    | NewAssetTypeFilter String
    | GetAssetMetadata
    | NewAssetMetadata (Result Http.Error (List CityIQAsset))
    | GetAssetEvent { assetUid : String, eventType : String, startTime : Time, endTime : Time }
    | NewAssetEvent (Result Http.Error Value)
    | NoOp


latLngZoom : Location -> Maybe ( Float, Float, Int )
latLngZoom location =
    case String.split "," (String.dropLeft 1 location.hash) of
        latStr :: lngStr :: zoomStr :: [] ->
            Maybe.map3
                (\lat -> \lng -> \zoom -> ( lat, lng, zoom ))
                (Result.toMaybe (String.toFloat latStr))
                (Result.toMaybe (String.toFloat lngStr))
                (Result.toMaybe (String.toInt zoomStr))

        "" :: [] ->
            Just ( defaultLatitude, defaultLongitude, defaultZoom )

        _ ->
            Nothing


init : Location -> ( Model, Cmd Msg )
init location =
    case latLngZoom location of
        Just ( lat, lng, zoom ) ->
            ( AwaitingMapBounds lat lng zoom
            , Cmd.none
            )

        Nothing ->
            ( AwaitingMapBounds defaultLatitude defaultLongitude defaultZoom
            , Navigation.modifyUrl "/"
            )


getAssetMetadataCmd : String -> String -> Bounds -> Cmd Msg
getAssetMetadataCmd accessToken filteredAssetType bounds =
    Http.send NewAssetMetadata
        (let
            url : String
            url =
                "/proxy/https://sandiego.cityiq.io/api/v2/metadata/assets/search?bbox="
                    ++ toString bounds.north
                    ++ ":"
                    ++ toString bounds.west
                    ++ ","
                    ++ toString bounds.south
                    ++ ":"
                    ++ toString bounds.east
                    ++ "&page=0&size=200&q=assetType:"
                    ++ filteredAssetType

            --&q=eventTypes:TFEVT"
            cityIqAssetDecoder : Decode.Decoder CityIQAsset
            cityIqAssetDecoder =
                Decode.map5
                    (\assetUid ->
                        \parentAssetUid ->
                            \maybeEventTypes ->
                                \assetType ->
                                    \coordinates ->
                                        let
                                            ( lat, lng ) =
                                                case String.split ":" coordinates of
                                                    lat :: lng :: [] ->
                                                        ( Result.withDefault 0.0 (String.toFloat lat), Result.withDefault 0.0 (String.toFloat lng) )

                                                    _ ->
                                                        ( 0.0, 0.0 )

                                            eventTypes : List String
                                            eventTypes =
                                                Maybe.withDefault [] maybeEventTypes
                                        in
                                        CityIQAsset assetUid parentAssetUid eventTypes assetType lat lng
                    )
                    (Decode.field "assetUid" Decode.string)
                    (Decode.field "parentAssetUid" (Decode.maybe Decode.string))
                    (Decode.field "eventTypes" (Decode.maybe (Decode.list Decode.string)))
                    (Decode.field "assetType" Decode.string)
                    (Decode.field "coordinates" Decode.string)

            request : Http.Request (List CityIQAsset)
            request =
                Http.request
                    { method = "GET"
                    , headers =
                        [ Http.header "Authorization" ("Bearer " ++ accessToken)
                        , Http.header "Predix-Zone-Id" "SD-IE-TRAFFIC"
                        ]
                    , url = url
                    , body = Http.emptyBody
                    , expect = Http.expectJson (Decode.field "content" (Decode.list cityIqAssetDecoder))
                    , timeout = Nothing
                    , withCredentials = False
                    }
         in
         request
        )


port clearGoogleMapMarkersCmd : () -> Cmd msg


port googleMapMarkersCmd : List CityIQAsset -> Cmd msg


port initBoundsChangedListenerCmd : Value -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        AwaitingMapBounds _ _ _ ->
            case msg of
                MapReady event ->
                    ( model
                    , initBoundsChangedListenerCmd event.rawEvent
                    )

                NewMapBounds mapState ->
                    ( AwaitingAuthentication mapState
                    , Http.send NewAccessToken
                        (let
                            accessTokenDecoder : Decode.Decoder String
                            accessTokenDecoder =
                                Decode.field "access_token" Decode.string

                            request : Http.Request String
                            request =
                                Http.request
                                    { method = "GET"
                                    , headers = [ BasicAuth.buildAuthorizationHeader "PublicAccess" "qPKIadEsoHjyh226Snz7" ]
                                    , url = "https://auth.aa.cityiq.io/oauth/token?grant_type=client_credentials"
                                    , body = Http.emptyBody
                                    , expect = Http.expectJson accessTokenDecoder
                                    , timeout = Nothing
                                    , withCredentials = False
                                    }
                         in
                         request
                        )
                    )

                unexpected ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( unexpected, model )
                    in
                    ( model, Cmd.none )

        AwaitingAuthentication ({ bounds } as mapState) ->
            case msg of
                NewAccessToken (Ok accessToken) ->
                    ( Authenticated
                        { accessToken = accessToken
                        , checkPointMapState = mapState
                        , bounds = bounds
                        , filteredAssetType = defaultFilteredAssetType
                        , assets = Dict.empty
                        , message = Nothing
                        , ignoreLocationChange = False
                        , dirty = False
                        }
                    , getAssetMetadataCmd accessToken defaultFilteredAssetType bounds
                    )

                NewAccessToken (Err err) ->
                    ( FailedAuthentication (toString err)
                    , Cmd.none
                    )

                NewMapBounds mapState ->
                    ( AwaitingAuthentication mapState
                    , Cmd.none
                    )

                unexpected ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( unexpected, model )
                    in
                    ( model, Cmd.none )

        Authenticated authModel ->
            case msg of
                NewMapBounds { latitude, longitude, zoom, bounds } ->
                    ( Authenticated { authModel | bounds = bounds, dirty = True }
                    , Cmd.none
                    )

                MapPannedZoomed { latitude, longitude, zoom } ->
                    ( Authenticated { authModel | ignoreLocationChange = True }
                    , Navigation.newUrl ("#" ++ toString latitude ++ "," ++ toString longitude ++ "," ++ toString zoom)
                    )

                NewLocation location ->
                    if authModel.ignoreLocationChange then
                        ( Authenticated { authModel | ignoreLocationChange = False }
                        , Cmd.none
                        )

                    else
                        case latLngZoom location of
                            Just ( lat, lng, zoom ) ->
                                let
                                    mapState : MapState
                                    mapState =
                                        authModel.checkPointMapState
                                in
                                ( Authenticated
                                    { authModel
                                        | checkPointMapState =
                                            { mapState | latitude = lat, longitude = lng, zoom = zoom }
                                    }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model
                                , Navigation.modifyUrl "/"
                                )

                NewAssetTypeFilter assetType ->
                    ( Authenticated { authModel | filteredAssetType = assetType, assets = Dict.empty }
                    , Cmd.batch
                        [ clearGoogleMapMarkersCmd ()
                        , getAssetMetadataCmd authModel.accessToken assetType authModel.bounds
                        ]
                    )

                GetAssetMetadata ->
                    ( model
                    , getAssetMetadataCmd authModel.accessToken authModel.filteredAssetType authModel.bounds
                    )

                NewAssetMetadata (Ok assets) ->
                    let
                        incomingAssets : Dict String CityIQAsset
                        incomingAssets =
                            List.foldl
                                (\asset ->
                                    \accum ->
                                        if asset.assetType == "NODE" then
                                            accum

                                        else
                                            Dict.insert asset.assetUid asset accum
                                )
                                Dict.empty
                                assets

                        allAssets : Dict String CityIQAsset
                        allAssets =
                            Dict.union authModel.assets incomingAssets

                        newAssets : Dict String CityIQAsset
                        newAssets =
                            Dict.diff incomingAssets authModel.assets
                    in
                    ( Authenticated { authModel | assets = allAssets, dirty = False }
                    , googleMapMarkersCmd (Dict.values newAssets)
                    )

                NewAssetMetadata (Err ((Http.BadStatus { status, body }) as err)) ->
                    ( if status.code == 500 && String.contains "Assets not found " body then
                        -- API returns 500 when there's no result
                        model

                      else
                        Authenticated { authModel | message = Just (Error (toString err)) }
                    , Cmd.none
                    )

                NewAssetMetadata (Err err) ->
                    ( Authenticated { authModel | message = Just (Error (toString err)) }
                    , Cmd.none
                    )

                (GetAssetEvent { assetUid, eventType, startTime, endTime }) as req ->
                    ( model
                    , Http.send NewAssetEvent
                        (let
                            url : String
                            url =
                                "/proxy/https://sandiego.cityiq.io/api/v2/event/assets/"
                                    ++ assetUid
                                    ++ "/events?eventType="
                                    ++ eventType
                                    ++ "&startTime="
                                    ++ toString startTime
                                    ++ "&endTime="
                                    ++ toString endTime
                                    ++ "&pageSize=100"

                            predixZoneId : String
                            predixZoneId =
                                case eventType of
                                    "PKIN" ->
                                        "SD-IE-PARKING"

                                    "PKOUT" ->
                                        "SD-IE-PARKING"

                                    "TFEVT" ->
                                        "SD-IE-TRAFFIC"

                                    "BICYCLE" ->
                                        "SD-IE-BICYCLE"

                                    "PEDEVT" ->
                                        "SD-IE-PEDESTRIAN"

                                    "PRESSURE" ->
                                        "SD-IE-ENVIRONMENTAL"

                                    "TEMPERATURE" ->
                                        "SD-IE-ENVIRONMENTAL"

                                    "ORIENTATION" ->
                                        "SD-IE-ENVIRONMENTAL"

                                    "HUMIDITY" ->
                                        "SD-IE-ENVIRONMENTAL"

                                    "METROLOGY" ->
                                        "SD-IE-METROLOGY"

                                    "ENERGY_ALERT" ->
                                        "SD-IE-METROLOGY"

                                    "ENERGY_TIMESERIES" ->
                                        "SD-IE-METROLOGY"

                                    _ ->
                                        "UNKNOWN"

                            request : Http.Request Value
                            request =
                                Http.request
                                    { method = "GET"
                                    , headers =
                                        [ Http.header "Authorization" ("Bearer " ++ authModel.accessToken)
                                        , Http.header "Predix-Zone-Id" predixZoneId
                                        ]
                                    , url = url
                                    , body = Http.emptyBody
                                    , expect = Http.expectJson Decode.value
                                    , timeout = Nothing
                                    , withCredentials = False
                                    }
                         in
                         request
                        )
                    )

                NewAssetEvent (Ok jsonValue) ->
                    ( Authenticated { authModel | message = Just (Info (Encode.encode 2 jsonValue)) }
                    , Task.attempt (always NoOp) (Dom.Scroll.toBottom "console")
                    )

                NewAssetEvent (Err err) ->
                    ( Authenticated { authModel | message = Just (Error (toString err)) }
                    , Cmd.none
                    )

                NoOp ->
                    ( model, Cmd.none )

                (MapReady _) as msg ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( msg, model )
                    in
                    ( model, Cmd.none )

                (NewAccessToken _) as msg ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( msg, model )
                    in
                    ( model, Cmd.none )

        FailedAuthentication _ ->
            case msg of
                unexpected ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( unexpected, model )
                    in
                    ( model, Cmd.none )


port boundsChangedSub : (MapState -> msg) -> Sub msg


port getAssetEventsSub : ({ assetUid : String, eventType : String, startTime : Time, endTime : Time } -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ boundsChangedSub NewMapBounds
        , getAssetEventsSub GetAssetEvent
        , case model of
            Authenticated { dirty } ->
                if not dirty then
                    Sub.none

                else
                    Time.every (500 * millisecond) (always GetAssetMetadata)

            _ ->
                Sub.none
        ]


extractLatLngZoom : Model -> Maybe ( Float, Float, Int )
extractLatLngZoom model =
    case model of
        AwaitingMapBounds lat lng zm ->
            Just ( lat, lng, zm )

        AwaitingAuthentication { latitude, longitude, zoom } ->
            Just ( latitude, longitude, zoom )

        Authenticated { checkPointMapState } ->
            Just ( checkPointMapState.latitude, checkPointMapState.longitude, checkPointMapState.zoom )

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ googleMap
            (apiKey "AIzaSyD6jMwmDZ4Bvgee_-mMN4PUqBaK-qitqAg"
                :: dragEvents True
                :: onReady MapReady
                :: onDragend MapPannedZoomed
                :: onZoomChanged MapPannedZoomed
                :: (case extractLatLngZoom model of
                        Just ( lat, lng, zm ) ->
                            [ latitude lat
                            , longitude lng
                            , zoom zm
                            ]

                        Nothing ->
                            [ latitude defaultLatitude
                            , longitude defaultLongitude
                            , zoom defaultZoom
                            ]
                   )
            )
            [ Html.node "link" [ rel "import", href "/assets/javascripts/google-map/google-map.html" ] []
            ]
        , let
            enabled : Bool
            enabled =
                case model of
                    Authenticated _ ->
                        True

                    _ ->
                        False

            assetType : String
            assetType =
                case model of
                    Authenticated { filteredAssetType } ->
                        filteredAssetType

                    _ ->
                        defaultFilteredAssetType
          in
          select
            [ id "asset-type-filter"
            , onInput NewAssetTypeFilter
            , disabled (not enabled)
            ]
            [ option
                [ value "CAMERA", selected (assetType == "CAMERA") ]
                [ text "📷 CAMERA" ]
            , option
                [ value "MIC", selected (assetType == "MIC") ]
                [ text "🎤 MIC" ]
            , option
                [ value "ENV_SENSOR", selected (assetType == "ENV_SENSOR") ]
                [ text "🌡 ENV_SENSOR" ]
            , option
                [ value "EM_SENSOR", selected (assetType == "EM_SENSOR") ]
                [ text "⚡️ EM_SENSOR" ]
            ]
        , div [ id "console" ]
            (case model of
                Authenticated { message } ->
                    case message of
                        Just (Info msg) ->
                            [ pre [] [ text msg ] ]

                        Just (Error msg) ->
                            [ pre [ class "stderr" ] [ text msg ] ]

                        Nothing ->
                            []

                AwaitingMapBounds _ _ _ ->
                    [ pre [] [ text "Awaiting map bounds..." ] ]

                AwaitingAuthentication _ ->
                    [ pre [] [ text "Awaiting authentication..." ] ]

                FailedAuthentication errorMsg ->
                    [ pre [ class "stderr " ] [ text ("Failed to obtain access token: " ++ errorMsg) ] ]
            )
        ]


main : Program Never Model Msg
main =
    Navigation.program
        NewLocation
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
