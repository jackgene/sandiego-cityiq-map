port module Main exposing (..)

import BasicAuth
import Dict exposing (Dict)
import Dom.Scroll
import GoogleMap exposing (googleMap, googleMapMarker)
import GoogleMap.Attributes exposing (apiKey, latitude, longitude, zoom)
import GoogleMap.Events exposing (MapEvent, googleMapReady)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Navigation exposing (Location)
import Task
import Time exposing (Time)



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
    , initMapState : MapState
    , bounds : Bounds
    , filteredAssetType : String
    , assets : Dict String CityIQAsset
    , message : Maybe ConsoleMessage
    }


type Model
    = AwaitingMapBounds Float Float Int
    | AwaitingAuthentication MapState
    | FailedAuthentication String
    | Authenticated AuthenticatedModel


type Msg
    = MapReady MapEvent
    | NewMapBounds MapState
    | NewAccessToken (Result Http.Error String)
    | NewAssetTypeFilter String
    | NewAssetMetadata (Result Http.Error (List CityIQAsset))
    | GetAssetEvent { assetUid : String, eventType : String, startTime : Time, endTime : Time }
    | NewAssetEvent (Result Http.Error Value)
    | NoOp


init : Location -> ( Model, Cmd Msg )
init location =
    let
        latLngZoom : Maybe ( Float, Float, Int )
        latLngZoom =
            case String.split "|" (String.dropLeft 1 location.hash) of
                latStr :: lngStr :: zoomStr :: [] ->
                    Maybe.map3
                        (\lat -> \lng -> \zoom -> ( lat, lng, zoom ))
                        (Result.toMaybe (String.toFloat latStr))
                        (Result.toMaybe (String.toFloat lngStr))
                        (Result.toMaybe (String.toInt zoomStr))

                _ ->
                    Nothing
    in
    case latLngZoom of
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
                                    , headers = [ BasicAuth.buildAuthorizationHeader "PublicAccess" "uVeeMuiue4k=" ]
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
                    ( Authenticated (AuthenticatedModel accessToken mapState bounds defaultFilteredAssetType Dict.empty Nothing)
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
                    ( Authenticated { authModel | bounds = bounds }
                    , Cmd.batch
                        [ getAssetMetadataCmd authModel.accessToken authModel.filteredAssetType bounds
                        , Navigation.modifyUrl ("#" ++ toString latitude ++ "|" ++ toString longitude ++ "|" ++ toString zoom)
                        ]
                    )

                NewAssetTypeFilter assetType ->
                    ( Authenticated { authModel | filteredAssetType = assetType, assets = Dict.empty }
                    , Cmd.batch
                        [ clearGoogleMapMarkersCmd ()
                        , getAssetMetadataCmd authModel.accessToken assetType authModel.bounds
                        ]
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
                    ( Authenticated { authModel | assets = allAssets }
                    , googleMapMarkersCmd (Dict.values newAssets)
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
        ]


extractLatLngZoom : Model -> Maybe ( Float, Float, Int )
extractLatLngZoom model =
    case model of
        AwaitingMapBounds lat lng zm ->
            Just ( lat, lng, zm )

        AwaitingAuthentication { latitude, longitude, zoom } ->
            Just ( latitude, longitude, zoom )

        Authenticated { initMapState } ->
            Just ( initMapState.latitude, initMapState.longitude, initMapState.zoom )

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ googleMap
            (case extractLatLngZoom model of
                Just ( lat, lng, zm ) ->
                    [ latitude lat
                    , longitude lng
                    , zoom zm
                    , apiKey "AIzaSyD6jMwmDZ4Bvgee_-mMN4PUqBaK-qitqAg"
                    , googleMapReady MapReady
                    ]

                Nothing ->
                    [ latitude defaultLatitude
                    , longitude defaultLongitude
                    , zoom defaultZoom
                    , apiKey "AIzaSyD6jMwmDZ4Bvgee_-mMN4PUqBaK-qitqAg"
                    , googleMapReady MapReady
                    ]
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
        (always NoOp)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
