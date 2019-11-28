port module Main exposing (..)

import BasicAuth
import Dict exposing (Dict)
import GoogleMap exposing (googleMap, googleMapMarker)
import GoogleMap.Attributes exposing (apiKey, latitude, longitude, zoom)
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode


type alias Bounds =
    { north : Float
    , south : Float
    , east : Float
    , west : Float
    }


type alias CityIQAsset =
    { assetUid : String
    , parentAssetUid : Maybe String
    , assetType : String
    , latitude : Float
    , longitude : Float
    }


type alias AuthenticatedModel =
    { accessToken : String
    , assets : Dict String CityIQAsset
    }


type Model
    = AwaitingMapBounds
    | AwaitingAuthentication Bounds
    | FailedAuthentication String
    | Authenticated AuthenticatedModel


type Msg
    = NewMapBounds Bounds
    | NewAccessToken (Result Http.Error String)
    | NewContent (Result Http.Error (List CityIQAsset))


init : ( Model, Cmd Msg )
init =
    ( AwaitingMapBounds
    , Cmd.none
    )


getAssetsCmd : String -> Bounds -> Cmd Msg
getAssetsCmd accessToken bounds =
    Http.send NewContent
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
                    ++ "&page=0&size=500"

            --&q=eventTypes:TFEVT"
            cityIqAssetDecoder : Decode.Decoder CityIQAsset
            cityIqAssetDecoder =
                Decode.map4
                    (\assetUid ->
                        \parentAssetUid ->
                            \assetType ->
                                \coordinates ->
                                    let
                                        ( lat, lng ) =
                                            case String.split ":" coordinates of
                                                lat :: lng :: [] ->
                                                    ( Result.withDefault 0.0 (String.toFloat lat), Result.withDefault 0.0 (String.toFloat lng) )

                                                _ ->
                                                    ( 0.0, 0.0 )
                                    in
                                    CityIQAsset assetUid parentAssetUid assetType lat lng
                    )
                    (Decode.field "assetUid" Decode.string)
                    (Decode.field "parentAssetUid" (Decode.maybe Decode.string))
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


port googleMapMarkersCmd : List CityIQAsset -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        AwaitingMapBounds ->
            case msg of
                NewMapBounds bounds ->
                    ( AwaitingAuthentication bounds
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

        AwaitingAuthentication bounds ->
            case msg of
                NewAccessToken (Ok accessToken) ->
                    ( Authenticated (AuthenticatedModel accessToken Dict.empty)
                    , getAssetsCmd accessToken bounds
                    )

                NewAccessToken (Err err) ->
                    ( FailedAuthentication (toString err)
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
                NewContent (Ok assets) ->
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

                NewContent (Err err) ->
                    let
                        _ =
                            Debug.log "Error" err
                    in
                    ( model
                    , Cmd.none
                    )

                NewMapBounds bounds ->
                    ( model
                    , getAssetsCmd authModel.accessToken bounds
                    )

                unexpected ->
                    let
                        _ =
                            Debug.log "Unexpected message/state" ( unexpected, model )
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


port boundsChangedSub : (Bounds -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    boundsChangedSub NewMapBounds


view : Model -> Html Msg
view model =
    case model of
        Authenticated { assets } ->
            googleMap [] []

        AwaitingAuthentication _ ->
            googleMap [] []

        AwaitingMapBounds ->
            googleMap
                [ latitude 32.71143062
                , longitude -117.1600173
                , zoom 15
                , apiKey "AIzaSyD6jMwmDZ4Bvgee_-mMN4PUqBaK-qitqAg"
                ]
                []

        FailedAuthentication errorMsg ->
            div [] [ text ("Failed to obtain access token: " ++ errorMsg) ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
