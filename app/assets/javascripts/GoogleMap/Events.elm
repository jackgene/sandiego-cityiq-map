module GoogleMap.Events exposing (MapEvent, onDragend, onIdle, onReady, onZoomChanged)

{-| <https://www.webcomponents.org/element/GoogleWebComponents/google-map/elements/google-map#events>
-}

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Value)


type alias MapEvent =
    { latitude : Float
    , longitude : Float
    , zoom : Int
    , rawEvent : Value
    }


mapEventDecoder : Decode.Decoder MapEvent
mapEventDecoder =
    Decode.map4 MapEvent
        (Decode.at [ "target", "latitude" ] Decode.float)
        (Decode.at [ "target", "longitude" ] Decode.float)
        (Decode.at [ "target", "zoom" ] Decode.int)
        Decode.value


onReady : (MapEvent -> cmd) -> Attribute cmd
onReady tagger =
    on "google-map-ready" (Decode.map tagger mapEventDecoder)


onDragend : (MapEvent -> cmd) -> Attribute cmd
onDragend tagger =
    on "google-map-dragend" (Decode.map tagger mapEventDecoder)


onZoomChanged : (MapEvent -> cmd) -> Attribute cmd
onZoomChanged tagger =
    on "zoom-changed" (Decode.map tagger mapEventDecoder)


onIdle : (MapEvent -> cmd) -> Attribute cmd
onIdle tagger =
    on "google-map-idle" (Decode.map tagger mapEventDecoder)
