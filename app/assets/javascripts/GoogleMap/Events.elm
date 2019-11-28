module GoogleMap.Events exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Value)


type alias MapEvent =
    { latitude : Float
    , longitude : Float
    , rawEvent : Value
    }


googleMapReady : (MapEvent -> cmd) -> Attribute cmd
googleMapReady tagger =
    on "google-map-ready"
        (Decode.map3
            (\lat -> \lng -> \raw -> MapEvent lat lng raw |> tagger)
            (Decode.at [ "target", "latitude" ] Decode.float)
            (Decode.at [ "target", "longitude" ] Decode.float)
            Decode.value
        )
