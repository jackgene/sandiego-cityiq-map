module GoogleMap.Attributes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


apiKey : String -> Attribute a
apiKey key =
    attribute "api-key" key


dragEvents : Bool -> Attribute a
dragEvents enabled =
    attribute "drag-events" (toString enabled)


label : String -> Attribute a
label name=
    attribute "label" name


latitude : Float -> Attribute a
latitude lat =
    attribute "latitude" (toString lat)


longitude : Float -> Attribute a
longitude lng =
    attribute "longitude" (toString lng)


slot : String -> Attribute a
slot slotType =
    attribute "slot" slotType


zoom : Int -> Attribute a
zoom level =
    attribute "zoom" (toString level)
