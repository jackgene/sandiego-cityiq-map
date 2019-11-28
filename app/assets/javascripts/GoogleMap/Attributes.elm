module GoogleMap.Attributes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


apiKey : String -> Attribute a
apiKey key =
    attribute "api-key" key


latitude : Float -> Attribute a
latitude lat =
    attribute "latitude" (toString lat)


longitude : Float -> Attribute a
longitude lng =
    attribute "longitude" (toString lng)


zoom : Int -> Attribute a
zoom level =
    attribute "zoom" (toString level)
