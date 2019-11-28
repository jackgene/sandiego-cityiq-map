module GoogleMap exposing (..)

import Html exposing (Attribute, Html)


googleMap : List (Attribute a) -> List (Html a) -> Html a
googleMap =
    Html.node "google-map-wip"


googleMapMarker : List (Attribute a) -> List (Html a) -> Html a
googleMapMarker =
    Html.node "google-map-marker"
