module Components.Modal exposing (..)

import Color exposing (Color)
import Material.Icons.Navigation exposing (close)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition, linear)
import Css.Animations exposing (keyframes, property)
import Svg exposing (svg)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Svg.Styled exposing (toUnstyled, fromUnstyled)
import Time exposing (..)

type alias Model = 
    {
        message: String,
        modalType: Type,
        show: Bool,
        choice: Maybe Choice
    }

type Msg
    = Show
    | Close

type Type 
    = Warning
    | Information

type Choice
    = Accepted
    | Declined

init: String -> Type -> Model
init message modalType =
    {
        message = message,
        modalType = modalType,
        show = True,
        choice = Nothing
    }

update: Msg -> Model -> Model
update msg model =
    case msg of
        Show ->
            { model | show = True }
        Close ->
            { model | show = False }

view: Model -> Html Msg
view model =
    case model.show of
    True ->
        div[
            css[
                position fixed,
                zIndex (int 1),
                left (px 0),
                top (px 0),
                overflow auto,
                width (pct 100),
                height (pct 100),
                backgroundColor (rgb 0 0 0),
                backgroundColor (rgba 0 0 0 0.4)
            ]
        ][
            div[
                css[
                    padding (px 20),
                    width (pct 50),
                    backgroundColor (hex "#fefefe"),
                    margin2 (pct 15) auto,
                    border3 (px 1) solid (hex "888"),
                    height (pct 30)
                ]
            ][
                span [
                    css[
                        marginLeft (px 15),
                        fontWeight bold,
                        float right,
                        lineHeight (px 20),
                        cursor pointer,
                        hover[
                            opacity (num 0.5)
                        ],
                        transition
                        [ 
                            Css.Transitions.transform3 100 0 easeInOut
                        ]
                    ],
                    onClick Close
                ]
                [   
                    close (Color.rgb255 0 0 0) 20 |> Svg.Styled.fromUnstyled
                ],
                text "This is a modal window"
            ]
        ]
    False ->
        div[][]

modalWarning: Type
modalWarning =
    Warning

modalInformation: Type
modalInformation =
    Information