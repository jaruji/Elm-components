--popup alert components
--maybe something like ruby on rails

module Components.Alert exposing (Model, Msg, Type, view, update, init, alertDanger, alertSuccess, alertInformation, alertWarning, subscriptions)

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
        content: String,
        alertType: Type,
        show: Bool,
        timeout: Maybe Int
    }

type Msg
    = Hide

type Type 
    = Information 
    | Warning
    | Success
    | Danger


init: String -> Type -> Maybe Int -> Model
init content alertType timeout =
    {
        content = content,
        alertType = alertType,
        show = True,
        timeout = timeout
    }

update: Msg -> Model -> Model
update msg model =
    { model | show = False }

view: Model -> Html Msg
view model =
        div[ 
            css[
                padding (px 20),
                case model.show of
                    True ->
                        Css.batch[
                            opacity (num 1.0),
                            cursor pointer,
                            hover[
                                opacity (num 0.9),
                                transform (translateY (px -2.5))
                            ]
                        ]
                    False ->
                        opacity (num 0.0)
                ,
                case model.alertType of
                    Warning ->
                        backgroundColor (rgb 255 243 205)
                    Information ->
                        backgroundColor (rgb 204 229 255)
                    Success ->
                        backgroundColor (rgb 212 237 218)
                    Danger -> 
                        backgroundColor (rgb 248 215 218)
                ,
                marginBottom (px 15),
                width (pct 40),
                margin auto,
                color (rgb 0 0 0),
                marginTop (px 5),
                transition
                [ 
                    Css.Transitions.transform3 100 0 easeInOut,
                    Css.Transitions.opacity3 300 0 linear
                ]
            ]
        ]
        [
            span [
                css[
                    marginLeft (px 15),
                    fontWeight bold,
                    float right,
                    lineHeight (px 20),
                    cursor pointer,
                    hover[
                        opacity (num 0.5)
                        --transform (translateY (px -1.5))
                    ],
                    transition
                    [ 
                        Css.Transitions.transform3 100 0 easeInOut
                    ]
                    --transition (num 0.3)
                ],
                onClick Hide
            ]
            [   
                close (Color.rgb255 0 0 0) 20 |> Svg.Styled.fromUnstyled
            ],
            text model.content
        ]

subscriptions: Model -> Sub Msg
subscriptions model =
  --every 5 seconds, move carousel in certain direction
  case model.timeout of
    Just int ->
        if model.show == True then
            Time.every (toFloat int * 1000) (\_ ->
            Hide
            )
        else
            Sub.none
    Nothing ->
      Sub.none

alertWarning: Type
alertWarning =
    Warning

alertInformation: Type
alertInformation =
    Information

alertSuccess: Type
alertSuccess =
    Success

alertDanger: Type
alertDanger =
    Danger