module Main exposing (..)

import Components.Alert as Alert exposing (..)
import Components.Modal as Modal exposing (..)

import Browser
import Html exposing (..)
import Html.Styled exposing(toUnstyled)
import Html.Attributes exposing (src, style, class)
import Html.Events exposing (..)
import Bootstrap.Alert exposing (h1, simpleWarning)


---- MODEL ----


type alias Model =
    {
        --situational component - better to solve here or inside the component? (Maybe keyword)
        alert: Maybe Alert.Model,
        alertList: List (Maybe Alert.Model),
        modal: Maybe Modal.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { alert = Nothing , alertList = [Nothing], modal = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateAlert Alert.Msg
    | ChangeType Int
    | UpdateModal Modal.Msg
    | ShowModal
    | Add (Maybe Alert.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        UpdateAlert mesg ->
           case model.alert of
                Just alert -> 
                    ({ model | alert = Just(Alert.update mesg alert) }, Cmd.none) 
                Nothing ->
                    (model, Cmd.none)
        UpdateModal mesg ->
           case model.modal of
                Just modal -> 
                    ({ model | modal = Just(Modal.update mesg modal) }, Cmd.none) 
                Nothing ->
                    (model, Cmd.none)
        ChangeType int ->
            case int of
                1 ->
                    ({model | alert = Just(Alert.init "Hello world" Alert.alertInformation (Just 5)), alertList = model.alert :: model.alertList }, Cmd.none)
                2 ->
                    ({model | alert = Just(Alert.init "Hello world" Alert.alertSuccess Nothing)}, Cmd.none)
                3 ->
                    ({model | alert = Just(Alert.init "Hello world" Alert.alertWarning (Just 5))}, Cmd.none)
                4 ->
                    ({model | alert = Just(Alert.init "Hello world" Alert.alertDanger Nothing)}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Add alert ->
            ({model | alertList = alert :: model.alertList}, Cmd.none)
        ShowModal ->
            ({model | modal = Just(Modal.init "Hello world" Modal.modalWarning)}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , Html.h1 [] [ text "Your Elm App is working!" ]
        , div[] (List.map viewButton [1,2,3,4])
        , case model.alert of
            Just alert ->
                Alert.view alert |> toUnstyled |> Html.map UpdateAlert
            Nothing ->
                text ""
        --, div[] (List.map viewAlert model.alertList)
        , case model.modal of
            Just modal -> 
                Modal.view modal |> toUnstyled |> Html.map UpdateModal
            Nothing ->
                text ""
        , simpleWarning [][
            Bootstrap.Alert.h1 [] [ text "Hello world" ],
            p [] [ text "Content" ]
        ]
        , button [onClick ShowModal][ text "Show modal window" ]
        ]

viewAlert: Maybe Alert.Model -> Html Msg
viewAlert alert =
    case alert of
        Just alertJ ->
            Alert.view alertJ |> toUnstyled |> Html.map UpdateAlert
        Nothing ->
            text ""

viewButton: Int -> Html Msg
viewButton num =
        button[
            style "margin" "5px",
            onClick (ChangeType num)][ text (String.fromInt num)]


---- PROGRAM ----

subscriptions: Model -> Sub Msg
subscriptions model = 
    case model.alert of
        Just alert ->
            Alert.subscriptions alert |> Sub.map UpdateAlert
        Nothing ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
