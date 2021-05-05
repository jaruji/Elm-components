module Main exposing (..)

import Components.Alert as Alert exposing (..)

import Browser
import Html exposing (..)
import Html.Styled exposing(toUnstyled)
import Html.Attributes exposing (src, style)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    {
        --situational component - better to solve here or inside the component?
        -- alert: Maybe Alert.Model,
        alertList: List (Maybe Alert.Model)
    }


init : ( Model, Cmd Msg )
init =
    ( { {-alert = Nothing ,-} alertList = [Nothing] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateAlert Alert.Msg
    | ChangeType Int
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
        ChangeType int ->
            case int of
                1 ->
                    ({model | alertList = Just(Alert.init "Hello world" Alert.alertInformation (Just 5)) :: model.alertList }, Cmd.none)
                2 ->
                    ({model | alertList = Just(Alert.init "Hello world" Alert.alertSuccess Nothing) :: model.alertList }, Cmd.none)
                3 ->
                    ({model | alertList = Just(Alert.init "Hello world" Alert.alertWarning (Just 5)) :: model.alertList }, Cmd.none)
                4 ->
                    ({model | alertList = Just(Alert.init "Hello world" Alert.alertDanger Nothing) :: model.alertList }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Add alert ->
            ({model | alertList = alert :: model.alertList}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div[] (List.map viewButton [1,2,3,4])
        -- , case model.alert of
        --     Just alert ->
        --         Alert.view alert |> toUnstyled |> Html.map UpdateAlert
        --     Nothing ->
        --         text ""
        , div[] (List.map viewAlert model.alertList)
            
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
   Sub.batch (List.map alertSubscription model.alertList)

alertSubscription: Maybe Alert.Model -> Sub Msg
alertSubscription alert =
    case alert of
        Just alertJ ->
            Alert.subscriptions alertJ |> Sub.map UpdateAlert
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
