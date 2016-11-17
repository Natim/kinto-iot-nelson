port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Time


-- Model


url =
    "https://nelson.alwaysdata.net/v1/buckets/default/collections/order/records"


type Msg
    = NewPosition String
    | NewPassword String
    | PositionUpdated (Result Http.Error String)
    | Tick Time.Time
    | EncodedAuth String


type alias Model =
    { position : Int
    , lastSentPosition : Int
    , password : String
    , base64 : String
    }



-- Init


init : ( Model, Cmd Msg )
init =
    ( Model 90 -1 "" "", Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPosition position ->
            case String.toInt position of
                Ok pos ->
                    { model | position = pos } ! []

                _ ->
                    model ! []

        Tick _ ->
            if model.lastSentPosition /= model.position then
                { model | lastSentPosition = model.position } ! [ postNewPosition model ]
            else
                model ! []

        PositionUpdated _ ->
            model ! []

        NewPassword password ->
            { model | password = password } ! [ b64encode ("token:" ++ password) ]

        EncodedAuth base64 ->
            { model | base64 = base64 } ! []


postNewPosition : Model -> Cmd Msg
postNewPosition model =
    Encode.object [ ( "data", Encode.object [ ( "position", (Encode.int model.position) ) ] ) ]
        |> Http.jsonBody
        |> (\body ->
                Http.send PositionUpdated <|
                    Http.request
                        { method = "POST"
                        , headers = [ Http.header "Authorization" ("Basic " ++ model.base64) ]
                        , url = url
                        , body = body
                        , expect = Http.expectStringResponse (\{ body } -> Ok body)
                        , timeout = Nothing
                        , withCredentials = False
                        }
           )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second * 2) Tick
        , encoded EncodedAuth
        ]



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            [ Html.text "Password: "
            , Html.input
                [ Html.Events.onInput NewPassword ]
                []
            ]
        , viewRange model
        ]


viewRange : Model -> Html.Html Msg
viewRange model =
    case model.password of
        "" ->
            Html.div [] []

        _ ->
            Html.div []
                [ Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "180"
                    , Html.Events.onInput NewPosition
                    ]
                    []
                , Html.label [] [ Html.text <| toString model.position ]
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ports


port b64encode : String -> Cmd msg


port encoded : (String -> msg) -> Sub msg
