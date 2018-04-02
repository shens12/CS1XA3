import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)
import Date exposing (..)
import Task


main =
    Html.program
        { view = view
        , init = init
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    Time


init : ( Model, Cmd Msg )
init =
    ( 0, now )



-- UPDATE


type Msg
    = SetTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update (SetTime time) _ =
    ( time, Cmd.none )

-- As shown in the elm-
-- update msg model =
--  case msg of
--    SetTime newTime ->
--      (newTime, Cmd.none)


now : Cmd Msg
now =
    Task.perform SetTime Time.now



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second SetTime



-- VIEW

view : Model -> Html Msg
view model =
    let
        secondNow =
            Date.second <| Date.fromTime <| model

        minuteNow =
            Date.minute <| Date.fromTime <| model

        hourNow =
            (Date.hour <| Date.fromTime <| model) % 12

        angleSecond =
            toFloat <| 6 * secondNow

        angleMinute =
            toFloat <| 6 * minuteNow

        angleHour =
          
            (+) (angleMinute / 12) <| (toFloat <| 30 * hourNow)

        handSecondX =
            toString (50 + 38 * sin (angleSecond * pi / 180))

        handSecondY =
            toString (50 - 38 * cos (angleSecond * pi / 180))

        handMinuteX =
            toString (50 + 32 * sin (angleMinute * pi / 180))

        handMinuteY =
            toString (50 - 32 * cos (angleMinute * pi / 180))

        handHourX =
            toString (50 + 25 * sin (angleHour * pi / 180))

        handHourY =
            toString (50 - 25 * cos (angleHour * pi / 180))
    in
        div []
            [ svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", fill "#2a2a2a" ] []
                , circle [ cx "50", cy "50", r "40", fill "#f6f6f6" ] []
                , line [ x1 "50", y1 "50", x2 handSecondX, y2 handSecondY, stroke "#2a2a2a", strokeWidth "0.5" ] []
                , line [ x1 "50", y1 "50", x2 handMinuteX, y2 handMinuteY, stroke "#2a2a2a", strokeWidth "1" ] []
                , line [ x1 "50", y1 "50", x2 handHourX, y2 handHourY, stroke "#2a2a2a", strokeWidth "2" ] []
                ]
            , div [] [ Html.text ("model(UNIX time): " ++ (toString model)) ]
            , div [] [ Html.text ("Date.fromTime model: " ++ (toString (Date.fromTime model))) ]
            ]