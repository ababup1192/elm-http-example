module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import Task exposing (Task)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { resultChunk : List String
    , currentPage : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { resultChunk = [], currentPage = 1 }, Cmd.none )


type alias Response =
    { result : String, isNext : Bool }



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = KickTestServer
      -- | GotServerResponse (Result Http.Error Response)
    | GotServerResponse (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ resultChunk, currentPage } as model) =
    case msg of
        KickTestServer ->
            -- ( model, Task.attempt GotServerResponse <| chunkResultTask 1 [] )
            let
                getResultTask =
                    getTestServerResponseWithPageTask >> Task.map .result
            in
            ( model
            , Task.attempt GotServerResponse <| (List.range 1 3 |> List.map getResultTask |> Task.sequence)
            )

        -- , getTestServerResponseWithPage currentPage
        {-
           GotServerResponse res ->
                 case res of
                     Ok { result, isNext } ->
                         if isNext then
                             ( { model | resultChunk = result :: resultChunk, currentPage = currentPage + 1 }, getTestServerResponseWithPage currentPage )

                         else
                             ( { model | currentPage = 1 }, Cmd.none )

                     Err err ->
                         ( { model | resultChunk = [ "Error: " ++ httpErrorToString err ] }, Cmd.none )
        -}
        GotServerResponse res ->
            case res of
                Ok rc ->
                    ( { model | resultChunk = rc }, Cmd.none )

                Err err ->
                    ( { model | resultChunk = [ "Error: " ++ httpErrorToString err ] }, Cmd.none )



{-
   getTestServerResponseWithPage : Int -> Cmd Msg
   getTestServerResponseWithPage pageNum =
       let
           expect =
               Http.expectJson GotServerResponse responseDecoder
       in
       Http.get { url = "/test?page=" ++ String.fromInt pageNum, expect = expect }
-}


getTestServerResponseWithPageTask : Int -> Task Http.Error Response
getTestServerResponseWithPageTask pageNum =
    Http.task
        { method = "GET"
        , headers = []
        , url = "/test?page=" ++ String.fromInt pageNum
        , body = Http.emptyBody
        , resolver = jsonResolver responseDecoder
        , timeout = Nothing
        }


jsonResolver : Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map2 Response
        (Decode.field "result" Decode.string)
        (Decode.field "isNext" Decode.bool)


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl _ ->
            "BadUrl"

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s


chunkResultTask : Int -> List String -> Task Http.Error (List String)
chunkResultTask currentPage resultChunk =
    getTestServerResponseWithPageTask currentPage
        |> Task.andThen
            (\{ result, isNext } ->
                if isNext then
                    chunkResultTask (currentPage + 1) (result :: resultChunk)

                else
                    Task.succeed (result :: resultChunk)
            )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view { resultChunk } =
    div [ class "container" ]
        [ button [ onClick <| KickTestServer ] [ text "getPages" ]
        , p [] [ text <| String.join ", " (resultChunk |> List.reverse) ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm Http"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
