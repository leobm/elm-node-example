port module Main exposing (..)

import Task exposing (Task, andThen, map, onError)
import Json.Encode as Json
import Json.Decode exposing (Decoder, decodeString, object2, list, int, string, (:=))
import String exposing (isEmpty, join, split)
import Html exposing (..)
import Html.App as Html
import Node.File as File
import Node.Console as Console
import Node.Http as Http
import Node.Url as Url
import Node.Process as Process
import Node.Path as Path
import Node.Stream as Stream
import Node.Encoding as Encoding
import List
import Dict


type alias Artist =
    { id : Int
    , name : String
    }


mimeTypes : Dict.Dict String String
mimeTypes =
    Dict.fromList
        [ ( ".txt", "text/plain" )
        , ( ".html", "text/html" )
        , ( ".jpeg", "image/jpeg" )
        , ( ".png", "image/png" )
        , ( ".svg", "image/svg+xml" )
        , ( ".js", "text/javascript" )
        , ( ".css", "text/css" )
        , ( ".json", "application/json" )
        ]


mimetype : Path.FilePath -> String
mimetype filename =
    let
        ext =
            Path.extname filename
    in
        Maybe.withDefault "text/plain" (Dict.get ext mimeTypes)


artistValue : Artist -> Json.Value
artistValue artist =
    Json.object
        [ ( "artistId", Json.int artist.id )
        , ( "name", Json.string artist.name )
        ]


artistsValue : List Artist -> Json.Value
artistsValue artists =
    artists
        |> List.map artistValue
        |> Json.list


encodeArtists : String
encodeArtists =
    Json.encode 4
        <| artistsValue
            [ { id = 1
              , name = "Radiohead"
              }
            , { id = 2
              , name = "Massive Attack"
              }
            ]


decodeArtists : String -> Result String (List Artist)
decodeArtists json =
    decodeString artistsDecoder json


artistsDecoder : Decoder (List Artist)
artistsDecoder =
    list
        <| object2 Artist
            ("id" := int)
            ("name" := string)


readArtists : Task String String
readArtists =
    File.read "data.json"
        `withError` "Could not find the given data file: data.json"


staticPath : String -> String
staticPath filename =
    Process.cwd ++ "/static/" ++ filename


readAndSendStatic : String -> Http.Response -> Task a ()
readAndSendStatic filename res =
    Stream.createReadStream (staticPath filename)
        `andThen` \readable ->
                    res
                        |> Http.setStatusCode 200
                        |> Http.setHeader "Content-Type" (mimetype filename)
                        |> Http.responseAsStream
                        |> Stream.pipe readable


redirectTo : String -> Http.Response -> Task a ()
redirectTo path res =
    res
        |> Http.setStatusCode 302
        |> Http.setHeader "Location" path
        |> Http.end


send : String -> String -> Http.Response -> Task a ()
send content contentType res =
    res
        |> Http.setHeader "Content-Type" contentType
        |> Http.sendResponse content


sendJson : String -> Http.Response -> Task a ()
sendJson json res =
    send json "application/json" res


sendHtml : String -> Http.Response -> Task a ()
sendHtml html res =
    send html "text/html" res


handle : Http.Request -> Http.Response -> Task a ()
handle req res =
    let
        url =
            req.url

        search =
            Maybe.withDefault "" url.search

        query =
            Url.getQuery url

        emptyUrl =
            Url.emptyUrl

        urlStr =
            Url.format
                { emptyUrl
                    | protocol = Just "http"
                    , hostname = Just "localhost"
                    , search = Just "?query=1234"
                    , port' = Just 100
                }

        pathList =
            List.filter (not << isEmpty) (split "/" (url.pathname ? ""))
    in
        case pathList of
            [] ->
                tryCatch (emitError res)
                    <| readAndSendStatic "index.html" res

            "static" :: file ->
                tryCatch (emitError res)
                    <| readAndSendStatic (join "/" file) res

            [ "send" ] ->
                case req.method of
                    "POST" ->
                        (Http.requestAsStream req
                            |> Stream.setEncoding Encoding.UTF8
                            |> Stream.onData
                        )
                            `andThen` \_ -> sendHtml "<h1>send json</h1>" res

                    _ ->
                        emitErrorAsJson res ""

            [ "load" ] ->
                tryCatch (emitErrorAsJson res)
                    <| readArtists
                    `andThen` (\json ->
                                case (decodeArtists json) of
                                    Ok val ->
                                        Task.succeed (toString val)

                                    Err err ->
                                        Task.fail err
                              )
                    `andThen` \str -> sendHtml str res

            "query" :: [] ->
                --Console.log (Url.parseQueryString search)
                --  `andThen` \_ ->
                Http.sendResponse (toString <| Dict.get "x" query) res

            [ "format" ] ->
                Console.log urlStr
                    `andThen` \_ -> Http.sendResponse ("Format Url:" ++ urlStr) res

            [ "hello" ] ->
                -- (Maybe.withDefault "" (Dict.get "x" url.query))
                Http.sendResponse "Hello World!" res

            _ ->
                Console.log
                    ("Pathname:"
                        ++ (Maybe.withDefault "" url.pathname)
                        ++ "Host:"
                        ++ (Maybe.withDefault "" url.host)
                        ++ "Port:"
                        ++ (toString (Maybe.withDefault -1 url.port'))
                        ++ "Protocol:"
                        ++ (Maybe.withDefault "" url.protocol)
                    )
                    `andThen` \_ -> sendJson encodeArtists res


emitError : Http.Response -> String -> Task x ()
emitError res message =
    res
        |> Http.setStatusCode 404
        |> Http.setHeader "Content-Type" "text/plain"
        |> Http.sendResponse message


emitErrorAsJson : Http.Response -> String -> Task x ()
emitErrorAsJson res message =
    let
        json =
            [ Json.object [ ( "error", Json.string (message) ) ] ]
                |> Json.list
                |> Json.encode 0
    in
        Http.setStatusCode 404 res
            |> sendJson json


withError : Task x a -> y -> Task y a
withError task error =
    Task.mapError (\_ -> error) task


tryCatch : (x -> Task y a) -> Task x a -> Task y a
tryCatch =
    flip Task.onError


(?) : Maybe a -> a -> a
(?) maybe default =
    Maybe.withDefault default maybe


type alias Model =
    { httpPort : Int }


init : ( Model, Cmd Msg )
init =
    ( { httpPort = 8081 }, Cmd.none )


type Msg
    = Started Bool
    | Serve
    | None


port dbg : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Started val ->
            ( model
            , Cmd.batch [ Task.perform (always None) (always (Serve)) (Http.serve model.httpPort handle) ]
            )

        Serve ->
            ( model, Task.perform (always None) (always None) (Console.log ("Serve at http://localhost:" ++ (toString model.httpPort))) )

        None ->
            ( model, Cmd.none )


main : Program Never
main =
    Html.program
        { init = init
        , view = always <| div [] []
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


port started : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    started Started
