module Main exposing (..)

import Books exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Extra exposing (innerHtml)
import Html.Events exposing (onClick)
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { books : List Book
    , page : Page
    , chaptersWithContent : List ChapterWithContent
    }


type Page
    = BookOverview
    | ReadBook Book


initialModel : Model
initialModel =
    { books = []
    , page = BookOverview
    , chaptersWithContent = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Http.send BooksResult getBooks )


type Msg
    = BooksResult (Result Http.Error (List Book))
    | ChapterResult (Result Http.Error ChapterWithContent)
    | ChooseBook Book
    | NextPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BooksResult (Ok books) ->
            ( { model | books = books }, Cmd.none )

        BooksResult (Err _) ->
            ( model, Cmd.none )

        ChooseBook book ->
            let
                cmd =
                    book.chapters
                        |> List.map getChapter
                        |> List.map (Http.send ChapterResult)
                        |> Cmd.batch
            in
            ( { model | page = ReadBook book }, cmd )

        ChapterResult (Ok chapter) ->
            ( { model | chaptersWithContent = addChapterWithContent model.chaptersWithContent chapter }, Cmd.none )

        ChapterResult (Err _) ->
            ( model, Cmd.none )

        NextPage ->
            { model | chaptersWithContent = List.tail model.chaptersWithContent |> Maybe.withDefault [] } ! []


addChapterWithContent : List ChapterWithContent -> ChapterWithContent -> List ChapterWithContent
addChapterWithContent chapters chapter =
    chapter :: chapters


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style [ ( "text-align", "center" ) ] ]
        [ h1 [] [ text "Global Digital Library" ]
        , case model.page of
            BookOverview ->
                div []
                    [ h2 [] [ text "Books" ]
                    , model.books
                        |> List.map viewBook
                        |> div []
                    ]

            ReadBook book ->
                model.chaptersWithContent
                    |> List.sortBy .seqNo
                    |> readBook book
        ]


viewBook : Book -> Html Msg
viewBook book =
    p [ onClick (ChooseBook book) ]
        [ div []
            [ img [ book.coverPhotoUrl ++ "?width=250" |> src ] []
            , div [] [ text book.title ]
            , div [] [ "Level " ++ book.readingLevel |> text ]
            ]
        ]


readBook : Book -> List ChapterWithContent -> Html Msg
readBook book chapters =
    div []
        [ h2 [] [ text book.title ]
        , p [] [ text book.description ]
        , case chapters of
            first :: second :: rest ->
                div []
                    [ viewChapter first
                    , button [ onClick NextPage ] [ text "->" ]
                    ]

            last :: [] ->
                viewChapter last

            _ ->
                text ""
        ]


viewChapter : ChapterWithContent -> Html Msg
viewChapter chapter =
    div []
        [ h3 [] [ "Chapter " ++ toString chapter.seqNo |> text ]
        , p [ innerHtml chapter.content ] []
        ]
