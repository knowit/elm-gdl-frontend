module Main exposing (..)

import Books exposing (..)
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Exts.List exposing (chunk)
import Html exposing (Html, program)
import Html.Attributes.Extra exposing (innerHtml)
import Html.Events as HtmlEvents
import Http
import Keyboard
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias ElementMsg variation =
    Element Styles variation Msg


type Styles
    = NoStyle
    | Heading1
    | GridStyle
    | CellStyle
    | BookStyle
    | ImageStyle
    | BookTitleStyle
    | LevelStyle
    | BookReaderTitleStyle


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style GridStyle []
        , Style.style CellStyle
            [ Style.hover [ Shadow.deep ]
            , Color.background Color.white
            ]
        , Style.style Heading1
            [ Font.size 40 ]
        , Style.style BookStyle
            [ Font.size 20 ]
        , Style.style BookTitleStyle
            [ Font.typeface [ Font.sansSerif ] ]
        , Style.style ImageStyle
            [ Border.rounded 10 ]
        , Style.style LevelStyle
            [ Shadow.box { offset = ( 0.0, 0.0 ), size = 2.0, blur = 0.12, color = Color.rgba 0 0 0 0.12 }
            , Border.rounded 4
            , Color.background <| Color.rgb 102 102 102
            , Color.text Color.white
            ]
        , Style.style BookReaderTitleStyle
            [ Font.size 50 ]
        ]


type alias Model =
    { books : List Book
    , page : Page
    , chaptersWithContent : List ChapterWithContent

    -- TODO Improve this
    , pageNumber : Int
    }


type Page
    = BookOverview
    | ReadBook Book


initialModel : Model
initialModel =
    { books = []
    , page = BookOverview
    , chaptersWithContent = []
    , pageNumber = 1
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Http.send BooksResult getBooks )


type Msg
    = BooksResult (Result Http.Error (List Book))
    | ChapterResult (Result Http.Error ChapterWithContent)
    | ChooseBook Book
    | NextPage
    | KeyPress Keyboard.KeyCode


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
            { model | pageNumber = model.pageNumber + 1 } ! []

        KeyPress code ->
            case code of
                39 ->
                    { model | pageNumber = model.pageNumber + 1 } ! []

                27 ->
                    { model
                        | pageNumber = 1
                        , page = BookOverview
                        , chaptersWithContent = []
                    }
                        ! []

                _ ->
                    ( model, Cmd.none )


addChapterWithContent : List ChapterWithContent -> ChapterWithContent -> List ChapterWithContent
addChapterWithContent chapters chapter =
    chapter :: chapters


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        BookOverview ->
            Sub.none

        ReadBook book ->
            Keyboard.downs KeyPress


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        case model.page of
            BookOverview ->
                column NoStyle
                    []
                    [ el Heading1 [] (text "Books")
                    , viewBooks model.books
                    ]

            ReadBook book ->
                viewBookReader book model.pageNumber model.chaptersWithContent


viewBooks : List Book -> ElementMsg v
viewBooks books =
    let
        nChunks =
            6

        bookChunks =
            chunk nChunks books

        nRows =
            List.length bookChunks
    in
    grid GridStyle
        [ padding 50, spacing 10 ]
        { columns = List.repeat nChunks (px 200)
        , rows = List.repeat nRows (px 310)
        , cells =
            bookChunks
                |> List.indexedMap bookRow
                |> List.concat
        }


bookRow : Int -> List Book -> List (OnGrid (ElementMsg v))
bookRow rowIndex books =
    books
        |> List.indexedMap (bookCell rowIndex)


bookCell : Int -> Int -> Book -> OnGrid (ElementMsg v)
bookCell rowIndex columnIndex book =
    cell
        { start = ( columnIndex, rowIndex )
        , width = 1
        , height = 1
        , content =
            column CellStyle
                [ onClick (ChooseBook book), center, width (px 200), height (px 300) ]
                [ image ImageStyle
                    [ width (px 200), padding 5 ]
                    { src = book.coverPhotoUrl ++ "?focalX=50&focalY=50&ratio=0.81&width=200"
                    , caption = "Book cover for " ++ book.title
                    }
                , el BookTitleStyle [] (text book.title)
                , el LevelStyle [ paddingLeft 10, paddingRight 10 ] (text <| "Level " ++ book.readingLevel)
                ]
        }


findChapter : Int -> List ChapterWithContent -> Maybe ChapterWithContent
findChapter pageNumber chapters =
    chapters
        |> List.filter (\c -> c.seqNo == pageNumber)
        |> List.head


viewBookReader : Book -> Int -> List ChapterWithContent -> ElementMsg v
viewBookReader book pageNumber chapters =
    column NoStyle
        [ center ]
        [ el BookReaderTitleStyle [] (text book.title)
        , findChapter pageNumber chapters
            |> Maybe.map viewChapter
            |> Maybe.withDefault empty
        ]


viewChapter : ChapterWithContent -> ElementMsg v
viewChapter chapter =
    Html.div []
        [ Html.div [ innerHtml chapter.content ] []
        , Html.button [ HtmlEvents.onClick NextPage ] [ Html.text "->" ]
        ]
        |> html
