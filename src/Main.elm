module Main exposing (..)

import Books exposing (..)
import Color exposing (..)
import Dict
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Exts.List exposing (chunk)
import Html exposing (Html, program)
import Http
import Navigation exposing (Location)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import UrlParser exposing (..)


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias WithLanguageCode =
    { code : String }


defaultLanguage : WithLanguageCode
defaultLanguage =
    { code = "eng" }


type alias ElementMsg variation =
    Element Styles variation Msg


type Styles
    = NoStyle
    | HeadingStyle
    | GridStyle
    | CellStyle
    | BookStyle
    | ImageStyle
    | BookTitleStyle
    | LevelStyle


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style GridStyle []
        , Style.style HeadingStyle
            [ Font.size 40
            ]
        , Style.style CellStyle
            [ Style.hover [ Shadow.deep ]
            , Color.background Color.white
            ]
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
        ]


type alias Model =
    { language : WithLanguageCode
    , books : Dict.Dict String (List Book)
    , page : Page
    }


type Page
    = BookOverview String
    | SelectedBook String Int
    | NotFound
    | ErrorPage String


parsePage : Location -> Page
parsePage location =
    let
        matchers =
            oneOf
                [ UrlParser.map (BookOverview defaultLanguage.code) top
                , UrlParser.map BookOverview string
                , UrlParser.map SelectedBook (string </> int)
                ]
    in
    parseHash matchers location
        |> Maybe.withDefault NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            parsePage location

        language =
            case page of
                BookOverview lang ->
                    WithLanguageCode lang

                SelectedBook lang _ ->
                    WithLanguageCode lang

                _ ->
                    defaultLanguage

        model =
            { language = language, page = page, books = Dict.empty }

        cmd =
            Http.send (BooksResult language) (getBooks language)
    in
    ( model, cmd )


type Msg
    = BooksResult WithLanguageCode (Result Http.Error (List Book))
    | LocationChange Location
    | SelectBook Book


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BooksResult language (Ok books) ->
            { model | books = Dict.insert language.code books model.books } ! []

        BooksResult _ (Err errorMessage) ->
            { model | page = ErrorPage (toString errorMessage) } ! []

        LocationChange location ->
            let
                page =
                    parsePage location

                cmd =
                    case page of
                        BookOverview language ->
                            if Dict.member language model.books then
                                Cmd.none
                            else
                                let
                                    languageWithCode =
                                        WithLanguageCode language
                                in
                                getBooks languageWithCode
                                    |> Http.send (BooksResult languageWithCode)

                        _ ->
                            Cmd.none
            in
            ( { model | page = page }, cmd )

        SelectBook book ->
            let
                hash =
                    "#/" ++ book.language.code ++ "/" ++ toString book.id
            in
            ( model, Navigation.newUrl hash )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        case model.page of
            BookOverview languageCode ->
                model.books
                    |> Dict.get languageCode
                    |> Maybe.withDefault []
                    |> viewBooks

            SelectedBook language bookId ->
                el NoStyle [] ("Book " ++ toString bookId ++ " in language " ++ language |> text)

            NotFound ->
                el NoStyle [] (text "Not found!")

            ErrorPage errorMessage ->
                el NoStyle [] (text <| "Error! " ++ errorMessage)


viewBooks : List Book -> ElementMsg v
viewBooks books =
    let
        nChunks =
            6

        bookChunks =
            books
                |> chunk nChunks

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
                [ onClick (SelectBook book), center, width (px 200), height (px 300) ]
                [ image ImageStyle
                    [ width (px 200), padding 5 ]
                    { src = book.coverPhotoUrl ++ "?focalX=50&focalY=50&ratio=0.81&width=200"
                    , caption = "Book cover for " ++ book.title
                    }
                , el BookTitleStyle [] (text book.title)
                , el LevelStyle [ paddingLeft 10, paddingRight 10 ] (text <| "Level " ++ book.readingLevel)
                ]
        }
