module Books
    exposing
        ( Book
        , Chapter
        , ChapterWithContent
        , Language
        , getBooks
        , getChapter
        , getLanguages
        )

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


gdlEnvironment =
    Test


type GdlEnvironment
    = Local
    | Test
    | Staging
    | Prod


baseUrl : String
baseUrl =
    case gdlEnvironment of
        Local ->
            "http://localhost:40001/book-api/v1"

        Test ->
            "https://api.test.digitallibrary.io/book-api/v1"

        Staging ->
            "https://api.staging.digitallibrary.io/book-api/v1"

        Prod ->
            "https://api.digitallibrary.io/book-api/v1"


type alias Book =
    { id : Int
    , title : String
    , description : String
    , language : Language
    , publisher : String
    , readingLevel : String
    , coverPhotoUrl : String
    , chapters : List Chapter
    }


type alias Language =
    { code : String
    , name : String
    }


type alias Chapter =
    { id : Int
    , seqNo : Int
    , url : String
    }


type alias ChapterWithContent =
    { id : Int
    , seqNo : Int
    , content : String
    }


getBooks : { code : String } -> Http.Request (List Book)
getBooks language =
    let
        url =
            baseUrl ++ "/books/" ++ language.code ++ "/?page-size=30"
    in
    Http.get url (field "results" (list book))


getChapter : Chapter -> Http.Request ChapterWithContent
getChapter chapter =
    Http.get chapter.url chapterWithContent


getLanguages : Http.Request (List Language)
getLanguages =
    Http.get (baseUrl ++ "/languages") (list language)


book : Decoder Book
book =
    decode Book
        |> required "id" int
        |> required "title" string
        |> required "description" string
        |> required "language" language
        |> requiredAt [ "publisher", "name" ] string
        |> required "readingLevel" string
        |> requiredAt [ "coverPhoto", "large" ] string
        |> required "chapters" (list chapter)


language : Decoder Language
language =
    decode Language
        |> required "code" string
        |> required "name" string


chapter : Decoder Chapter
chapter =
    decode Chapter
        |> required "id" int
        |> required "seqNo" int
        |> required "url" string


chapterWithContent : Decoder ChapterWithContent
chapterWithContent =
    decode ChapterWithContent
        |> required "id" int
        |> required "seqNo" int
        |> required "content" string
