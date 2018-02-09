module Books exposing (Book, Chapter, ChapterWithContent, getBooks, getChapter)

import Http
import Json.Decode as Decode exposing (..)


baseUrl : String
baseUrl =
    "https://api.test.digitallibrary.io/book-api/v1/books"


type alias Book =
    { title : String
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


getBooks : Http.Request (List Book)
getBooks =
    Http.get baseUrl (field "results" (list book))


getChapter : Chapter -> Http.Request ChapterWithContent
getChapter chapter =
    Http.get chapter.url chapterWithContent


book : Decoder Book
book =
    map7 Book
        (field "title" string)
        (field "description" string)
        (field "language" language)
        (at [ "publisher", "name" ] string)
        (field "readingLevel" string)
        (at [ "coverPhoto", "large" ] string)
        (field "chapters" (list chapter))


language : Decoder Language
language =
    map2 Language
        (field "code" string)
        (field "name" string)


chapter : Decoder Chapter
chapter =
    map3 Chapter
        (field "id" int)
        (field "seqNo" int)
        (field "url" string)


chapterWithContent : Decoder ChapterWithContent
chapterWithContent =
    map3 ChapterWithContent
        (field "id" int)
        (field "seqNo" int)
        (field "content" string)
