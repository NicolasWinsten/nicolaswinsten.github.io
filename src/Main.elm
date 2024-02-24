port module Main exposing (..)

import Browser
import Element.Background as Background
import Element exposing (..)
import Element.Font as Font
import Json.Decode as Decode exposing (Value, Decoder)
import Json.Decode as Decode
import Element.Border as Border

port log : String -> Cmd msg

type alias ISBN = String 

type Loadable a = Loading | Loaded a | Failed

viewLoadable : (a -> Element b) -> Loadable a -> Element b
viewLoadable f l = case l of
  Failed -> text "X"
  Loading -> image [] {src="images/hourglass.gif", description="spinning hourglass"}
  Loaded value -> f value

type alias Book = {isbn : ISBN, rating : Int}

type Shelf = Shelf String (List Book)

type alias Model =
  { currentReads : Loadable Shelf
  , recentlyRead : Loadable Shelf
  , mobile : Bool
  }

initialModel = { currentReads=Loading, recentlyRead=Loading, mobile=False }

type Msg
  = ReceivedShelf (Result Decode.Error Shelf)

{- goodreads withdrew API support...
so I gotta scrape the site manually

-}

{-| fetch the goodreads page listing some number of books from one of my shelves
-}
port fetchGoodReadsShelf : {shelf : String, numBooks : Int} -> Cmd msg

port receiveShelf : (Value -> msg) -> Sub msg


{-| discard the books that received a rating of 3 stars or less
(0 rating means the book is not rated so keep it)

discard all but the first 5
-}
takeBestReads : Shelf -> Shelf
takeBestReads (Shelf name books) =
  let bestReads = books
        |> List.filter (\{rating} -> rating == 0 || rating > 3)
        |> List.take 5
  in Shelf name bestReads

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    ReceivedShelf (Ok (Shelf "read" _ as shelf)) ->
      ({model | recentlyRead=Loaded (takeBestReads shelf)}, Cmd.none)

    ReceivedShelf (Ok (Shelf "currently-reading" _ as shelf)) ->
      ({model | currentReads=Loaded shelf}, Cmd.none)

    ReceivedShelf (Ok (Shelf _ _)) -> (model, Cmd.none)

    ReceivedShelf (Err err) -> (model, log (Decode.errorToString err))

hyperlink url label = link [Font.color (rgb255 13 110 253), Font.underline] {url=url, label=text label}

header {mobile} =
  let textWidth = if mobile then [width fill] else [] 
  in el
  [ width fill, Background.tiled "images/starbackground.gif", padding 20]
  (image textWidth {src="images/text.gif", description="nicolas winsten"})

interests = wrappedRow [Font.center]
  [ paragraph [centerY, centerX] [text "my main interests are functional programming, compilers, and reading non-fiction"]
  , image [] {src="images/lambdaspin.gif", description=""}
  ]

fontSize {mobile} =
  if mobile then Font.size 36
  else Font.size 24

doodads = textColumn [spacing 15]
  [ wrappedRow [Font.center]
    [ paragraph [] [text "doodads"]
    , image [width fill] {src="images/walkingrobot.gif", description="walking robot"}
    , paragraph [] [text "a ", hyperlink "turing" "turing machine simulator"]
    ]
  , paragraph []
    [ text "a peer-to-peer ", hyperlink "racer" "Wikipedia game"
    , image [] {src="images/wikilogo.gif", description=""}
    , text " to play with your friends"
    ]
  ]

email {mobile} =
  let fill_ = if mobile then [width fill] else []
  in wrappedRow fill_
  [ image [] {src="images/mail_spin.gif", description="email"}
  , image fill_ {src="images/email_text.gif", description="nicolasd dot winsten at gmail dot com"}
  ]

fade : List (Color, Int) -> Attribute msg
fade colors =
  let steps = List.concatMap (\(c, portion) -> List.repeat portion c) colors
  in inFront <| el [width fill, height fill, Background.gradient {angle=pi, steps=steps}] none

black = rgb255 0 0 0
cloudColor = rgb255 220 232 255
transparent = rgba255 0 0 0 0

clouds = el
  [ width fill, height (fillPortion 1)
  , Background.tiled "images/clouds.png", fade [(black, 1), (transparent, 1), (cloudColor, 1)]
  ]
  none

beach = el
  [ width fill, height (fillPortion 8)
  , Background.image "images/beachandwaves.gif"
  , fade [(cloudColor, 1), (transparent, 10)]
  ]
  none

background = column [width fill, height fill] [clouds, beach]

coverWidth = 150

viewBook : Book -> Element a
viewBook {isbn} = newTabLink [Border.width 5]
  { url="https://openlibrary.org/isbn/" ++ isbn
  , label=image [width (px coverWidth)]
    { src="https://covers.openlibrary.org/b/isbn/" ++ isbn ++ "-L.jpg"
    , description="book cover"
    }
  }

viewBookList : Shelf -> Element a
viewBookList (Shelf _ books) = wrappedRow [spacing 20] (List.map viewBook books)

viewShelf : String -> Loadable Shelf -> Element a
viewShelf topic shelf = case shelf of
  Loaded (Shelf _ []) -> none
  _ -> column [spacing 20] [text topic, viewLoadable viewBookList shelf]


goodreadsLink = newTabLink [Font.underline]
  { url="https://www.goodreads.com/user/show/98059963-nicolas"
  , label=image
      [inFront (el [Font.underline, centerX, centerY] (text "my goodreads"))]
      {src="images/book.gif", description="good reads link"}
  }

viewBookShelves : Model -> Element a
viewBookShelves {currentReads, recentlyRead} = column [spacing 40, centerX]
  [ wrappedRow [centerX, width fill, spaceEvenly, spacing 50]
    [ el [width fill] <| el [centerX] goodreadsLink
    , el [width fill] <| el [centerX] (viewShelf "what i'm currently reading" currentReads)
    ]
  , viewShelf "good books i read recently" recentlyRead
  ]

body model = column
  [ behindContent background
  , width fill
  , height fill
  , padding 30
  , spacing 50
  ]
  [ el [centerX] interests
  , el [centerX] doodads
  , viewBookShelves model
  , el
    (if model.mobile then [width fill] else [alignBottom, alignLeft, scale 0.8])
    (email model)
  ]

view : Model -> Element a
view model = column
  [ width fill
  , height fill
  , Font.family [Font.typeface "Comic Sans MS", Font.typeface "Comic Sans", Font.sansSerif]
  , fontSize model
  ]
  [ header model
  , body model
  ]

bookDecoder : Decoder Book
bookDecoder = Decode.map2 Book
  (Decode.field "isbn" Decode.string)
  (Decode.field "rating" Decode.int)

decodeShelf : Decoder Shelf
decodeShelf = Decode.map2 Shelf
  (Decode.field "name" Decode.string)
  (Decode.field "books" (Decode.list bookDecoder))

subscriptions : Model -> Sub Msg
subscriptions _ =
  let gotShelf = Sub.map ReceivedShelf (receiveShelf (Decode.decodeValue decodeShelf))
  in Sub.batch [gotShelf]

initalCmd : Cmd Msg
initalCmd = Cmd.batch
  [ fetchGoodReadsShelf {shelf="read", numBooks=20}
  , fetchGoodReadsShelf {shelf="currently-reading", numBooks=5}
  ]

main : Program {mobile : Bool} Model Msg
main = Browser.element
  { init = \{mobile} -> ({initialModel | mobile=mobile}, initalCmd)
  , view = view >> layout []
  , update = update
  , subscriptions = subscriptions
  }
  