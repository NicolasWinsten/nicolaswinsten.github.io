port module Main exposing (..)

import Browser
import Element.Background as Background
import Element exposing (..)
import Element.Font as Font
import Json.Decode as Decode exposing (Value, Decoder)
import Json.Decode as Decode
import Element.Border as Border
import List exposing (repeat)

port log : String -> Cmd msg

type alias ISBN = String 

type Loadable a = Loading | Loaded a | Failed

viewLoadable : (a -> Element b) -> Loadable a -> Element b
viewLoadable f l = case l of
  Failed -> text "X"
  Loading -> image [centerX] {src="images/hourglass.gif", description="spinning hourglass"}
  Loaded value -> f value

type alias Book = {rating : Int, title : String, author : String, url : String, cover : String}

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

takeBooks : Int -> Shelf -> Shelf
takeBooks num (Shelf name books) = Shelf name (List.take num books)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    ReceivedShelf (Ok (Shelf "read" _ as shelf)) ->
      ({model | recentlyRead=Loaded (takeBooks 5 shelf)}, Cmd.none)

    ReceivedShelf (Ok (Shelf "currently-reading" _ as shelf)) ->
      ({model | currentReads=Loaded (takeBooks 5 shelf)}, Cmd.none)

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

doodads = textColumn [spacing 30]
  [ wrappedRow [Font.center]
    [ paragraph [] [text "doodads"]
    , image [width fill] {src="images/walkingrobot.gif", description="walking robot"}
    , paragraph [] [text "a ", hyperlink "turing" "turing machine simulator"]
    ]
  , paragraph [Font.center]
    [ text "a peer-to-peer ", hyperlink "racer" "Wikipedia game"
    , image [] {src="images/wikilogo.gif", description=""}
    , text " to play with your friends"
    ]
  , paragraph [Font.center]
    [ text "a ", hyperlink "wikiweb" "3D interactive graph visualization"
    , text " for wikipedia categories (meant for desktop and mouse)"
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

gold = rgb255 255 215 0

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


viewStarRating : Int -> Element msg
viewStarRating rating = row
  [ Background.color (rgba 0 0 0 0.5)
  , Border.rounded 5
  ]
  (repeat rating (image [] {src="images/star.gif", description=""}))

coverWidth {mobile} = if mobile then 300 else 150

viewBook : Model -> Book -> Element a
viewBook model {title, url, cover, rating} =
  let borderColor = if rating > 3 then gold else black
      stars = if rating > 0 then el [alignBottom, centerX] (viewStarRating rating) else none
  in newTabLink [Border.width 5, Border.color borderColor, inFront stars]
  { url=url
  , label=image [width <| px <| coverWidth model]
    { src=cover
    , description=title
    }
  }
      

viewBookList : Model -> Shelf -> Element a
viewBookList model (Shelf _ books) = wrappedRow [spacing 20] (List.map (viewBook model) books)

viewShelf : Model -> String -> Loadable Shelf -> Element a
viewShelf model topic shelf = case shelf of
  Loaded (Shelf _ []) -> none
  _ -> column [spacing 20] [text topic, viewLoadable (viewBookList model) shelf]


goodreadsLink = newTabLink [Font.underline]
  { url="https://www.goodreads.com/user/show/98059963-nicolas"
  , label=image
      [inFront (el [Font.underline, centerX, centerY] (text "my goodreads"))]
      {src="images/book.gif", description="good reads link"}
  }



body model = column
  [ behindContent background
  , width fill
  , height fill
  , padding 30
  , spacing 50
  ]
  [ el [centerX] interests
  , el [centerX] doodads
  , wrappedRow [centerX, spacing 30]
    [ goodreadsLink
    , el [width fill] (viewShelf model "what i'm currently reading" model.currentReads)
    ]
  -- hide recently read list because it reveals too much about my reading habits
  -- , el [centerX] (viewShelf model "books i read recently" model.recentlyRead)
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
bookDecoder = Decode.map5 Book
  (Decode.field "rating" Decode.int)
  (Decode.field "title" Decode.string)
  (Decode.field "author" Decode.string)
  (Decode.field "url" Decode.string)
  (Decode.field "cover" Decode.string)

decodeShelf : Decoder Shelf
decodeShelf = Decode.map2 Shelf
  (Decode.field "name" Decode.string)
  (Decode.field "books" (Decode.list bookDecoder))

subscriptions : Model -> Sub Msg
subscriptions _ =
  let gotShelf = Sub.map ReceivedShelf (receiveShelf (Decode.decodeValue decodeShelf))
  in Sub.batch [gotShelf]

initialCmd : Cmd Msg
initialCmd = Cmd.batch
  [ fetchGoodReadsShelf {shelf="read", numBooks=5}
  , fetchGoodReadsShelf {shelf="currently-reading", numBooks=5}
  ]

main : Program {mobile : Bool} Model Msg
main = Browser.element
  { init = \{mobile} -> ({initialModel | mobile=mobile}, initialCmd)
  , view = view >> layout []
  , update = update
  , subscriptions = subscriptions
  }


test = \_ -> (testModel, Cmd.none)

testModel : Model
testModel =
  { currentReads=Loaded (shelfOf 1)
  , recentlyRead=Loaded (shelfOf 5)
  , mobile=False
  }

testBook =
  { author = "James, C.L.R."
  , cover = "https://i.gr-assets.com/images/S/compressed.photo.goodreads.com/books/1684793790l/125078769.jpg"
  , rating = 5
  , title = "Toussaint Louverture: The Story of the Only Successful Slave Revolt in History"
  , url = "https://www.goodreads.com/book/show/125078769-toussaint-louverture"
  }

emptyShelf = Shelf "" []

shelfOf num = Shelf "" (repeat num testBook)