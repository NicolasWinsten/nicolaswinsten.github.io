port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Dom
import Task
import Element.Background as Background
import Element exposing (..)
import Element.Font as Font
import Json.Decode as Decode exposing (Value, Decoder)
import Json.Decode as Decode
import Element.Border as Border
import List exposing (repeat)
import Html.Attributes exposing (class)
import Element.Input exposing (button)
import Random
import Dict exposing (Dict)
import Html.Events
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

type WindowId = Zimi | P2PGame | GraphViz | TuringMachine

type alias WindowState =
  { visible : Bool
  , offsetX : Int
  , offsetY : Int
  }

type alias Model =
  { currentReads : Loadable Shelf
  , recentlyRead : Loadable Shelf
  , mobile : Bool
  , windows : Dict String WindowState
  , windowWidth : Int
  , windowHeight : Int
  }

initialWindowState = { visible = True, offsetX = 0, offsetY = 0 }

initialModel = 
  { currentReads=Loading
  , recentlyRead=Loading
  , mobile=False
  , windows = Dict.fromList
      [ ("zimi", initialWindowState)
      , ("p2p", initialWindowState)
      , ("graph", initialWindowState)
      , ("turing", initialWindowState)
      ]
  , windowWidth = 0
  , windowHeight = 0
  }

type Msg
  = ReceivedShelf (Result Decode.Error Shelf)
  | ClickExit String
  | SetOffset String (Int, Int)
  | WindowResized Int Int

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

    ClickExit windowId ->
      let updatedWindows = Dict.update windowId (Maybe.map (\w -> {w | visible = False})) model.windows
          randomOffset = Random.pair (Random.int -100 100) (Random.int -100 100)
      in ({model | windows = updatedWindows}, Random.generate (SetOffset windowId) randomOffset)

    SetOffset windowId (x, y) ->
      let updatedWindows = Dict.update windowId (Maybe.map (\w -> {w | visible = True, offsetX = x, offsetY = y})) model.windows
      in ({model | windows = updatedWindows}, Cmd.none)

    WindowResized width height ->
      ({model | windowWidth = width, windowHeight = height}, Cmd.none)

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

popup : String -> WindowState -> String -> String -> Element Msg -> Element Msg
popup key windowState title link content =
  if not windowState.visible then none
  else el [moveRight (toFloat windowState.offsetX), moveDown (toFloat windowState.offsetY)]
  (winXPWindow key title link content)



doodads model = wrappedRow [Font.center, spacing 10]
    [ column [] [text "doodads", image [width fill] {src="images/walkingrobot.gif", description="walking robot"}]
    , popup "zimi" (Dict.get "zimi" model.windows |> Maybe.withDefault initialWindowState) "Zimi 字谜！" "https://zimi.nicolaswinsten.com" <| paragraph [Font.center] [text "zimi: a daily Chinese character puzzle"]
    , popup "p2p" (Dict.get "p2p" model.windows |> Maybe.withDefault initialWindowState) "P2P Wikipedia Race!"  "https://nicolaswinsten.com/racer" <| paragraph [Font.center]
    [ image [] {src="images/wikilogo.gif", description=""}, text "a peer-to-peer Wikipedia game to play with your friends"]
    , popup "graph" (Dict.get "graph" model.windows |> Maybe.withDefault initialWindowState) "3D wikipedia graph" "https://nicolaswinsten.com/wikiweb" <| paragraph [Font.center]
    [ text "a 3D traversible graph visualization for wikipedia"]
    , popup "turing" (Dict.get "turing" model.windows |> Maybe.withDefault initialWindowState) "Turing Machine Simulator" "https://nicolaswinsten.com/turing" <| paragraph [] [text "a turing machine simulator built in Scala.js"]
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

winXPTitleBar : String -> msg -> Element msg
winXPTitleBar titleText onExit = row
  [ width fill
  , Background.color (rgb255 0 84 227)
  , Font.color (rgb255 255 255 255)
  , Font.bold
  , padding 2
  , spacing 5
  , height (px 24)
  ]
  [ image [width (px 20)] {src="images/Alert.png", description="alert icon"}
  , el [Font.size 14, Font.family [Font.typeface "MS Sans Serif", Font.sansSerif], paddingXY 4 2] (text titleText)
  , el [width fill] none
  , el [pointer, htmlAttribute <| class "hover-brightness depress", width (px 15), htmlAttribute (Html.Events.onClick onExit)]
      (image [width (px 15)] {src="images/Exit.png", description="exit icon"})
  ]

winXPButton : String -> Element msg
winXPButton label = el
  [ Background.color (rgb255 192 192 192)
  , Border.width 2
  , Border.color (rgb255 128 128 128)
  , padding 2
  , Font.family [Font.typeface "MS Sans Serif", Font.sansSerif]
  -- , Font.size 11
  -- , width (px 75)
  -- , height (px 23)
  , centerX
  , centerY
  , htmlAttribute <| class "depress"
  ]
  (el [pointer, padding 3, width fill, centerX, centerY, htmlAttribute <| class "hover-dotted"] (text label))


winXPWindow : String -> String -> String -> Element Msg -> Element Msg
winXPWindow windowId title link content = 
  el
    [ width (px 300)
    , Border.widthEach {top=2, bottom=3, left=2, right=3}
    , Border.color (rgb255 0 84 227)
    , Border.shadow {
      offset=(5,5), size=2, color=(rgba255 0 0 0 0.5), blur=5
    }
    ]
    (column
      [ width fill
      , Background.color (rgb255 192 192 192)
      ]
      [ winXPTitleBar title (ClickExit windowId)
      , column
        [ width fill
        , Background.color (rgb255 192 192 192)
        , padding 20
        , spacing 15
        ]
        [ el [ Font.family [Font.typeface "MS Sans Serif", Font.sansSerif], centerX] content
        , newTabLink [centerX] {url = link, label = winXPButton "Check it out!"}
        ]
      ]
    )


body model = column
  [ behindContent background
  , width fill
  , height fill
  , padding 30
  , spacing 50
  ]
  [ el [centerX] interests
  , if model.mobile then text "MOBILE" else none
  , el [centerX] (doodads model)
  , wrappedRow [centerX, spacing 30]
    [ goodreadsLink
    -- , el [width fill] (viewShelf model "what i'm currently reading" model.currentReads)
    ]
  -- hide recently read list because it reveals too much about my reading habits
  -- , el [centerX] (viewShelf model "books i read recently" model.recentlyRead)
  , el
    (if model.mobile then [width fill] else [alignBottom, alignLeft, scale 0.8])
    (email model)
  ]

view : Model -> Element Msg
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
  in Sub.batch 
    [ gotShelf
    , Browser.Events.onResize WindowResized
    ]

initialCmd : Cmd Msg
initialCmd = Cmd.batch
  [ fetchGoodReadsShelf {shelf="read", numBooks=5}
  , fetchGoodReadsShelf {shelf="currently-reading", numBooks=5}
  , Task.perform (\vp -> WindowResized (round vp.viewport.width) (round vp.viewport.height)) Browser.Dom.getViewport
  , Random.generate (SetOffset "zimi") (Random.pair (Random.int -50 50) (Random.int -50 50))
  , Random.generate (SetOffset "p2p") (Random.pair (Random.int -50 50) (Random.int -50 50))
  , Random.generate (SetOffset "graph") (Random.pair (Random.int -50 50) (Random.int -50 50))
  , Random.generate (SetOffset "turing") (Random.pair (Random.int -50 50) (Random.int -50 50))
  ]

main : Program {mobile : Bool} Model Msg
main = Browser.element
  { init = \{mobile} -> ({initialModel | mobile=mobile}, initialCmd)
  , view = view >> layout []
  , update = update
  , subscriptions = subscriptions
  }


-- test = \_ -> (testModel, Cmd.none)

-- testModel : Model
-- testModel =
--   { currentReads=Loaded (shelfOf 1)
--   , recentlyRead=Loaded (shelfOf 5)
--   , mobile=False
--   }

-- testBook =
--   { author = "James, C.L.R."
--   , cover = "https://i.gr-assets.com/images/S/compressed.photo.goodreads.com/books/1684793790l/125078769.jpg"
--   , rating = 5
--   , title = "Toussaint Louverture: The Story of the Only Successful Slave Revolt in History"
--   , url = "https://www.goodreads.com/book/show/125078769-toussaint-louverture"
--   }

-- emptyShelf = Shelf "" []

-- shelfOf num = Shelf "" (repeat num testBook)