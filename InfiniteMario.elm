import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Text

-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  , stuff : Float
  , dead : Bool
  , score : Int
  , speed : Float
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  , stuff = 350
  , dead = False
  , score = 0
  , speed = 3
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) mario =
  mario
    |> gravity dt
    |> jump keys
    |> physics dt
    |> obstacle dt
    |> death dt


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 && mario.vy == 0 then
      { mario | vy = 6.0 }

  else
      mario


gravity : Float -> Model -> Model
gravity dt mario =
  { mario |
      vy = if mario.y > 0 then mario.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y = max 0 (mario.y + dt * mario.vy)
  }

obstacle : Float -> Model -> Model
obstacle dt mario =
  let
    newRound = mario.stuff < -350
  in
  { mario |
      stuff = 
      if mario.dead then mario.stuff
      else 
        if newRound then 350 
        else mario.stuff - mario.speed,
      speed = if newRound then mario.speed + 1 else mario.speed   
  }

death : Float -> Model -> Model
death dt mario =
  let
    vDistance = mario.y - 25
    hDistance = abs (mario.x - mario.stuff)
    maybeDead = mario.dead || (hDistance < 5 && vDistance < 5)
  in
  { mario |
      dead = maybeDead,
      score = if maybeDead then mario.score else mario.score + 1
  }



walk : Keys -> Model -> Model
walk keys mario =
  { mario |
      vx = toFloat keys.x,
      dir =
        --if keys.x < 0 then
        --    Left

        --else if keys.x > 0 then
        --    Right

        --else
            mario.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if mario.y > 0 then
          "jump"
      else if mario.dead then
          "jump"
      else
          "walk"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "/imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , toForm (show mario.score)
      --, toForm (Text.fromString (toString mario.score))
      --, toForm (show (toString mario))    
      , rect 20 25
          |> filled (rgb 0 0 0)
          |> move (mario.stuff, 60 - h/2)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , marioImage
          |> toForm
          |> move position
          |> rotate (if mario.dead then 90 else 0)
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
    
    
    
