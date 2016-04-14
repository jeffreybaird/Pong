module Pong where

import Time
import Keyboard
import Color exposing (rgb)
import Text
import Window
import Graphics.Element exposing (container, leftAligned, middle, spacer)
import Graphics.Collage exposing (rect, oval, collage, move, filled, toForm, Shape, Form)

--- Representation of the Game
type alias Input =
  {
    space : Bool
  , paddle1 : Int
  , paddle2 : Int
  , delta : Time.Time
  }

-- Track the passage of time
delta : Signal Time.Time
delta = Signal.map Time.inSeconds (Time.fps 35)

-- This updates the game state based on fps instead of user input.
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
    -- Make sure to import Keyboard
      Keyboard.space
      (Signal.map .y Keyboard.wasd)
      (Signal.map .y Keyboard.arrows)
      delta

-- Game board!
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

-- Define the board and paddles. We make a baseline object
-- and then share the structure between the ball and player

type alias Object a =
  { a |
      x  : Float
    , y  : Float
    , vx : Float
    , vy : Float
  }

type alias Ball =
  Object {}

type alias Player =
  Object {score : Int}


-- Game State

type State = Play | Pause

-- setting up the game

type alias Game =
  {
    state   : State
  , ball    : Ball
  , player1 : Player
  , player2 : Player
  }


-- takes a starting position and returns a player

player : Float -> Player
player x =
  {x=x, y=0, vy=0, vx=0, score =0}

defaultGame : Game
defaultGame =
    {
      state   = Pause
    , ball    = {x=0, y=0, vy=200, vx=200}
    --- distance from the edge of the board
    , player1 = player (10 - halfWidth)
    , player2 = player (halfWidth - 10)
  }

-- Update the Game

-- Is one object close to another object on the board?
near : Float -> Float -> Float -> Bool
near object1 distance object2 =
  object2 >= object1 - distance && object2 <= object1 + distance

-- Is the ball within the paddle?
-- Hey Jeff, come back and play with these numbers

within : Ball -> Player -> Bool
within ball player =
  near player.x 8 ball.x && near player.y 20 ball.y

-- Change the direction of the ball after a collision
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else v

-- Step the position of an object based on its velocity and time
stepObj : Time.Time -> Object a -> Object a
stepObj t ({x,y,vx,vy} as obj) =
    { obj |
        x = x + vx * t,
        y = y + vy * t
    }

-- Move the ball

stepBall : Time.Time -> Ball -> Player -> Player -> Ball
stepBall time ({x,y,vx,vy} as ball) player1 player2 =
  if not (ball.x |> near 0 halfWidth)
    then {ball | x = 0, y = 0}
    else
      stepObj time
          { ball |
            vx =
              stepV vx (ball `within` player1) (ball `within` player2),
            vy =
              stepV vy (y < 7-halfHeight) (y > halfHeight-7)
        }

-- Step a player

stepPlyr : Time.Time -> Int -> Int -> Player -> Player
stepPlyr time direction points player =
  -- Velocity of paddle
  let player' = stepObj time { player | vy = toFloat direction * 300 }
      y'      = clamp (22-halfHeight) (halfHeight-22) player'.y
      score'  = player.score + points
  in
      { player' | y = y', score = score' }


-- Step the game
stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space,paddle1,paddle2,delta} = input
    {state,ball,player1,player2} = game

    score1 =
        if ball.x > halfWidth then 1 else 0
    score2 =
        if ball.x < -halfWidth then 1 else 0

    state' =
        if space then Play
        else if score1 /= score2 then Pause
        else state
    ball' =
        if state == Pause
            then ball
            else stepBall delta ball player1 player2

    player1' = stepPlyr delta paddle1 score1 player1
    player2' = stepPlyr delta paddle2 score2 player2
  in
      { game |
          state   = state',
          ball    = ball',
          player1 = player1',
          player2 = player2'
      }

gameState : Signal Game
gameState =
    Signal.foldp stepGame defaultGame input


--- View

-- helper values
pongGreen : Color.Color
pongGreen = rgb 60 100 60

textGreen : Color.Color
textGreen = rgb 160 200 160

txt : (Text.Text -> Text.Text) -> String -> Graphics.Element.Element
txt f = leftAligned << f << Text.monospace << Text.color textGreen << Text.fromString

msg : String
msg = "SPACE to start, WS and &uarr;&darr; to move"


-- shared function for rendering objects
displayObj : Object a -> Shape -> Form
displayObj obj shape =
    move (obj.x, obj.y) (filled Color.white shape)

-- display a game state
display : (Int,Int) -> Game -> Graphics.Element.Element
display (w,h) {state,ball,player1,player2} =
  let scores : Graphics.Element.Element
      scores =
          toString player1.score ++ "  " ++ toString player2.score
            |> txt (Text.height 50)
  in
      container w h middle <|
      collage gameWidth gameHeight
       [ filled pongGreen   (rect gameWidth gameHeight)
       , displayObj ball    (oval 15 15)
       , displayObj player1 (rect 10 40)
       , displayObj player2 (rect 10 40)
       , toForm scores
           |> move (0, gameHeight/2 - 40)
       , toForm (if state == Play then spacer 1 1 else txt identity msg)
           |> move (0, 40 - gameHeight/2)
       ]

main : Signal Graphics.Element.Element
main =
    Signal.map2 display Window.dimensions gameState
