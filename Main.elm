
module Main where

-- import Window
import Time
import Graphics.Element exposing (show)
import Keyboard

{-- Part 1: Model the user input ----------------------------------------------
What information do you need to represent all relevant user input?
Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.
------------------------------------------------------------------------------}



type alias Input =
          {  space : Bool
            , paddle1 : Int
            , paddle2 : Int
            , delta   : Time.Time
          }


delta : Signal Time.Time
delta = Signal.map Time.inSeconds (Time.fps 35)



{-- Part 2: Model the game ----------------------------------------------------
What information do you need to represent the entire game?
Tasks: Redefine `Game` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.
For example, if you want to represent many objects that just have a position,
your Game might just be a list of coordinates and your default game might
be an empty list (no objects at the start):
    type Game = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }
------------------------------------------------------------------------------}

type alias Game = {
    state : State
  , ball : Ball
  , player1 : Player
  , player2 : Player
}

player : Float -> Player
player x =
  {x = x, y = 0, vx = 0, vy = 0, score = 0}

defaultGame : Game
defaultGame =
    { state   = Pause
    , ball    = { x=0, y=0, vx=200, vy=200 }
    , player1 = player (20-halfWidth)
    , player2 = player (halfWidth-20)
    }

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type alias Object a =
  {
  a |
    x: Float,
    y: Float,
    vx: Float,
    vy: Float
  }

type alias Ball = Object {}

type alias Player = Object {score : Int}

type State  = Play | Pause


{-- Part 3: Update the game ---------------------------------------------------
How does the game step from one state to another based on user input?
Task: redefine `stepGame` to use the UserInput and Game
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.
------------------------------------------------------------------------------}

stepGame : Input -> Game -> Game
stepGame {timeDelta,{}} gameState = gameState



{-- Part 4: Display the game --------------------------------------------------
How should the Game be displayed to the user?
Task: redefine `display` to use the Game you defined in part 2.
------------------------------------------------------------------------------}

display : (Int,Int) -> Game -> Graphics.Element.Element
display (w,h) gameState = show gameState



{-- That's all folks! ---------------------------------------------------------
The following code puts it all together and shows it on screen.
------------------------------------------------------------------------------}
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
    Keyboard.space
    (Signal.map .y Keyboard.wasd)
    (Signal.map .y Keyboard.arrows)
    delta

gameState = Signal.foldp stepGame defaultGame input
