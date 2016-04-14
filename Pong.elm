module Pong where

import Time

type alias Input =
  {
    space : Bool
  , paddle1 : Int
  , paddle2 : Int
  , delta : Time.Time
  }

delta : Signal Time.Time
delta = Signal.map Time.inSeconds (Time.fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
