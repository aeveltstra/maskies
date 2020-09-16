{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    The game expects key presses from the user.
    This file contains the key events and functions to
    handle those.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.913.1034
-}
module Keys where

import Prelude
import qualified Data.Char

{- The game expects very specific key presses from the player. Those are listed below. Consider them events. Note that 'Wait' is not a key press... but it is an event. The Key events are used to determine which stage to show next. The Wait Key event is used to make the same stage replay. -}
data Key
    = Q
    | W
    | A
    | S
    | D
    | Y
    | N
    | H
    | J1
    | J2
    | J3
    | J4
    | J5
    | Wait
    deriving (Show, Eq, Enum)

{- The application captures key presses. This function converts those into Key events (see above). Unrecognized keys turn into the Wait Key event. This is used to determine which stage to show next: the Wait event makes the same stage to replay. -}
key :: Char -> Key
key 'w' = W
key 'a' = A
key 's' = S
key 'd' = D
key 'y' = Y
key 'n' = N
key 'h' = H
key 'q' = Q
key '1' = J1
key '2' = J2
key '3' = J3
key '4' = J4
key '5' = J5
key  _  = Wait

