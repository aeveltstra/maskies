{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    The game expects key presses from the user.
    This file contains the key events and functions to
    handle those.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.1217.2132
-}
module Keys where

import Prelude

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
    deriving (Bounded, Read, Show, Eq, Enum)

{- The application captures key presses. This function converts those into Key events (see above). Unrecognized keys turn into the Wait Key event. This is used to determine which stage to show next: the Wait event makes the same stage to replay. -}
read :: Char -> Key
read 'w' = W
read 'a' = A
read 's' = S
read 'd' = D
read 'y' = Y
read 'n' = N
read 'h' = H
read 'q' = Q
read '1' = J1
read '2' = J2
read '3' = J3
read '4' = J4
read '5' = J5
read  _  = Wait

