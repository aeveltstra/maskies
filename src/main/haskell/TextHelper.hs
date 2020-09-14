{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains functions to make dealing with
    Data.Text.Text a little less verbose. It needed to
    be put in its own library because it needed to get
    used by multiple other scripts and I wanted to 
    prevent code duplicaiton.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.913.2329
-}
module TextHelper where

import Prelude
import qualified Data.Text as T

{- Built-in Haskell function putStrLn operates on Strings. Due to constraints in the String type and how Haskell deals with them, it was chosen to use Data.Text instead. To output those to console, we have to unpack them. To prevent having to do that every time, use this function. -}
ln :: T.Text -> IO ()
ln input = putStrLn $ T.unpack input
