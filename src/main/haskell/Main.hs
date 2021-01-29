{-# LANGUAGE CPP, OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.21.026.2217
-}
module Main where
import qualified Data.Char
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Keys
import qualified NameValidation as NV
import qualified TextWrapper as TW

import Prelude
import qualified Stages as S
import qualified System.IO
import qualified System.Random as R

{- This is the game loop. It keeps looping stages until the player quits. It assumes that the stage will give the player options for keys to press to have actions performed. It then asks the player for their input, uses that to figure out which stage to show next, and recurses in on itself. -}
loop :: S.Stage -> TL.Text -> IO ()
loop S.Quit player = TW.wrap $ S.stage S.Quit player
#if defined(mingw32_HOST_OS)
loop theStage player
  = (TW.wrap $ S.stage theStage player) >> TLIO.getLine >>=
      \ k -> if 0 == TL.length k 
           then loop (S.next theStage (Keys.read '-')) player
           else loop (S.next theStage (Keys.read (k!!0))) player
#else
loop theStage player
  = (TW.wrap $ S.stage theStage player) >> getChar >>=
      \ k -> loop (S.next theStage (Keys.read k)) player
#endif

{- Sanitizes and validates the player's name. If needed, reprimands the player for choosing a foolish name, and announces a substitute. Returns either the sanitized input or the substitute. -}
checkPlayerName :: R.StdGen -> TL.Text -> IO TL.Text
checkPlayerName randomizerSeed taintedName = do 
    let unvalidated = TL.strip $ TL.filter Data.Char.isPrint taintedName
        result = NV.validate unvalidated
        player = NV.replace unvalidated result randomizerSeed
    NV.show result player 
    return player

{- 
  This turns off input buffering for non-Windows OS-es. 
  Buffering makes it so that the executable will 
  wait for an enter key press after any input.
  Our application wouild see the enter key and 
  act on it, too, acting twice on the same stage. 
  By turning off buffering, that is avoided.
-}
disableBuffering :: IO ()
#if defined(mingw32_HOST_OS)
disableBuffering = System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
#else
disableBuffering = System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
#endif

{- This is the entry point of the application. It shows the Init stage, which asks the player to enter their name. It captures and validates that name, and uses the result to kick off the game loop. -}
main :: IO ()
main
  = (TW.wrap $ S.stage S.Init "") >> TLIO.getLine >>=
      \ p -> disableBuffering >> R.newStdGen >>=
        \ s -> checkPlayerName s p >>= loop S.A1DarkHallway

