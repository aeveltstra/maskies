{- | Wraps text to the width of the terminal window,
 -   such that short words aren't broken. Long words 
 -   will be broken according to regular grammar rules.
 -}
module TextWrapper (wrap) where

import qualified System.Console.Terminal.Size as SCTS
import qualified Text.Wrap as Wrap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

{- | Specify what character(s) to add as the left margin. 
 -   Generally expected to be just a space.
 -}
firstMargin :: T.Text
firstMargin = T.pack " "

{- | Specify what character(s) to add to the left of each
 -   line. Generally equals a new-line character and the
 -   first margin (see above).
 -}
prefix :: T.Text
prefix = T.pack "\n " 

{- | How the text line wrapper is supposed to behave.
 -   At the moment this is set to remove received indentation
 -   and to break long words.
 -}
wrapSettings :: Wrap.WrapSettings
wrapSettings = Wrap.WrapSettings {
    Wrap.preserveIndentation = False
    , Wrap.breakLongWords = True
  }

{- | Wraps text lines to fit the window width and writes them 
 -   to stdOut.
 -   Adds a margin to the left of the first line if so indicated 
 -   by firstMargin above.
 -   Adds a new-line character and a margin to the left of each 
 -   line if so indicated by prefix above.
 -   If the window width cannot be determined, this method throws
 -   an error.
 -   If the window width is 4 columns or less, this method throws
 -   an error.
 -}
wrap :: TL.Text -> IO ()
wrap t = do
  p <- SCTS.size
  case p of
    Nothing -> error "Could not get your window size."
    Just (SCTS.Window _ cols) -> do
      case (4>cols) of 
        True -> error "Your window is too narrow."
        False -> TLIO.putStrLn out where
          columnMargin = T.length firstMargin
          innerWidth = (cols - (columnMargin * 2))
          tw = Wrap.wrapTextToLines wrapSettings innerWidth (TL.toStrict t)
          out = TL.fromStrict (T.intercalate prefix (firstMargin : tw))
  
