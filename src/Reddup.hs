module Reddup where

import qualified Config as C
import qualified Options as O
import Control.Monad.Reader
import qualified Turtle as Tu
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data ReddupD =
  ReddupD
  { reddupConfig  :: C.ProcessedConfig
  , reddupOptions :: O.Options
  } deriving (Eq, Show)

type Reddup = ReaderT ReddupD Tu.Shell

debug' :: O.Options -> T.Text -> Tu.Shell ()
debug' opts txt =
  if O.optDebug opts then
    Tu.liftIO $ TIO.putStrLn txt
  else
    return ()

debug :: T.Text -> Reddup ()
debug txt = do
  reddup <- ask
  let opts = reddupOptions reddup
  lift $ debug' opts txt

verbose :: T.Text -> Reddup ()
verbose txt = do
  reddup <- ask
  let opts = reddupOptions reddup
  lift $ verbose' opts txt

verbose' :: O.Options -> T.Text -> Tu.Shell ()
verbose' opts txt = do
  if O.optDebug opts || O.optVerbose opts then
    Tu.liftIO $ TIO.putStrLn txt
  else
    return ()

isInteractive :: Reddup Bool
isInteractive = do
  reddup <- ask
  let opts = reddupOptions reddup
  lift $ return $ O.optInteractive opts
