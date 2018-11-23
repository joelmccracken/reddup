module Reddup where

import qualified Config as C
import qualified Options as O
import Control.Monad.Reader
import qualified Turtle as Tu
import qualified System.IO as SIO
import qualified Data.Text as T

data Reddup =
  Reddup
  { reddupConfig  :: C.ProcessedConfig
  , reddupOptions :: O.Options
  }
  deriving (Eq, Show)

type ReddupT = ReaderT Reddup Tu.Shell

debug' :: O.Options -> T.Text -> Tu.Shell ()
debug' opts txt =
  if O._debug opts then
    Tu.liftIO $ SIO.putStrLn $ T.unpack txt
  else
    return ()

debug :: T.Text -> ReddupT ()
debug txt = do
  reddup <- ask
  let opts    = reddupOptions reddup
  lift $ debug' opts txt

verbose :: T.Text -> ReddupT ()
verbose txt = do
  reddup <- ask
  let opts    = reddupOptions reddup
  lift $ verbose' opts txt

verbose' :: O.Options -> T.Text -> Tu.Shell ()
verbose' opts txt = do
  if O._debug opts || O._verbose opts then
    Tu.liftIO $ SIO.putStrLn $ T.unpack txt
  else
    return ()

isInteractive :: ReddupT Bool
isInteractive = do
  reddup <- ask
  let opts = reddupOptions reddup
  lift $ return $ O._interactive opts
