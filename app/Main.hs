--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import           System.IO           (hFlush, stdout)
import           System.Environment
import           Lib              (wsAddress)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Network.URI  as URI


--------------------------------------------------------------------------------

app :: WS.ClientApp ()
app conn = do
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn $ "< " <> msg
        putStr "> "
        hFlush stdout

    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
-- | Example usage: 
-- stack exec ws-exe ws://localhost:8123/middleware
-- < Request served by 31158f9596be
-- > Hello
-- < Hello
main :: IO ()
main = do
    args <-  getArgs 
    let uriStr = head args
    case wsAddress uriStr of
        Nothing -> print $ "Bad URI: " <> uriStr
        Just (addr, port, path) -> withSocketsDo $ WS.runClient addr port path app
