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
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Network.URI         as URI


--------------------------------------------------------------------------------

prompt :: String -> IO Text
prompt str = do
    putStr str
    hFlush stdout
    T.getLine

app :: WS.ClientApp ()
app conn = do
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    let loop = do
            line <- prompt "> "
            unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let
        addr = head args
        port = read (args!!1)::Int
        route = args!!2
    -- eg. "127.0.0.1" 8123 "/middleware"
    withSocketsDo $ WS.runClient addr port route app
