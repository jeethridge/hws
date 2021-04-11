module Lib
    ( wsAddress
    ) where

import Network.URI

type Host = String 
type Port = Int 
type Path = String
type ErrorMsg = String

parsePort :: URIAuth -> Port
parsePort p = (read . tail . uriPort) p

wsAddressInfo :: String -> Either ErrorMsg (Host, Port, Path)
wsAddress uriStr = do
    case parseURI uriStr of
        -- TODO: Could be simpler to just let this be Nothing -> Nothing and handle error messages in main.
        Nothing -> Left $ "Bad format: " <> uriStr
        Just uri -> do
            case uriAuthority uri of
                Nothing -> Left $ "Bad format: " <> uriStr
                Just au -> Right (uriRegName au, parsePort au, uriPath uri)