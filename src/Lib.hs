module Lib
    ( wsAddress
    ) where

import Network.URI

type Host = String 
type Port = Int 
type Path = String
type ErrorMsg = String

parsePort :: URIAuth -> Port
parsePort = read . tail . uriPort

wsAddress :: String -> Maybe (Host, Port, Path)
wsAddress uriStr =
    case parseURI uriStr of
        Nothing -> Nothing
        Just uri ->
            case uriAuthority uri of
                Nothing -> Nothing
                Just au -> Just (uriRegName au, parsePort au, uriPath uri)
