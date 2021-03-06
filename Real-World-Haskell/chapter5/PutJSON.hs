module PutJSON where

import Data.List (intercalate)
import SimpleJSON

-- represent JSON data as a string
renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool b) = if b then "true" else "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k, v) = show k ++ "; " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)

-- print JValue
putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue