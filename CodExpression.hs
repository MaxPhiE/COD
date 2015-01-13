module CodExpression where

import Data.Char (toUpper)
import Text.Regex (subRegex, mkRegex)

initFormat :: String -> String
initFormat expr = processed $ format (FormatState "" expr 0 0)

data FormatState = FormatState
    { processed :: String
    , remaining :: String
    , indents :: Int
    , inFunction :: Int
    } deriving (Show)

indentSize :: Int
indentSize = 4

format :: FormatState -> FormatState
format fs
    | length (remaining fs) == 0    = fs
    | otherwise                     = format (formatStep fs)

formatStep :: FormatState -> FormatState
formatStep fs
    | take 4 l == "IIf("                = fs { processed    = p ++ "IIf("
                                             , remaining    = drop 4 l
                                             , indents      = t + indentSize
                                             }
    | head l == ',' && f == 0           = fs { processed    = p ++ ['\n'] ++ (replicate t ' ') ++ ","
                                             , remaining    = tail l
                                             }
    | toUpperList (take 5 l) == " AND " = fs { processed    = p ++ ['\n'] ++ (replicate t ' ') ++ "AND" ++ ['\n'] ++ (replicate t ' ')
                                             , remaining    = drop 5 l
                                             }
    | toUpperList (take 4 l) == " OR "  = fs { processed    = p ++ ['\n'] ++ (replicate t ' ') ++ "OR" ++ ['\n'] ++ (replicate t ' ')
                                             , remaining    = drop 4 l
                                             }
    | head l == '(' && p == ""          = fs { processed    = "(" ++ ['\n'] ++ (replicate (t+indentSize) ' ')
                                             , remaining    = tail l
                                             , indents      = t + indentSize
                                             }
    | head l == '(' && (last p == ' '
                 || last p == '\n' )    = fs { processed    = p ++ "(" ++ ['\n'] ++ (replicate (t+indentSize) ' ')
                                             , remaining    = tail l
                                             , indents      = t + indentSize
                                             }
    | head l == '('                     = fs { processed    = p ++ "("
                                             , remaining    = tail l
                                             , inFunction = f + 1
                                             }
    | head l == ')' && f > 0            = fs { processed    = p ++ ")"
                                             , remaining    = tail l
                                             , inFunction = f - 1
                                             }
    | head l == ')'                     = fs { processed    = p ++ ['\n'] ++ (replicate (t-indentSize) ' ') ++ ")"
                                             , remaining = tail l
                                             , indents = t - indentSize
                                             }
    | length l >= 1                     = fs { processed    = p ++ [head l]
                                             , remaining    = tail l
                                             }
    | otherwise                         = fs { processed    = p ++ "ERROR"
                                             , remaining    = ""}
    where p = processed fs
          l = remaining fs
          t = indents fs
          f = inFunction fs

toUpperList :: String -> String
toUpperList = map toUpper

deformat :: String -> String
deformat text = replace "(\\( )" "(" .
                replace "( \\))" ")" .
                replace "(\t| )+" " " .
                replace "(\r|\n)+" " " $ text

replace :: String -> String -> String -> String
replace regex repl text = subRegex (mkRegex regex) text repl