{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod
import           Data.Text              (Text, unpack)
import           Control.Applicative    ((<$>))
import           CodExpression          (initFormat, deformat)
import           System.IO              (appendFile)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Time            (getClockTime)


data COD = COD
mkYesod "COD" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod COD

instance RenderMessage COD FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        [whamlet| ^{defaultForm ""} |]

postHomeR :: Handler Html
postHomeR = do
    myExpression <- runInputPost $ iopt textareaField "exprField"
    isFormat <- runInputPost $ iopt textField "format"
    isDeformat <- runInputPost $ iopt textField "deformat"
    case myExpression of
        Just expression -> defaultLayout $ do 
            case isFormat of
                Just f -> do
                    [whamlet| ^{defaultForm $ processRequest "format" (unpack $ unTextarea expression)} |]
                _ -> case isDeformat of
                    Just d -> do
                        [whamlet| ^{defaultForm $ processRequest "deformat" (unpack $ unTextarea expression)} |]
                    _ -> [whamlet|
                            <p>Error.
                         |]
        _ -> defaultLayout $ do
            [whamlet|
                <p>Error
            |]

defaultForm :: String -> Widget
defaultForm expression = do
    setTitle "COD Format Expressions"
    toWidgetHead [hamlet| <meta http-equiv="content-type" content="text/html; charset=UTF-8"> |]
    [whamlet| ^{defaultCSS} |]
    [whamlet| ^{tabIndentJS} |]
    toWidget [julius| 
                var isCtrl = false;
                document.onkeyup = function(event) {
                    if(event.which == 17) isCtrl = false;
                }
                document.onkeydown = function(event) {
                    if(event.which == 17) isCtrl = true;
                    if(event.which == 70 && isCtrl == true) {
                        document.getElementById("formatSubmit").click();
                        return false;
                    } else if(event.which == 68 && isCtrl == true) {
                        document.getElementById("deformatSubmit").click();
                        return false;
                    }
                }
             |]
    [whamlet|
        <h1>COD Format Expressions
        <form method=post>
            <textarea name="exprField" id="exprField">#{expression}
            <input type="submit" id="formatSubmit" name="format" value="format (ctrl+f)">
            <input type="submit" id="deformatSubmit" name="deformat" value="deformat (ctrl+d)">
    |]

processRequest :: String -> String -> String
processRequest "format" s   = unsafePerformIO $ callInitFormat s
processRequest "deformat" s = unsafePerformIO $ callDeformat s

callInitFormat :: String -> IO String
callInitFormat s = do
    customLog "  format" (deformat s)
    return $ initFormat s

callDeformat :: String -> IO String
callDeformat s = do
    let x = deformat s
    customLog "deformat" x
    return x

customLog :: String -> String -> IO ()
customLog t s = do
    time <- getClockTime
    appendFile "cod.log" ((show time) ++ " - " ++ t ++ ": " ++ s ++ "\n")
    --getClockTime >>= (\time -> appendFile "cod.log" ((show time) ++ " - " ++ t ++ ": " ++ s ++ "\n"))


{--------------------------------
 - js widgets -------------------
 --------------------------------}

tabIndentJS :: Widget
tabIndentJS = toWidgetHead [julius|
    window.onload = function() {
        document.querySelector("textarea").addEventListener('keydown',function(event) {
            if(event.which == 9) {
                var start = this.selectionStart;
                var end = this.selectionEnd;

                var target = event.target;
                var value = target.value;

                target.value = value.substring(0, start) + "\t" + value.substring(end);
                this.selectionStart = this.selectionEnd = start + 1;

                event.preventDefault();
            }
        },false);
    }
|]


{--------------------------------
 - css widget -------------------
 --------------------------------}

defaultCSS :: Widget
defaultCSS = toWidgetHead [lucius| 
    form {
        padding:            20px 20px;
        border-radius:      15px;
        background-color:   #83B81A;
        font-size:          14px;
        font-family:        arial;
    }
    input {
        font-size:          14px;
        width:              200px;
    }
    button {
        margin-top:         20px;
        font-size:          14px;
        font-family:        arial;
    }
    textarea {
        font-size:          14px;
        font-family:        courier;
        width:              100%;
        height:             500px;
        margin-top:         10px;
        margin-bottom:      10px
    }
    h1 {
        padding:            20px 20px;
        border-radius:      15px;
        background-color:   #83B81A;
        color:              white;
    }
    body {
        background-color:   #FFFFFF;
    }
    button {
        margin-top:         20px;
        margin-left:        216px;
        font-size:          14px;
        font-family:        arial;
    }
    div {
        margin-top:         10px;
    }
    label {
        display:            inline-block;
        width:              70px;
    }
    p {
        margin-left:        25px;
    }
|]


{--------------------------------
 - main function ----------------
 --------------------------------}

main = warp 3000 COD