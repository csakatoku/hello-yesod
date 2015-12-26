module Handler.Message where

import Import


getMessageR :: Handler Html
getMessageR = do
    mmessage <- lookupSession "message"
    let message = case mmessage of
                      Just value -> value
                      Nothing    -> "none"

    (widget, _) <- generateFormPost messageForm

    defaultLayout $(widgetFile "message")


postMessageR :: Handler ()
postMessageR = do
    ((result, _), _) <- runFormPost messageForm
    case result of
        FormSuccess message -> do
            _ <- setSession "message" (messageContent message)
            redirect MessageR
        _ ->
            redirect MessageR


data Message = Message
    { messageContent :: Text
    }
    deriving Show


messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textField "content" Nothing
