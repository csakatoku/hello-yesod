module Handler.Wiki where

import Import

import qualified Data.List as DL
import qualified Data.Text.Lazy as TL
import Text.Markdown (markdown, def)

wikiForm mtext = renderDivs $ areq textareaField "Content" mtext

getWikiR :: [Text] -> Handler RepHtml
getWikiR title = do
  let title' = if null title then "Home" else DL.head title
  mpage <- runDB $ getBy $ UniqueTitle title'

  let mtext = case mpage of
        Nothing              -> Nothing
        Just (Entity _ page) -> Just $ wikiPageContent page

  (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext

  defaultLayout $ do
    case mtext of
      Nothing -> do
        [whamlet|<p>Page does not yet exist|]

      Just text -> do
        toWidget $ markdown def $ TL.fromStrict text

    $(widgetFile "wiki/get_wiki")


postWikiR :: [Text] -> Handler RepHtml
postWikiR title = do
  let title' = if null title then "Home" else DL.head title
  ((res, form), enctype) <- runFormPost $ wikiForm $ fmap Textarea $ Nothing
  case res of
    FormSuccess (Textarea text) -> do
      maybePage <- runDB $ getBy $ UniqueTitle title'
      case maybePage of
        Nothing -> do
          runDB $ insert $ WikiPage title' text
          setMessage $ toHtml $ title' <> " created"
          redirect $ WikiR title

        Just (Entity pageId _) -> do
          runDB $ update pageId [WikiPageContent =. text]
          setMessage $ toHtml $ title' <> " updated"
          redirect $ WikiR title

    _ -> defaultLayout $ do $(widgetFile "wiki/post_wiki")
