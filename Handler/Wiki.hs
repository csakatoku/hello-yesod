module Handler.Wiki where

import Import

import qualified Data.List as DL
import qualified Data.Text.Lazy as TL

import Text.Markdown (markdown, def)


wikiForm mtext = renderDivs $ areq textareaField "content" mtext


getWikiR :: [Text] -> Handler Html
getWikiR ts = do
  mpage <- runDB $ getBy $ UniqueTitle title

  (form, enctype) <- generateFormPost $ wikiForm $ fmap Textarea $ pageContent mpage

  defaultLayout $ do
      case mpage of
          Nothing -> do
              [whamlet|<p>Page does not yet exist|]

          Just (Entity _ page) -> do
              toWidget $ markdown def $ TL.fromStrict $ wikiPageContent page

      $(widgetFile "wiki/wiki-page")
  where
    title = if null ts then "Home" else DL.head ts

    pageContent mpage = case mpage of
      Just (Entity _ page) -> Just $ (wikiPageContent page)
      Nothing -> Nothing


postWikiR :: [Text] -> Handler Html
postWikiR ts = do
  let title = if null ts then "Home" else DL.head ts

  ((result, form), enctype) <- runFormPost $ wikiForm $ fmap Textarea $ Nothing

  case result of
      FormSuccess (Textarea text) -> do
          maybePage <- runDB $ getBy $ UniqueTitle title
          case maybePage of
              Nothing -> do
                  runDB $ insert $ WikiPage title text
                  setMessage $ toHtml $ title <> " created"
                  redirect $ WikiR ts

              Just (Entity pageId _) -> do
                  runDB $ update pageId [WikiPageContent =. text]
                  setMessage $ toHtml $ title <> " updated"
                  redirect $ WikiR ts

      _ -> defaultLayout $(widgetFile "wiki/wiki-page")
