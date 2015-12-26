module Handler.Blog where

import Import


entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq textField "Title" Nothing
    <*> areq textField "Content" Nothing


getBlogR :: Handler Html
getBlogR = do
  articles <- runDB $ selectList [] [Desc ArticleTitle]
  (widget, enctype) <- generateFormPost entryForm
  defaultLayout $(widgetFile "blog/article-list")


postBlogR :: Handler Html
postBlogR = do
  ((result, widget), enctype) <- runFormPost entryForm
  case result of
    FormSuccess article -> do
      articleId <- runDB $ insert article
      setMessage $ toHtml $ (articleTitle article) <> " created"
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form"
      $(widgetFile "blog/article-form-error")


getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
  article <- runDB $ get404 articleId
  defaultLayout $ do
    setTitle $ toHtml $ articleTitle article
    $(widgetFile "blog/article-detail")
