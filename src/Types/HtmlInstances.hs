{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.HtmlInstances where

import GHC.Generics
import Control.Monad
import Lens.Family ((^.))

import Data.Monoid ((<>))
import Data.Char
import Data.Text (Text(..), pack)
import Data.Text.Encoding (decodeLatin1, encodeUtf8, decodeUtf8')

import Servant hiding (URI)
import Servant.Common.Text 

import URI.ByteString as BU
import Data.Aeson
import Data.Aeson.Types
import Lucid

import Types
import API

---------- HTML Instances ------------

newtype WrapThread = WrapThread { unWrapThread :: Thread }
  deriving(Eq, Show)

instance ToHtml WrapThread where
  toHtmlRaw = toHtml
  toHtml (WrapThread t) = do
    a_ [ class_ "thread-link"
       , href_ $ maybe "" id (threadLink t >>= uriToText)
       ] (toHtml $ threadTitle t)
    foldMap toHtml (threadComments t)

instance ToHtml Thread where
  toHtmlRaw = toHtml
  toHtml t =
    toHtml . Page "forgetit" "/static/logo.png"  . WrapThread $ t

instance ToHtml Comment where
  toHtml (Comment u t) = div_ [class_ "comment"] $ do
    span_ [class_ "user"] (toHtml u)
    span_ [class_ "text"] (toHtml t)
  toHtmlRaw = toHtml

-- | Generic 'Page' template
instance ToHtml a => ToHtml (Page a) where
  toHtml (Page title logo content) = html_ $ do
    head_ $ do
      link_ [ rel_ "stylesheet"
            , type_ "text/css"
            , href_ "/static/style.css"
            ]
    body_ $ do
      div_ [class_ "page-header"] $ do
        img_ [class_ "logo", src_ logo, width_ "50", height_ "50"]
        h1_ (toHtml title)
      div_ [class_ "page-body"] (toHtml content)
  
  toHtmlRaw = toHtml

instance ToHtml IndexPage where
  toHtml (IndexPage threads) = do
    div_ [class_ "index-page"] $
      -- TODO: button to view thread
      foldMap (uncurry threadSummary) threads

  toHtmlRaw = toHtml

-- | HTML summarising a thread for use in showing lists of 'Thread's.
threadSummary :: Monad m => Id -> Thread -> HtmlT m ()
threadSummary threadId (Thread link title cs) = do
  div_ [ class_ "thread-summary"] $ do
    div_ $ do
      case (link >>= uriToText) of
           Just l  -> a_ [href_ l] $ h2_ (toHtml title)
           Nothing -> a_ [href_ selfLink] $ h2_ (toHtml title)
      case targetHost of
           Just h  -> span_ $ "(" <> toHtml h <> ")"
           Nothing -> return ()
    a_ [href_ selfLink] $
      span_ [class_ "comment-count"] $ toHtml (show n) <> " comments"

  where
    n          = length cs
    selfLink   =
      (pack . Servant.uriPath . safeThreadLink) threadId
    targetLink :: Maybe Text
    targetLink = link >>= uriToText

    -- This is kinda ew.
    targetHost :: Maybe Text
    targetHost = do
      l <- link >>= BU.uriAuthority
      let host = l ^. authorityHostL . hostBSL
      either (const Nothing) Just (decodeUtf8' host)
