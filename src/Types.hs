{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types where

import GHC.Generics
import Control.Monad
import Lens.Family ((^.))

import Data.Monoid ((<>))
import Data.Char
import Data.Text (Text(..))
import Data.Text.Encoding (decodeLatin1, encodeUtf8, decodeUtf8')

import Servant hiding (URI)
import Servant.Common.Text 

import URI.ByteString
import Data.Aeson
import Data.Aeson.Types
import Lucid

-- | Type of IDs
type Id = Int

uriToText :: URI -> Maybe Text
uriToText u = e2m $ decodeUtf8' (serializeURI' u)
  where e2m = either (const Nothing) Just

------- URL JSON instances -------
instance FromJSON URI where
  parseJSON (String t)
    = case parseURI strictURIParserOptions (encodeUtf8 t) of
           Left err -> mzero
           Right u  -> return u
  parseJSON _          = mzero

instance ToJSON URI where
  toJSON = String . decodeLatin1 . serializeURI'
----------------------------------

------ Main datatypes ------------

-- TODO: Use the "generic CRUD" trick

data Create = Create
  { createLink  :: Maybe URI
  , createTitle :: Text
  } deriving(Eq, Show, Generic)

data Thread = Thread
  { threadLink     :: Maybe URI
  , threadTitle    :: Text
  , threadComments :: [Comment]
  } deriving(Eq, Show, Generic)

data Comment = Comment
  { commentUser :: Text
  , commentText :: Text
  } deriving(Eq, Show, Generic)

data Page a = Page
  { pageTitle   :: Text
  , pageLogo    :: Text
  , pageContent :: a
  } deriving(Eq, Show, Generic)

data IndexPage = IndexPage
  { pageThreads   :: [(Id, Thread)]
  } deriving(Eq, Show, Generic)

-- | Make a 'Thread' from a 'Create' request
fromCreate :: Create -> Thread
fromCreate (Create l t) = Thread l t []

----------------- JSON Instances --------------
instance FromJSON Create where
  parseJSON =
    genericParseJSON defaultOptions {
      fieldLabelModifier = drop 6 . map toLower
    }

instance ToJSON   Create where
  toJSON =
    genericToJSON defaultOptions {
      fieldLabelModifier = drop 6 . map toLower
    }

instance FromJSON Comment where
  parseJSON =
    genericParseJSON defaultOptions {
      fieldLabelModifier = drop 7 . map toLower
    }

instance ToJSON   Comment where
  toJSON    =
    genericToJSON defaultOptions {
      fieldLabelModifier = drop 7 . map toLower
    }

instance FromJSON Thread where
  parseJSON =
    genericParseJSON defaultOptions {
      fieldLabelModifier = drop 6 . map toLower
    }

instance ToJSON   Thread where
  toJSON    =
    genericToJSON defaultOptions {
      fieldLabelModifier = drop 6 . map toLower
    }

instance FromJSON a => FromJSON (Page a) where
  parseJSON =
    genericParseJSON defaultOptions {
      fieldLabelModifier = drop 4 . map toLower
    }

instance ToJSON a   => ToJSON   (Page a) where
  toJSON    =
    genericToJSON defaultOptions {
      fieldLabelModifier = drop 4 . map toLower
    }

instance FromJSON IndexPage where
  parseJSON =
    genericParseJSON defaultOptions {
      fieldLabelModifier = drop 4 . map toLower
    }

instance ToJSON   IndexPage where
  toJSON    =
    genericToJSON defaultOptions {
      fieldLabelModifier = drop 4 . map toLower
    }
