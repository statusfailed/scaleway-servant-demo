{-# LANGUAGE OverloadedStrings #-}
module MemoryBackend where

import Servant
import Control.Monad.Trans.Either
import Control.Monad

import Types
import API

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.IORef

import Data.Text
import Data.Text.Encoding
import URI.ByteString as URI

parseURL :: Text -> Maybe URI.URI
parseURL t =
  case parseURI strictURIParserOptions (encodeUtf8 t) of
    Left err -> mzero
    Right u  -> return u

type Store = Map Id Thread

defaultStore :: Store
defaultStore = M.empty

-- | Add a 'Thread' to the 'Store'
add :: Thread -> Store -> (Store, Id)
add t s = (M.insert id' t s, id')
  where id' = if M.null s then 0 else fst (M.findMax s) + 1

-- | Add a thread to the store from a 'Create' request
addFromCreate :: Create -> Store -> (Store, Id)
addFromCreate c s = add (fromCreate c) s

addComment :: Id -> Comment -> Store -> (Store, Bool)
addComment i c s = maybe (s, False) f (M.lookup i s)
  where f t =
          let cs = c : threadComments t
              t' = t { threadComments = cs }
          in  (M.insert i t' s, True)

-- | Create a "backend": an API implementation using an 'IORef'
newMemoryBackend
  :: ([(Id, Thread)] -> Page IndexPage) -> IO (Server API)
newMemoryBackend mkIndexPage = do
  ref <- newIORef defaultStore
  return (apiServer mkIndexPage ref)

-- | The API implementation. Requires a "page factory" and a 'IORef'
-- to store data.
--
-- The function to create a page from a list of threads is
-- threaded through the API- this should really be a ReaderT.
apiServer
  :: ([(Id, Thread)] -> Page IndexPage) -> IORef Store -> Server API
apiServer f r =
  indexServer f r :<|> threadServer r :<|> serveDirectory "static"

indexServer
  :: ([(Id, Thread)] -> Page IndexPage)
  -> IORef Store
  -> EitherT ServantErr IO (Page IndexPage)
indexServer f r = EitherT . fmap (Right . f) $ threads
  where threads = fmap (Prelude.reverse . M.toList) (readIORef r)

-- | Serve threads from the global 'Store'
threadServer :: IORef Store -> Server ThreadAPI
threadServer r =
  createThread :<|> (\i -> retrieveThread i :<|> createComment i)
  where
    createThread :: Create -> EitherT ServantErr IO Id
    createThread c = EitherT $ do
      print c -- debug
      i <- atomicModifyIORef' r (addFromCreate c)
      return (Right i)

    retrieveThread :: Id -> EitherT ServantErr IO Thread
    retrieveThread i =
      EitherT . fmap (maybe (Left err404) return . M.lookup i) $ readIORef r

    createComment :: Id -> Comment -> EitherT ServantErr IO Comment
    createComment i c = EitherT $ do
      print c -- debug
      s <- atomicModifyIORef' r (addComment i c)
      return $ case s of
                    True  -> Right c
                    False -> Left err400
