{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant
import Servant.HTML.Lucid
import Types

-- | 'Thread' management API: create and read only.
--
-- TODO: might be wise to not have ToHtml instances for Page, so
-- that safeLink orphan instances hack can disappear.
type ThreadAPI
    = ReqBody '[JSON] Create :> Post '[JSON] Id
 :<|> Capture "id"    Id     :>
        (    Get     '[JSON, HTML] Thread
        :<|> ReqBody '[JSON] Comment :> Post '[JSON] Comment
        )

type API = Get '[JSON, HTML] (Page IndexPage)
      :<|> "thread" :> ThreadAPI
      :<|> "static" :> Raw

api :: Proxy API
api = Proxy

-- | Safely link to a 'Thread' by its 'Id'
safeThreadLink :: Int -> URI
safeThreadLink i = safeLink api with i where
  with = Proxy ::
    Proxy ("thread" :> Capture "id" Id :> Get '[JSON, HTML] Thread)
