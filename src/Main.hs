{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant

import Types
import Types.HtmlInstances
import API
import MemoryBackend

import Network.Wai
import Network.Wai.Handler.Warp

-- | Loads an 'IndexPage'-builder function.
-- In the real-world this might read some configuration from a file.
pageConfig :: IO ([(Id, Thread)] -> Page IndexPage)
pageConfig = do
  let img   = "static/logo.png"
      title = "forgetit"
  return (Page title img . IndexPage)

main = do
  mkPage <- pageConfig
  server <- newMemoryBackend mkPage
  run 8001 $ serve api server
