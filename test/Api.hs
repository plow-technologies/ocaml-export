{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant hiding (HList)
import Servant.API hiding (HList)

import Data.Typeable
import GHC.TypeLits

import Network.Wai
import Network.Wai.Handler.Warp

import Servant.InAndOut

import Product
import Sum

type ProductAPI = InAndOutListWithRouteNamesAPI '[Person, Company, Suit, Card] ["person", "company", "suit", "card"]

productServer :: Server ProductAPI
productServer
     = return
  :<|> return
  :<|> return
  :<|> return

productAPI :: Proxy ProductAPI
productAPI = Proxy

productApp :: Application
productApp = serve productAPI productServer

type SumAPI = InAndOutWithRouteNamesAPI '[OnOrOff,NameOrIdNumber] ["onOrOff","nameOrIdNumber"]

sumServer :: Server SumAPI
sumServer = return :<|> return

sumAPI :: Proxy SumAPI
sumAPI = Proxy

sumApp :: Application
sumApp = serve sumAPI sumServer
