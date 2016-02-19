{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty
import Model
import Data.Int
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 8080 $ routes

routes :: ScottyM ()
routes = do
  get "/hello/:name" $ do
    name <- param "name"
    text ("Hello, " <> name <> "!")
  get "/users/:id" $ do
    id <- param "id"
    usr <- liftIO . get_user $ id
    json $ USER (id, usr)
  post "/users/" $ do
    usr <- liftIO . get_user $ id
    json $ USER (id, usr)


