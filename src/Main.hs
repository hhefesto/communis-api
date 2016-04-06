{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty
import Model
import Data.Int
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Text.Lazy
import Database

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 8000 $ routes

routes :: ScottyM ()
routes = do
  -- Hello
  get "/hello/:name" $ do
    name <- param "name"
    text ("Hello, " <> name <> "!")
  -- users rest
  get "/users/:id" $ do
    id <- param "id"
    usr <- liftIO . get_user $ id
    json $ USER (id, usr)
  post "/users/" $ do
    u <- jsonData :: ActionM Users
    liftIO . new_user $ u
    json u
  delete "/users/:id" $ do
    id <- param "id"
    liftIO . delete_user $ id
    text ("User " <> (pack . show $ id) <> " deleted.")
  put "/users/:email" $ do
    email <- param "email"
    u <- jsonData :: ActionM Users
    liftIO $ update_user email u
    json u

  -- post rest
  get "/post/:id" $ do
    id <- param "id"
    pst <- liftIO . get_post $ id
    json $ POST (id, pst)
  post "/post/" $ do
    u <- jsonData :: ActionM Post
    liftIO . new_post $ u
    json u
  delete "/post/:id" $ do
    id <- param "id"
    liftIO . delete_post $ id
    text ("User " <> (pack . show $ id) <> " deleted.")
  put "/post/:email" $ do
    email <- param "email"
    p <- jsonData :: ActionM Post
    liftIO $ update_post email p
    json p

