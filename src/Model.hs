{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT, NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time
import Data.Int
import GHC.Generics
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Safe
import GHC.Unicode
import Data.Text.Lazy (toStrict, pack, unpack, Text)
import Web.Scotty (Parsable, parseParam)
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)
import Database
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

connStr = "host=localhost dbname=communis_db user=communis password=facilderecordar789 port=5432"

inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

toUserId :: Int64 -> UsersId
toUserId = toSqlKey

toPostId :: Int64 -> PostId
toPostId = toSqlKey

toCommentId :: Int64 -> CommentId
toCommentId = toSqlKey

--User CRUD
get_user :: ID -> IO (Maybe Users)
get_user (Id a) = inBackend . get . toUserId $ a
get_user (Email e) = inBackend . fmap (fmap unwrap) . getBy . UniqueEmail $ e
    where unwrap (Entity _ usr) = usr

type Page = Integer
type PerPage = Integer
data Pagination = Pag Page PerPage deriving Show

get_users  = inBackend . E.select $
             E.from $ \usr -> do
                        return usr

new_user :: Users -> IO ()
new_user (Users email pass alias image_url show_email _) = inBackend $ do
  now <- liftIO getCurrentTime
  usrid <- insert $ Users email pass alias image_url show_email now
  usr <- get usrid
  liftIO $ print usr

update_user :: String -> Users -> IO()
update_user em user = inBackend $ do
  Just (Entity userId _) <- getBy $ UniqueEmail em
  replace userId user

delete_user :: Int64 -> IO ()
delete_user = inBackend . delete . toUserId

--Post CRUD
get_post :: Int64 -> IO(Maybe Post)
get_post = inBackend . get . toPostId

new_post :: Post -> IO ()
new_post (Post atom material processing params image_url reference owner material_url _) = inBackend $ do
  now <- liftIO getCurrentTime
  postId <- insert $ Post atom material processing params image_url reference owner material_url now
  post <- get postId
  liftIO $ print post

update_post :: Int64 -> Post -> IO()
update_post id post = inBackend $ replace (toPostId id) post

delete_post :: Int64 -> IO ()
delete_post = inBackend . delete . toPostId

get_posts :: Int64 -> IO ([Entity Post])
get_posts i = inBackend $ selectList [PostOwner ==. toUserId i] []

-- Comments CRUD
get_comment :: Int64 -> IO(Maybe Comment)
get_comment = inBackend . get . toCommentId

new_comment :: Comment -> IO ()
new_comment (Comment owner post _ text) = inBackend $ do
  now <- liftIO getCurrentTime
  commentId <- insert $ Comment owner post now text
  comment <- get commentId
  liftIO $ print comment

update_comment :: Int64 -> Comment -> IO()
update_comment id comment = inBackend $ replace (toCommentId id) comment

delete_comment :: Int64 -> IO ()
delete_comment = inBackend . delete . toCommentId
