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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users
    email String
    password String
    alias String
    image_url String
    show_email Bool
    UniqueEmail email
    date UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Post
    atom Int
    material String
    processing String
    params String
    image_url String
    reference String
    owner UsersId
    material_url String
    date UTCTime default=CURRENT_TIMESTAMP
    deriving Show
Comment
    owner UsersId
    post PostId
    date UTCTime default=CURRENT_TIMESTAMP
    text String
    deriving Show
|]

newtype USER = USER (Int64, (Maybe Users))
instance ToJSON USER where
  toJSON (USER (uid, (Just (Users e p a i s d)))) = object
        [ "id" .= uid,
          "email" .= e,
          "password" .= p,
          "alias" .= a,
          "image_url" .= i,
          "show_email" .= s,
          "date" .= d
        ]
  toJSON (USER (uid, Nothing)) = object []

instance ToJSON Users where
  toJSON (Users e p a i s d) = object
        [ "email" .= e,
          "password" .= p,
          "alias" .= a,
          "image_url" .= i,
          "show_email" .= s,
          "date" .= d
        ]

instance FromJSON Users where
    parseJSON (Object o) = Users
        <$> o .: "email"
        <*> o .: "password"
        <*> o .: "alias"
        <*> o .: "image_url"
        <*> o .: "show_email"
        <*> o .: "date"
    parseJSON _ = fail "Invalid Users"

newtype POST = POST (Int64, (Maybe Post))
instance ToJSON POST where
  toJSON (POST (uid, (Just (Post a m p pp i r o mm d)))) = object
        [ "id" .= uid,
          "atom" .= a,
          "material" .= m,
          "processing" .= p,
          "params" .= pp,
          "image_url" .= i,
          "reference" .= r,
          "owner" .= o,
          "material_u" .= mm,
          "date" .= d
        ]
  toJSON (POST (uid, Nothing)) = object []

instance ToJSON Post where
  toJSON (Post a m p pp i r o mm d) = object
        [ "atom" .= a,
          "material" .= m,
          "processing" .= p,
          "params" .= pp,
          "image_url" .= i,
          "reference" .= r,
          "owner" .= o,
          "material_u" .= mm,
          "date" .= d
        ]

instance FromJSON Post where
    parseJSON (Object o) = Post
        <$> o .: "atom"
        <*> o .: "material"
        <*> o .: "processing"
        <*> o .: "params"
        <*> o .: "image_url"
        <*> o .: "reference"
        <*> o .: "owner"
        <*> o .: "material_u"
        <*> o .: "date"
    parseJSON _ = fail "Invalid Post"

newtype COMMENT = COMMENT (Int64, (Maybe Comment))
instance ToJSON COMMENT where
  toJSON (COMMENT (uid, (Just (Comment o p d t)))) = object
        [ "id"    .= uid,
          "owner" .= o,
          "post"  .= p,
          "date"  .= d,
          "text"  .= t
        ]
  toJSON (COMMENT (uid, Nothing)) = object []

instance ToJSON Comment where
  toJSON (Comment o p d t) = object
        [ "owner" .= o,
          "post"  .= p,
          "date"  .= d,
          "text"  .= t
        ]

instance FromJSON Comment where
    parseJSON (Object o) = Comment
        <$> o .: "owner"
        <*> o .: "post" 
        <*> o .: "date" 
        <*> o .: "text" 
    parseJSON _ = fail "Invalid Comment"

data ID = Email String | Id Int64 deriving (Show)

connStr = "host=localhost dbname=communis_db user=communis password=facilderecordar789 port=5432"

inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

toUserId :: Int64 -> UsersId
toUserId = toSqlKey

-- toUserId2 :: ID -> Maybe UsersId
-- toUserId2 (Id a) = Just $ toSqlKey a
-- toUserId2 (Email em) = inBackend $ do
--   maybeUser <- getBy $ UniqueEmail em
--   case maybeUser of
--     Nothing -> liftIO Nothing
--     (Entity userId _) -> liftIO $ Just userId

toPostId :: Int64 -> PostId
toPostId = toSqlKey

toCommentId :: Int64 -> CommentId
toCommentId = toSqlKey

--User CRUD
get_user :: ID -> IO (Maybe (Entity Users))
get_user (Id a) = inBackend . get . toUserId $ a
get_user (Email e) = inBackend . getBy $ UniqueEmail e

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
