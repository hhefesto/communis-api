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


module Database where

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

newtype USER = USER (ID, (Maybe Users))
instance ToJSON USER where
  toJSON (USER ((Id uid), (Just (Users e p a i s d)))) = object
        [ "id" .= uid,
          "email" .= e,
          "password" .= p,
          "alias" .= a,
          "image_url" .= i,
          "show_email" .= s,
          "date" .= d
        ]
  toJSON (USER ((Email _), (Just (Users e p a i s d)))) = object
        [ "email" .= e,
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

instance Parsable ID where
  parseParam t
    | foldr (\x y -> y && (isDigit x)) True (unpack t) = Right $ Id . read . unpack $ t
    | isValid . encodeUtf8 . toStrict $ t = Right $ Email $ unpack t
    | otherwise = Left $ pack "Not a correct Users id or email"



