{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}

module Mongo where

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.IO.Class  (liftIO)
--import Control.Monad.Trans (liftIO)
import GHC.Generics
import Data.Dates

run :: Action IO () -> IO ()
run act = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "communis_db" act
    close pipe
    print e

data User = User {
    email :: String,
    password :: String,
    alias :: String,
    image_url :: String,
    show_email :: Bool,
    date :: DateTime
    } deriving (Show)

-- insert_user :: User -> Action IO []
-- insert_user u = insert "User" u


clearTeams :: Action IO ()
clearTeams = delete (select [] "team")

insertTeams :: Action IO [Value]
insertTeams = insertMany "team" [
    ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
    ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
    ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
    ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}

nationalLeagueTeams :: Action IO [Document]
nationalLeagueTeams = rest =<< find (select ["league" =: "National"] "team")

newYorkTeams :: Action IO [Document]
newYorkTeams = rest =<< find (select ["home.state" =: "NY"] "team") {project = ["name" =: 1, "league" =: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs


-- data Post = Post {
--     atom :: Int,
--     material :: String,
--     processing String
--     params String
--     image_url String
--     reference String
--     owner UsersId
--     material_url String
--     date UTCTime
-- }
-- Comment
--     owner UsersId
--     post PostId
--     date UTCTime default=CURRENT_TIMESTAMP
--     text String
--     deriving Show


-- run :: Action IO ()
-- run = do
--     clearTeams
--     insertTeams
--     allTeams >>= printDocs "All Teams"
--     nationalLeagueTeams >>= printDocs "National League Teams"
--     newYorkTeams >>= printDocs "New York Teams"
