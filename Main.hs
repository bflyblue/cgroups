{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty as Scotty
import Network.HTTP.Types

import Data.Aeson

import Control.Applicative
import Control.Monad.Morph

import Data.Foldable
import Data.Monoid ((<>))

import Cgroup
import Resource

main :: IO ()
main = do
    scotty 3000 cgroupApi
    return ()

cgroupApi :: ScottyM ()
cgroupApi = do
    get "/cgroups" listCgroups
    get (regex "^/cgroup(/.*)$") getCgroup
    put (regex "^/cgroup(/.*)$") putTask

listCgroups :: ActionM ()
listCgroups = do
    t <- lift $ listControlGroups root
    Scotty.json [Ref n (mkUri n) | ControlGroupName n <- toList t]
    where
        mkUri = ("/cgroup" <>)

getCgroup :: ActionM ()
getCgroup = do
    grp <- ControlGroupName <$> param "1"
    mcgrp <- lift $ getControlGroup grp
    case mcgrp of
        Just cgrp -> Scotty.json cgrp
        Nothing -> do
            status notFound404
            Scotty.json $ object [ "error" .= ("not found" :: String) ]

putTask :: ActionM ()
putTask = do
    grp <- ControlGroupName <$> param "1"
    task <- jsonData
    b <- lift $ setTask grp task
    if b
    then
        Scotty.json $ object [ "status" .= ("success" :: String) ]
    else do
        status notFound404
        Scotty.json $ object [ "error" .= ("not found" :: String) ]
