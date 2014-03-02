{-# LANGUAGE OverloadedStrings #-}

module Cgroup where

import Resource

import Control.Applicative
import Control.Monad

import Data.Tree
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Monoid

import System.Directory
import System.FilePath

-- Heirarchy to expose, use cpuset as an example
heir :: String
heir = "cpuset"

-- Top level cgroup name
root :: ControlGroupName
root = ControlGroupName "/"

-- Build filepath to /sys entry for cgroup name
cgrpPath :: ControlGroupName -> FilePath
cgrpPath (ControlGroupName name) = "/sys/fs/cgroup" </> heir </> (rel . path) name
    where rel = dropWhile isPathSeparator
          path = joinPath . splitOn "/"

-- Tasks file for cgroup
tasksPath :: ControlGroupName -> FilePath
tasksPath grp = cgrpPath grp </> "tasks"

-- Test if a directory looks like a cgroup
isCgroup :: ControlGroupName -> IO Bool
isCgroup grp = do
    isDir <- doesDirectoryExist (cgrpPath grp)
    hasTasks <- doesFileExist (tasksPath grp)
    return $ isDir && hasTasks

-- Returns Tree of cgroups by inspecting /sys entries
listControlGroups :: ControlGroupName -> IO (Tree ControlGroupName)
listControlGroups = unfoldTreeM groups
    where
        groups grp = do
            entries <- filter (not . hidden) <$> getDirectoryContents path
            subgroups <- filterM isCgroup $ map ((grp <>) . ControlGroupName) entries
            return (grp, subgroups)
            where
                hidden = ("." `isPrefixOf`)
                path = cgrpPath grp

-- Get Just Tasks for a cgroup, Nothing if invalid group
getControlGroup :: ControlGroupName -> IO (Maybe ControlGroup)
getControlGroup grp = do
    let path = cgrpPath grp
    isGrp <- isCgroup grp
    if isGrp then do
        tasks <- map (Task . TaskId) . lines <$> readFile (path </> "tasks")
        return $ Just $ ControlGroup grp tasks
    else
        return Nothing

-- Assign a Task to a cgroup
-- TODO: capture exceptions
setTask :: ControlGroupName -> Task -> IO Bool
setTask grp (Task (TaskId pid)) = do
    isGrp <- isCgroup grp
    if isGrp
    then do
        writeFile (tasksPath grp) pid
        return True
    else
        return False
