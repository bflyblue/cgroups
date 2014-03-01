{-# LANGUAGE DeriveGeneric #-}

module Resource where

import GHC.Generics
import Data.Aeson
import Data.Monoid

newtype ControlGroupName = ControlGroupName String
    deriving (Show, Generic)

instance Monoid ControlGroupName where
    mempty = ControlGroupName mempty
    ControlGroupName a `mappend` ControlGroupName b = ControlGroupName $ dedup $ a <> "/" <> b
        where dedup ('/':'/':r) = '/' : dedup r
              dedup (h : t)     = h : dedup t
              dedup []          = []

instance FromJSON ControlGroupName
instance ToJSON ControlGroupName

newtype TaskId = TaskId String
    deriving (Show, Generic)

instance FromJSON TaskId
instance ToJSON TaskId

data ControlGroup = ControlGroup
    { cgrpName      :: ControlGroupName
    , cgrpTasks     :: [Task]
    }
    deriving (Show, Generic)

instance FromJSON ControlGroup
instance ToJSON ControlGroup

data Task = Task
    { taskId        :: TaskId
    }
    deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

data Ref a = Ref
    { resource      :: a
    , uri           :: String
    }
    deriving (Show, Generic)

instance ToJSON a => ToJSON (Ref a)
