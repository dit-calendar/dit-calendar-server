{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Data.Domain.CalendarEntry
    ( CalendarEntry(..)
    ) where

import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.IxSet        (Indexable (..), IxSet, empty, ixFun, ixSet)
import           Data.SafeCopy     (base, deriveSafeCopy)

import           Data.Domain.Task  (Task)
import           Data.Domain.Types (Description, EndDate, Entity (..), EntryId,
                                    StartDate, Title, UserId, UserIdIndex (..))

data CalendarEntry = CalendarEntry
    { title       :: Title
    , description :: Maybe Description
    , entryId     :: EntryId
    , version     :: Int
    , owner       :: UserId
    , tasks       :: IxSet Task
    , startDate   :: StartDate
    , endDate     :: EndDate
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CalendarEntry)

instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ]
                , ixFun $ \bp -> [ UserIdIndex $ owner bp ]
                , ixFun $ \bp -> [ startDate bp]
                , ixFun $ \bp -> [ endDate bp]
                ]

instance Entity CalendarEntry where
    setId calendarEntry newId = calendarEntry {entryId = newId}
    getId = entryId
    setVersion calendarEntry newVersion = calendarEntry {version = newVersion}
    getVersion = version
    getUsersAccessRestriction a = [owner a]

instance Default CalendarEntry where
    def = CalendarEntry {entryId = -1, version = 0, tasks = empty, owner = -1}
