{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Repository.Acid.CalendarEntry
    ( CalendarDAO(..), initialEntryListState, EntryList(..), NewEntry(..), EntryById(..), AllEntrys(..),
    AllEntriesForUser(..), AllEntriesForUserAndRange(..), GetEntryList(..), UpdateEntry(..), DeleteEntry(..), AddTaskToEntry(..) ) where

import           Control.Monad.Reader               (asks)
import           Control.Monad.State                (get, put)
import           Data.Acid                          (Query, Update, makeAcidic)
import           Data.IxSet                         (Indexable (..), Proxy (..),
                                                     getEQ, getOne, getRange,
                                                     ixFun, ixSet, toDescList,
                                                     toList, updateIx)

import           Data.Domain.CalendarEntry          (CalendarEntry (..))
import           Data.Domain.Types                  (EitherResult, EntryId,
                                                     StartDate, TaskId,
                                                     UserIdIndex (..))

import qualified Data.Domain.User                   as User
import           Data.Maybe                         (fromJust)
import           Data.Repository.Acid.InterfaceAcid (incVersion)
import qualified Data.Repository.Acid.InterfaceAcid as InterfaceAcid


instance Indexable CalendarEntry where
  empty = ixSet [ ixFun $ \bp -> [ entryId bp ]
                , ixFun $ \bp -> [ UserIdIndex $ owner bp ]
                , ixFun $ \bp -> [ startDate bp]
                , ixFun $ \bp -> [ endDate bp]
                ]

type EntryList = InterfaceAcid.EntrySet CalendarEntry EntryId

initialEntryListState :: EntryList
initialEntryListState = InterfaceAcid.initialState 1

getEntryList :: Query EntryList EntryList
getEntryList = InterfaceAcid.getEntrySet

allEntrys :: Query EntryList [CalendarEntry]
allEntrys = InterfaceAcid.allEntrysAsList

--TODO suche nach mit Predicat? Für suche mit zeitlichen Grenzen
allEntriesForUser :: User.User -> Query EntryList [CalendarEntry]
allEntriesForUser user = asks (toDescList (Proxy :: Proxy StartDate) . getEQ (UserIdIndex $ User.userId user) . InterfaceAcid.entrys)

allEntriesForUserAndRange :: User.User -> StartDate -> StartDate -> Query EntryList [CalendarEntry]
allEntriesForUserAndRange user from to = asks (toList . getRange from to . getEQ (UserIdIndex $ User.userId user) . InterfaceAcid.entrys)

newEntry :: CalendarEntry -> Update EntryList CalendarEntry
newEntry = InterfaceAcid.newEntry

entryById :: EntryId -> Query EntryList (Maybe CalendarEntry)
entryById = InterfaceAcid.entryById

updateEntry :: CalendarEntry -> Update EntryList (EitherResult CalendarEntry)
updateEntry = InterfaceAcid.updateEntry

--Don't increment version when updating calendarEntry
addTaskToEntry :: EntryId -> TaskId -> Update EntryList (EitherResult CalendarEntry)
addTaskToEntry calendarId taskId = do
     b@InterfaceAcid.EntrySet{..} <- get
     let dbEntry = fromJust $ getOne (getEQ calendarId entrys)
     let incrementEntry = incVersion dbEntry in
         do  put b { InterfaceAcid.entrys =
                     updateIx calendarId (incrementEntry {tasks = taskId : tasks incrementEntry}) entrys
                 }
             return $ Right incrementEntry

deleteEntry :: EntryId -> Update EntryList ()
deleteEntry = InterfaceAcid.deleteEntry

$(makeAcidic ''EntryList ['newEntry, 'entryById, 'allEntrys, 'getEntryList, 'allEntriesForUser, 'allEntriesForUserAndRange, 'updateEntry, 'deleteEntry, 'addTaskToEntry])

class Monad m => CalendarDAO m where
    create :: NewEntry -> m CalendarEntry
    update :: UpdateEntry -> m (EitherResult CalendarEntry)
    addTask :: AddTaskToEntry -> m (EitherResult CalendarEntry)
    delete :: DeleteEntry -> m ()
    query  :: EntryById -> m (Maybe CalendarEntry)
    findList   :: AllEntriesForUser -> m [CalendarEntry]
    findListForRange   :: AllEntriesForUserAndRange -> m [CalendarEntry]
