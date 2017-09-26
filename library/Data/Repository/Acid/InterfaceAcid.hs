{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}

module Data.Repository.Acid.InterfaceAcid where

import Control.Applicative             ( (<$>) )
import Control.Monad.Reader            ( ask )
import Control.Monad.State             ( get, put )
import Data.Data                       ( Data, Typeable )
import Data.Acid                       ( Query, Update )
import Data.SafeCopy                   ( base, deriveSafeCopy )
import Data.IxSet                      ( Indexable(..), IxSet(..), getOne, insert
                                       , toList, getEQ, deleteIx, updateIx )

import Data.Domain.Types     ( Entry, setId, getId )


--type that represents the state we wish to store
data (Typeable a, Ord a, Indexable a) => EntrySet a = EntrySet
    { nextEntryId           :: Int
    , entrys                :: IxSet a
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''EntrySet)

initialState :: (Ord a, Typeable a, Indexable a) => EntrySet a
initialState =
    EntrySet { 
        nextEntryId              = 1
        , entrys                 = empty
        }

getEntrySet :: (Ord a, Indexable a) => Query (EntrySet a) (EntrySet a)
getEntrySet = ask

allEntrysAsList :: (Ord a, Typeable a, Indexable a) => Query (EntrySet a) [a]
allEntrysAsList = toList . entrys <$> ask

entryById :: (Ord a, Typeable a, Indexable a) => Int -> Query (EntrySet a) (Maybe a)
entryById eid = getOne . getEQ eid . entrys <$> ask
            
deleteEntry :: (Ord a, Typeable a, Indexable a) => Int -> Update (EntrySet a) ()
deleteEntry entryToDelete =
    do  b@EntrySet{..} <- get
        put $ b { entrys =
            deleteIx entryToDelete entrys
            }

updateEntry :: (Ord a, Typeable a, Indexable a, Entry a) => a -> Update (EntrySet a) ()
updateEntry updatedEntry =
     do b@EntrySet{..} <- get
        put $ b { entrys =
            updateIx (getId updatedEntry) updatedEntry entrys
            }

-- create a new entry and add it to the database
newEntry :: (Ord a, Typeable a, Indexable a, Entry a) => a -> Update (EntrySet a) a
newEntry entry =
    do  b@EntrySet{..} <- get
        let nEntry = setId entry nextEntryId
        --Because UserId is an instance of Enum we can use succ to increment it.
        put $ b { nextEntryId = succ nextEntryId
                , entrys      = insert nEntry entrys
                }
        return nEntry