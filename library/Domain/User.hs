{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.User where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )

import Domain.Types             ( UserId, EntryId )

data User = User { name :: String, userId :: UserId, calendarEntrys :: [EntryId] }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)