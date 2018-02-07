{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-}

module Data.Service.MonadDB.Task where

import Control.Monad.IO.Class

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.Task           ( Task(..) )
import Data.Domain.Types          ( TaskId )
import Data.Repository.Acid.MonadDB.CalendarEntry    ( MonadDBCalendar )
import Data.Repository.Acid.MonadDB.Task             ( MonadDBTask )
import Data.Repository.Acid.MonadDB.User             ( MonadDBUser )

import qualified Data.Repository.TaskRepo   as TaskRepo

class Monad m => MonadDBTaskService m where
    updateTask        :: Task   -> m ()
    deleteTask        :: Task   -> m ()
    createTask        :: String -> m Task
    getTask           :: TaskId -> m Task

instance (MonadDBUser CtrlV', MonadDBTask CtrlV', MonadDBCalendar CtrlV')
        => MonadDBTaskService CtrlV' where
    updateTask        = TaskRepo.updateTask
    deleteTask        = TaskRepo.deleteTask
    createTask        = TaskRepo.createTask
    getTask           = TaskRepo.getTask