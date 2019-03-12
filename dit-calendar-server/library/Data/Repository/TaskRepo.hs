{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Repository.TaskRepo
    ( deleteTaskImpl, createTaskImpl, updateTaskImpl, findTaskByIdImpl, MonadDBTaskRepo(..) ) where

import           Control.Monad.IO.Class
import           Data.Default              (def)

import qualified Happstack.Foundation      as Foundation

import           AcidHelper                (App)
import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (Description, EitherResponse, TaskId)
import           Data.Repository.Acid.Task (TaskDAO (..))

import qualified Data.Repository.Acid.Task as TaskAcid

instance TaskDAO App where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query

updateTaskImpl :: TaskDAO m => Task -> m (EitherResponse Task)
updateTaskImpl = update . TaskAcid.UpdateTask

deleteTaskImpl :: TaskDAO m => TaskId -> m ()
deleteTaskImpl = delete . TaskAcid.DeleteTask

createTaskImpl :: TaskDAO m => Task -> m Task
createTaskImpl = create . TaskAcid.NewTask

findTaskByIdImpl :: (TaskDAO m, MonadIO m) => TaskId -> m (Maybe Task)
findTaskByIdImpl = query . TaskAcid.TaskById


class (Monad m, TaskDAO App) => MonadDBTaskRepo m where
    createTask        :: Task -> m Task
    findTaskById      :: TaskId -> m (Maybe Task)
    updateTask        :: Task   -> m (EitherResponse Task)
    deleteTask        :: TaskId   -> m ()

instance MonadDBTaskRepo App where
    createTask        = createTaskImpl
    findTaskById      = findTaskByIdImpl
    updateTask        = updateTaskImpl
    deleteTask        = deleteTaskImpl