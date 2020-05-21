{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarTasks (CalendarTasksService(..), getCalendarTasksIml, deleteCalendarsTasksImpl) where

import           Data.Maybe                (fromJust)
import           Data.IxSet                (toSet, toList)

import           AppContext                (App)
import           Data.Domain.CalendarEntry as CalendarEntry
import           Data.Domain.Task          as Task
import           Data.Domain.Types         (TaskId)
import           Data.Repository.TaskRepo  (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo  as MonadDBTaskRepo

deleteCalendarsTasksImpl :: MonadDBTaskRepo m => CalendarEntry -> m ()
deleteCalendarsTasksImpl calendar =
    foldr ((>>) . MonadDBTaskRepo.deleteTask) (return ()) $ toSet (CalendarEntry.tasks calendar)

getCalendarTasksIml :: MonadDBTaskRepo m => CalendarEntry -> m [Task]
getCalendarTasksIml calendar = mapM getTaskWithFail (toList $ CalendarEntry.tasks calendar)

-- https://en.wikibooks.org/wiki/Haskell/do_notation#The_fail_method
getTaskWithFail :: (MonadDBTaskRepo m) => Task -> m Task
getTaskWithFail task = do
    Just task <- MonadDBTaskRepo.findTaskById $ taskId task
    return task

class Monad m => CalendarTasksService m where
    getCalendarTasks :: CalendarEntry -> m [Task]
    deleteCalendarsTasks :: CalendarEntry -> m ()

instance MonadDBTaskRepo App => CalendarTasksService App where
    getCalendarTasks = getCalendarTasksIml
    deleteCalendarsTasks = deleteCalendarsTasksImpl
