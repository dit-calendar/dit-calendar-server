{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.CalendarTasks (CalendarTasksService(..), getCalendarTasksIml, deleteCalendarsTasksImpl, updateCalendarAndTasksImpl) where

import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.Time                    (utctDay)

import           AppContext                   (App)
import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EitherResult, TaskId)
import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as CalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as MonadDBTaskRepo
import qualified Data.Service.Task            as TaskService
import           Data.Time.Calendar           (diffDays)

deleteCalendarsTasksImpl :: MonadDBTaskRepo m => CalendarEntry -> m ()
deleteCalendarsTasksImpl calendar =
    foldr (\ x ->
      (>>) (do
        task <- MonadDBTaskRepo.findTaskById x
        MonadDBTaskRepo.deleteTask (fromJust task) ))
    (return ()) $ CalendarEntry.tasks calendar

getCalendarTasksIml :: MonadDBTaskRepo m => CalendarEntry -> m [Task]
getCalendarTasksIml calendar = mapM getTaskWithFail (CalendarEntry.tasks calendar)

getTaskWithFail :: (MonadDBTaskRepo m) => TaskId -> m Task
getTaskWithFail taskId = fmap fromJust (MonadDBTaskRepo.findTaskById taskId)

updateCalendarAndTasksImpl :: (MonadDBCalendarRepo m, MonadDBTaskRepo m, TaskService.TaskService m)
    => CalendarEntry -> m (EitherResult CalendarEntry)
updateCalendarAndTasksImpl calendarEntry = do
    dbEntry <- CalendarRepo.findCalendarById (entryId calendarEntry)
    if startDate (fromJust dbEntry) == startDate calendarEntry
    then CalendarRepo.updateCalendar calendarEntry
    else do
        response <- CalendarRepo.updateCalendar calendarEntry
        case response of
            Left _ -> return response
            Right _ -> let daysDiff = diffDays (utctDay $ startDate calendarEntry) (utctDay $ startDate $ fromJust dbEntry) in
                        do
                            updateTasksOfCalendar calendarEntry daysDiff
                            return response

updateTasksOfCalendar :: (MonadDBTaskRepo m, TaskService.TaskService m) => CalendarEntry -> Integer -> m ()
updateTasksOfCalendar calendarEntry dayDiff =
    foldr (\taskId ->
        (>>) (do
            mTask <- MonadDBTaskRepo.findTaskById taskId
            case mTask of
                Just task -> TaskService.updateTasksDay task dayDiff
                Nothing   -> fail "internal error. Task should exist"
                )
        ) (return ()) $ tasks calendarEntry

class Monad m => CalendarTasksService m where
    getCalendarTasks :: CalendarEntry -> m [Task]
    deleteCalendarsTasks :: CalendarEntry -> m ()
    updateCalendarAndTasks :: CalendarEntry -> m (EitherResult CalendarEntry)

instance MonadDBTaskRepo App => CalendarTasksService App where
    getCalendarTasks = getCalendarTasksIml
    deleteCalendarsTasks = deleteCalendarsTasksImpl
    updateCalendarAndTasks = updateCalendarAndTasksImpl
