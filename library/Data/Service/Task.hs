{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.Task
    ( updateTasksDayImpl, createTasksInCalendarImpl, deleteTaskAndCascadeImpl, createTaskInCalendarImpl, updateTaskInCalendarImpl, TaskService(..) ) where

import           Control.Monad.IO.Class
import           Data.Time.Calendar           (addDays)
import           Data.Time.Clock              (utctDay)

import           AppContext                   (App)
import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (EitherResult)

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as TaskRepo
import           Data.Service.TelegramTasks   (TelegramTasksAssignmentService)
import qualified Data.Service.TelegramTasks   as TelegramTasksAssignmentService


deleteTaskAndCascadeImpl :: (MonadDBTaskRepo m, TelegramTasksAssignmentService m, MonadIO m, MonadDBCalendarRepo m) => CalendarEntry -> Task -> m ()
deleteTaskAndCascadeImpl calendar task = do
    MonadDBCalendarRepo.deleteTaskFromCalendarEntry calendar (taskId task)
    TelegramTasksAssignmentService.deleteTaskFromAllTelegramLinks task
    TaskRepo.deleteTask task

createTaskInCalendarImpl :: (MonadDBTaskRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> Task -> m Task
createTaskInCalendarImpl calendarEntry task = do
    mTask <- TaskRepo.createTask newTaskWithUser
    MonadDBCalendarRepo.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask
    where newTaskWithUser = task {Task.owner = CalendarEntry.owner calendarEntry }

createTasksInCalendarImpl :: (MonadDBTaskRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> [Task] -> m [Task]
createTasksInCalendarImpl calendarEntry = mapM (createTaskInCalendarImpl calendarEntry)

updateTaskInCalendarImpl :: MonadDBTaskRepo m => Task -> m (EitherResult Task)
updateTaskInCalendarImpl = TaskRepo.updateTask

updateTasksDayImpl :: MonadDBTaskRepo m => Task -> Integer -> m (EitherResult Task)
updateTasksDayImpl task dayDifference =
    case Task.startTime task of
        Just startTime -> TaskRepo.updateTask (updateTasksTime task dayDifference)
        Nothing -> return $ Right task

updateTasksTime :: Task -> Integer -> Task
updateTasksTime task dayDifference =
    case Task.startTime task of
        Just startTime ->
            let newStartTime = updateTime startTime dayDifference in
            case Task.endTime task of
                Just endTime ->
                    let newEndTime = updateTime endTime dayDifference in
                    task {Task.startTime = Just newStartTime, Task.endTime = Just newEndTime}
                Nothing -> task {Task.startTime = Just newStartTime}
        Nothing -> task

updateTime oldTime dayDifference = oldTime {utctDay = addDays dayDifference (utctDay oldTime)}

class Monad m => TaskService m where
    deleteTaskAndCascade :: CalendarEntry -> Task -> m ()
    createTaskInCalendar :: CalendarEntry -> Task -> m Task
    createTasksInCalendar :: CalendarEntry -> [Task] -> m [Task]
    updateTaskInCalendar :: Task -> m (EitherResult Task)
    updateTasksDay :: Task -> Integer -> m (EitherResult Task)

instance (MonadDBTaskRepo App, MonadDBCalendarRepo App) => TaskService App where
    deleteTaskAndCascade = deleteTaskAndCascadeImpl
    createTaskInCalendar = createTaskInCalendarImpl
    createTasksInCalendar = createTasksInCalendarImpl
    updateTaskInCalendar = updateTaskInCalendarImpl
    updateTasksDay = updateTasksDayImpl
