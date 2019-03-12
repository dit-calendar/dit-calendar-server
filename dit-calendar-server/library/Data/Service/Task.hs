{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Service.Task
    ( deleteTaskAndCascadeImpl, createTaskInCalendarImpl, updateTaskInCalendarImpl, addUserToTaskImpl, removeUserFromTaskImpl, TaskService(..) ) where

import           Control.Monad.IO.Class
import           Data.Generics.Aliases        (orElse)
import           Data.List                    (delete)
import           Data.Maybe                   (fromJust)

import           AcidHelper                   (App)
import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Domain.Types            (Description, EitherResponse,
                                               UserId)
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import qualified Data.Repository.CalendarRepo as MonadDBCalendarRepo
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import qualified Data.Repository.TaskRepo     as TaskRepo
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import qualified Data.Repository.UserRepo     as MonadDBUserRepo


deleteTaskAndCascadeImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadIO m, MonadDBCalendarRepo m) => CalendarEntry -> Task -> m ()
deleteTaskAndCascadeImpl calendar task = do
    MonadDBCalendarRepo.deleteTaskFromCalendarEntry calendar (taskId task)
    deleteTaskFromAllUsers task
    TaskRepo.deleteTask $ Task.taskId task

createTaskInCalendarImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m, MonadDBCalendarRepo m) =>
            CalendarEntry -> Task -> m Task
createTaskInCalendarImpl calendarEntry task = do
    mTask <- TaskRepo.createTask task
    MonadDBCalendarRepo.addTaskToCalendarEntry calendarEntry (Task.taskId mTask)
    return mTask

updateTaskInCalendarImpl :: MonadDBTaskRepo m => Task -> m (EitherResponse Task)
updateTaskInCalendarImpl = TaskRepo.updateTask

deleteTaskFromAllUsers :: (MonadDBUserRepo m, MonadIO m) =>
                        Task -> m ()
deleteTaskFromAllUsers task =
    foldr (\ x ->
      (>>) (do
        user <- MonadDBUserRepo.findUserById x
        MonadDBUserRepo.deleteTaskFromUser (fromJust user) x ))
    (return ()) $ Task.belongingUsers task

addUserToTaskImpl :: (MonadDBUserRepo m, MonadDBTaskRepo m, MonadIO m) =>
                Task -> User -> m (EitherResponse Task)
addUserToTaskImpl task user =
    if taskId task `elem` belongingUsers task
        then return (Right task) -- do nothing and return same task
        else do
            MonadDBUserRepo.addTaskToUser user (taskId task)
            TaskRepo.updateTask task {belongingUsers = User.userId user : belongingUsers task}

removeUserFromTaskImpl :: (MonadDBTaskRepo m, MonadDBUserRepo m) =>
                    Task -> User -> m (EitherResponse Task)
removeUserFromTaskImpl task user = do
    MonadDBUserRepo.deleteTaskFromUser user (taskId task)
    TaskRepo.updateTask task {belongingUsers = delete (User.userId user) (belongingUsers task)}

class TaskService m where
    deleteTaskAndCascade :: CalendarEntry -> Task -> m ()
    createTaskInCalendar :: CalendarEntry -> Task -> m Task
    updateTaskInCalendar :: Task -> m (EitherResponse Task)
    addUserToTask :: Task -> User -> m (EitherResponse Task)
    removeUserFromTask :: Task -> User -> m (EitherResponse Task)

instance (MonadDBTaskRepo App, MonadDBUserRepo App, MonadDBCalendarRepo App)
            => TaskService App where
    deleteTaskAndCascade = deleteTaskAndCascadeImpl
    createTaskInCalendar = createTaskInCalendarImpl
    updateTaskInCalendar = updateTaskInCalendarImpl
    addUserToTask = addUserToTaskImpl
    removeUserFromTask = removeUserFromTaskImpl