{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.TaskSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                         (def)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import           Control.Monad.Identity               (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class           (tell)
import           Data.Time.Clock                      (UTCTime)

import           Data.Domain.CalendarEntry            as CalendarEntry
import           Data.Domain.Task                     as Task

import           Data.Repository.CalendarRepo         (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo             (MonadDBTaskRepo)
import           Data.Service.TelegramTasks (TelegramTasksAssignmentService)

import qualified Data.Service.Task                    as TaskService


dbDateTaskStart = read "2011-11-19 20:00:00.000000 UTC"::UTCTime
dbDateTaskEnd = read "2011-11-20 10:00:00.000000 UTC"::UTCTime
mkFixture "Fixture" [ts| TelegramTasksAssignmentService, MonadDBTaskRepo, MonadDBCalendarRepo |]
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
taskFromDb = def{ Task.title="A", Task.description=Just "task1", taskId=5, startTime=Just dbDateTaskStart, endTime=Just dbDateTaskEnd, Task.owner=10}
entryFromDb = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=10,
        CalendarEntry.startDate=dbDate, CalendarEntry.endDate=dbDate,
        CalendarEntry.tasks = [1, 2]}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _addTaskToCalendarEntry = \entry taskId -> tell [show entry] >> tell [show taskId] >>= (\_ -> return $ Right entry)
                  , _updateTask = \a -> tell [show a] >>= (\_ -> return $ Right a)
                  , _deleteTask = \a -> tell [show a]
                  , _createTask = \a -> tell [show a] >>= (\_ -> return a)
                  , _findTaskById = \a -> tell [show a] >>= (\_ -> return $ Just taskFromDb)
                  , _deleteTaskFromCalendarEntry = \c i -> tell [show c] >> tell [show i] >>= (\_ -> return $ Right undefined)
                  , _deleteTaskFromAllTelegramLinks = \a -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "TaskServiceSpec" $ do
    it "deleteTaskAndCascade" $ do
        let task = def{ Task.title="A", Task.description=Just "task1", taskId=1, assignedTelegramLinks=[7], startTime=Nothing, endTime=Nothing, Task.owner=10}
        let (_, log) = evalTestFixture (TaskService.deleteTaskAndCascadeImpl entryFromDb task) fixture
        length log `shouldBe` 4
        -- calendarrepo calls
        log!!0 `shouldBe` show entryFromDb
        log!!1 `shouldBe` show (Task.taskId task)
        -- UserTasksService calls
        log!!2 `shouldBe` show task
        -- TaskRepo calls
        log!!3 `shouldBe` show task
    it "createTaskInCalendar" $ do
        let newDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
        let calc = def{ CalendarEntry.title = "A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=2,
            startDate=newDate, endDate=newDate}
        let newTask = def{ Task.title="A", Task.description=Just "task1", taskId=1, assignedTelegramLinks=[7], startTime=Nothing, endTime=Nothing, Task.owner = 2}
        let (result, log) = evalTestFixture (TaskService.createTaskInCalendarImpl calc newTask) fixture
        length log `shouldBe` 3
        result `shouldBe` newTask
        log!!0 `shouldBe` show newTask {Task.owner = CalendarEntry.owner calc}
        log!!1 `shouldBe` show calc
        log!!2 `shouldBe` show (Task.taskId newTask)
    it "createTasksInCalendar" $ do
        let newDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
        let calc = def{ CalendarEntry.title = "A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=2,
            startDate=newDate, endDate=newDate}
        let newTask1 = def{ Task.title="A", Task.description=Just "task1", taskId=1, assignedTelegramLinks=[7], startTime=Nothing, endTime=Nothing, Task.owner = 2}
        let newTask2 = def{ Task.title="B", Task.description= Nothing, taskId=2, startTime=Nothing, endTime=Nothing, Task.owner = 2}
        let (result, log) = evalTestFixture (TaskService.createTasksInCalendarImpl calc [newTask1,newTask2]) fixture
        length log `shouldBe` 6
        result `shouldBe` [newTask1, newTask2]
        log!!0 `shouldBe` show newTask1 {Task.owner = CalendarEntry.owner calc}
        log!!1 `shouldBe` show calc
        log!!2 `shouldBe` show (Task.taskId newTask1)
        log!!3 `shouldBe` show newTask2 {Task.owner = CalendarEntry.owner calc}
        log!!4 `shouldBe` show calc
        log!!5 `shouldBe` show (Task.taskId newTask2)
    it "updateTasksDay" $ do
        let (_, log) = evalTestFixture (TaskService.updateTasksDayImpl taskFromDb 62) fixture
        let updateTask = taskFromDb {startTime=Just (read "2012-01-20 20:00:00.000000 UTC"::UTCTime), endTime=Just (read "2012-01-21 10:00:00.000000 UTC"::UTCTime)}
        length log `shouldBe` 1
        assertEqual "Nach falscher TelegramLink-Id gesucht" (show updateTask) (head log)
