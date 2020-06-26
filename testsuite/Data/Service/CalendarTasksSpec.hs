{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.CalendarTasksSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Control.Monad.Writer.Class   (tell)
import           Data.Default                 (def)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import           Data.Time.Clock              (UTCTime)

import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.Task             as Task
import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.TaskRepo     (MonadDBTaskRepo)
import           Data.Service.Task            (TaskService)

import qualified Data.Service.CalendarTasks   as CalendarTasks


mkFixture "Fixture" [ts| TaskService, MonadDBCalendarRepo, MonadDBTaskRepo |]

taskFromDb1 = def { Task.title="A", Task.description=Just "task1", taskId=1, startTime=Nothing, endTime=Nothing, Task.owner=10}
taskFromDb2 = def { Task.title="A", Task.description=Just "task2", taskId=2, startTime=Nothing, endTime=Nothing, Task.owner=10}
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
entryFromDb = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=10, tasks=[1,2],
        startDate=dbDate, endDate=dbDate}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _findTaskById = \a -> return $ Just (if a == 1 then taskFromDb1 else taskFromDb2)
                    ,_deleteTask = \a -> tell [show a]
                    ,_findCalendarById = \a -> return $ Just entryFromDb
                    ,_updateCalendar = return . Right
                    ,_updateTask = \a -> tell [show a] >>= (\_ -> return $ Right a)
                    ,_updateTasksDay= \a b -> tell [show a] >> tell [show b] >>= (\_ -> return $ Right a)
                  }


spec = describe "CalendarEntryServiceSpec" $ do
    it "deleteCalendarsTasks" $ do
        let (_, log) = evalTestFixture (CalendarTasks.deleteCalendarsTasksImpl entryFromDb) fixture
        -- Test TaskRepo calls
        length log `shouldBe` 2
        log!!0 `shouldBe` show taskFromDb1
        log!!1 `shouldBe` show taskFromDb2
    it "getTasks" $ do
        let (result, _) = evalTestFixture (CalendarTasks.getCalendarTasksIml entryFromDb) fixture
        length result `shouldBe` 2
    it "updatCalendarTaskWithOldDate" $ do
        let (result, log) = evalTestFixture (CalendarTasks.updateCalendarAndTasksImpl entryFromDb) fixture
        length log `shouldBe` 0
    it "updatCalendarTaskWithNewDate" $ do
        let entryWithNewDate = entryFromDb {startDate = read "2012-01-20 18:28:52.607875 UTC"::UTCTime}
        let (_, log) = evalTestFixture (CalendarTasks.updateCalendarAndTasksImpl entryWithNewDate) fixture
        length log `shouldBe` 4
        assertEqual "falscher task upda" (show taskFromDb1) (head log)
        assertEqual "update task with wrong day difference" "62" (log!!1)
        assertEqual "falscher task upda" (show taskFromDb2) (log!!2)
        assertEqual "update task with wrong day difference" "62" (log!!3)
