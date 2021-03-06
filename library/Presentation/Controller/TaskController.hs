module Presentation.Controller.TaskController where

import           AppContext                     (App)
import           Data.Domain.Types              (EitherResult, EntryId, TaskId)
import           Data.Domain.User               as DomainUser (User (..))
import           Presentation.Dto.Task          as TaskDto (Task (..))
import           Presentation.Mapper.BaseMapper (transformToDtoE,
                                                 transformToDtoList)
import           Presentation.Mapper.TaskMapper (transformFromDto,
                                                 transformToDto)
import           Server.ResponseBuilder         (onEntryExist, onTaskExist)

import qualified Data.Repository.CalendarRepo   as CalendarRepo
import qualified Data.Service.CalendarEntry     as CalendarService
import qualified Data.Service.CalendarTasks     as CalendarTasks
import qualified Data.Service.Task              as TaskService


--handler for taskPage
taskPage :: TaskId -> DomainUser.User -> App (EitherResult TaskDto.Task)
taskPage i _ = onTaskExist i (return . Right . transformToDto)

calendarTasks :: EntryId -> DomainUser.User -> App (EitherResult [TaskDto.Task])
calendarTasks entryId user = onEntryExist entryId getTasks
    where
        getTasks cEntry = do
              result <- CalendarTasks.getCalendarTasks cEntry
              return $ Right (transformToDtoList result)

createTasks :: EntryId -> [TaskDto.Task] -> DomainUser.User -> App (EitherResult [TaskDto.Task])
createTasks calendarId taskDtos _ =
    let tasks = map (`transformFromDto` Nothing) taskDtos in
    onEntryExist calendarId
        (\e -> do
            result <- TaskService.createTasksInCalendar e tasks
            return $ Right $ transformToDtoList result)

updateTask :: TaskId -> TaskDto.Task -> DomainUser.User -> App (EitherResult TaskDto.Task)
updateTask id taskDto loggedUser =
    onTaskExist id (\t -> do
        result <- TaskService.updateTaskInCalendar $ transformFromDto taskDto (Just t)
        return $ transformToDtoE result)

deleteTask :: EntryId -> TaskId -> DomainUser.User -> App (EitherResult ())
deleteTask entryId taskId loggedUser =
    onEntryExist entryId
        (\entry ->
        onTaskExist taskId (\task -> do
            TaskService.deleteTaskAndCascade entry task
            return $ Right ()))
