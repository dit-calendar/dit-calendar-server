module Presentation.Route.TaskRoute
    ( routeTask
    , routeTaskDetail
    , routeTaskWithUser
    ) where

import           Data.Aeson                             (eitherDecode)
import           Happstack.Server                       (Method (DELETE, GET, POST, PUT),
                                                         Response, look)

import           Auth.Authorization                     (callIfAuthorized)
import           Data.Domain.Types                      (EntryId, TaskId,
                                                         UserId)
import           AcidHelper                (App)
import           Presentation.Dto.Task                  as TaskDto (Task (..))
import           Presentation.HttpServerHelper          (getBody, getHttpMethod)
import           Presentation.ResponseHelper            (badRequest,
                                                         notImplemented)

import qualified Presentation.Controller.TaskController as TaskController


routeTask :: EntryId -> App Response
routeTask entryId = do
    m <- getHttpMethod
    case m of
        POST -> do
            body <- getBody
            case eitherDecode body :: Either String TaskDto.Task of
                 Right taskDto ->
                      TaskController.createTask entryId taskDto
                 Left errorMessage -> badRequest errorMessage
        other -> notImplemented other

routeTaskDetail :: EntryId ->  TaskId -> App Response
routeTaskDetail entryId taskId = do
    m <- getHttpMethod
    case m of
        GET -> TaskController.taskPage taskId
        PUT -> do
            body <- getBody
            case eitherDecode body :: Either String TaskDto.Task of
                  Right taskDto ->
                       callIfAuthorized (TaskController.updateTask taskId taskDto)
                  Left errorMessage -> badRequest errorMessage
        DELETE -> callIfAuthorized (TaskController.deleteTask entryId taskId)
        other  -> notImplemented other

routeTaskWithUser :: EntryId -> TaskId -> App Response
routeTaskWithUser entryId taskId = do
    m <- getHttpMethod
    case m of
        DELETE -> callIfAuthorized (TaskController.removeUserFromTask taskId)
        PUT -> callIfAuthorized (TaskController.addUserToTask taskId)
        other -> notImplemented other