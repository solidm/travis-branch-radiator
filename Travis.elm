module Travis exposing (..)

import Json.Decode exposing (..)
import Http
import Task exposing (Task)
import Time exposing (second)
import Debug


type alias BranchStatus = {
  branches : List BranchBuild,
  commits : List Commit
}


type alias BranchBuild = {
  id : String,
  commitId : Int,
  state: String,
  number: String,
  date: String
}


type alias Commit = {
  id: Int,
  branch: String,
  committerName: String,
  committerEmail: String,
  message: String
}


decodeBranchStatus: Decoder BranchStatus
decodeBranchStatus = map2 BranchStatus (field "executions" (list decodeBranchBuild)) (field "executions" (list decodeCommit))


decodeBranchBuild: Decoder BranchBuild
decodeBranchBuild = map5 BranchBuild (at ["job", "name"] string) (field "id" int) (field "status" string) (at ["date-started", "date"] string) (at ["date-started", "date"] string)


decodeCommit: Decoder Commit
decodeCommit = map5 Commit (field "id" int) (at ["job", "name"] string) (field "user" string) (field "user" string) (at ["date-started", "date"] string)


getBranchBuildStatus : Maybe String -> String -> String -> Http.Request (String, BranchStatus)
getBranchBuildStatus apiKey jobId baseUrl =
  let key = case apiKey of
              Just key -> key
              Nothing -> ""
      url = baseUrl ++ "/api/14/job/" ++ jobId ++ "/executions?max=1&format=json&authtoken=" ++ key
      decoder = map (\result -> (jobId, result)) decodeBranchStatus
  in travisApiGet apiKey decoder url

travisApiGet : Maybe String -> Decoder a -> String -> Http.Request a
travisApiGet apiKey decoder url =
  let request =
    { method = "GET", headers = travisHeaders apiKey, url = url, body = Http.emptyBody, expect = Http.expectJson decoder, timeout = Just (20 * second), withCredentials = False }
  in Http.request request


travisHeaders : Maybe String -> List Http.Header
travisHeaders apiKey = [Http.header "Accept" "application/json"]


getAuthHeaders: Maybe String -> List Http.Header
getAuthHeaders maybeKey = case maybeKey of
  Just apiKey -> [Http.header "X-Rundeck-Auth-Token" apiKey]
  Nothing  -> []
