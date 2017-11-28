effect module Chokidar where { command = ChokidarCmd, subscription = ChokidarSub } exposing (..)

type alias Id = Int

type Matcher
  = StringMatcher String
  | ListMatcher (ListString)
  | RegexMatcher Regex
  | FunctionMatcher (String -> Bool)

type Polling = NoPolling | Polling { interval: Int, binaryInterval: Int }

type Awaiter = NoAwait | DefaultAwait | CustomAwait { stabilityThreshold: Int, pollInterval: Int  }

type alias Options =
  { persistent: Bool
  , ignored: List Matcher
  , ignoreInitial: Bool
  , followSymlinks: Bool
  , cwd: Maybe String
  , disableGlobbing: Bool
  , polling: Polling
  , useFsEvents: Maybe Bool
  , alwaysStat: Bool
  , depth: Maybe Int
  , awaitWriteFinish: Awaiter
  , ignorePermissionErrors: Bool
  , atomic: Maybe Bool
  }

defaultOptions: Options
defaultOptions =
  { persistent = True
  , ignored = []
  , ignoreInitial = False
  , followSymlinks = True
  , cwd = Nothing
  , disableGlobbing = False
  , polling = NoPolling
  , useFsEvents = Nothing
  , alwaysStat = False
  , depth = Nothing
  , awaitWriteFinish = NoAwait
  , ignorePermissionErrors = False
  , atomic = Nothing
  }

-- -----------------------------------------------------------------------------
-- CMD
-- -----------------------------------------------------------------------------

type ChokidarCmd msg
  = Add Id (List String)
  | Remove Id (List String)
  | Close Id

add: Id -> List String -> Cmd msg
add id paths =
  command (Add id paths)

remove: Id -> List String -> Cmd msg
remove id paths =
  command (Remove id paths)

close: Id -> Cmd msg
close id =
  command << Close

cmdMap : (a -> b) -> ChokidarCmd a -> ChokidarCmd b
cmdMap f cmd =
  case cmd of
    Add m n    -> Add m n
    Remove m n -> Remove m n
    Close n    -> Close n

-- -----------------------------------------------------------------------------
-- SUB
-- -----------------------------------------------------------------------------

type alias PathMsg = { id: Id, path: String }
type alias PathStatsMsg = { id: Id, path: String, stats: Maybe Stats }

type Msg
  = Added PathStatsMsg
  | Changed PathStatsMsg
  | Unlinked PathMsg
  | AddedDir PathStatsMsg
  | UnlinkedDir PathMsg
  | Error PathMsg
  | Ready PathMsg
  | Raw PathMsg

type HapiSub msg
  = Listen (Msg -> msg)

listen: (Msg -> msg) -> Sub msg
listen =
  subscription << Listen

subMap : (a -> b) -> HapiSub a -> HapiSub b
subMap f sub =
  case sub of
    Listen tagger -> Listen (tagger >> f)
