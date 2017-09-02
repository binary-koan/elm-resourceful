module Resource.Store.Memory exposing (..)

import Task
import Resource exposing (Store)


memoryStore : Store resource
memoryStore =
    { list = Task.succeed []
    , create = Task.succeed
    , update = Task.succeed
    }
