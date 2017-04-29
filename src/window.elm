module Window exposing (close)

import Task exposing (Task)
import Native.Window

close : Task () x
close = Native.Window.close
