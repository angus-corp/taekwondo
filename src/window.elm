module Window exposing (close)

import Task exposing (Task)

close : Task () x
close = Native.Window.close
