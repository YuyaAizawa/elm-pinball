module Vector exposing (..)

type alias Vector =
  { x : Float
  , y : Float
  }

add : Vector -> Vector -> Vector
add v1 v2 =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }

sub : Vector -> Vector -> Vector
sub v1 v2 =
  { x = v1.x - v2.x
  , y = v1.y - v2.y
  }

mul : Float -> Vector -> Vector
mul c v =
  { x = c * v.x
  , y = c * v.y
  }

dot : Vector -> Vector -> Float
dot v1 v2 =
  v1.x * v2.x + v1.y * v2.y

dst2 : Vector -> Vector -> Float
dst2 v1 v2 =
  let
    dx = v1.x - v2.x
    dy = v1.y - v2.y
  in
    dx*dx + dy*dy

normalize : Vector -> Vector
normalize v =
  let
    r = sqrt (dot v v)
  in
    { x = v.x / r
    , y = v.y / r
    }

zero =
  { x = 0.0
  , y = 0.0
  }
