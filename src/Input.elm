module Input exposing
  ( Key(..)
  , decoder
  --
  , KeySet
  , empty
  , member
  , insert
  , remove
  , fold
  , toList
  , fromList
  )

import Browser.Events exposing (onKeyUp, onKeyDown)
import Json.Decode as Json
import Bitwise exposing (..)



-- KEY

type Key
  = LFlipper
  | RFlipper

all : List Key
all =
  [ LFlipper
  , RFlipper
  ]

mask : Key -> Int
mask key =
  case key of
    LFlipper -> 1
    RFlipper -> 2

decoder : (KeySet -> msg) -> (String -> Maybe Key) -> KeySet -> Sub msg
decoder msgMapper config previous =
  Sub.batch
  [ onKeyDown <| keyUpdater config insert previous <| msgMapper
  , onKeyUp   <| keyUpdater config remove previous <| msgMapper
  ]

keyUpdater : (String -> Maybe Key) -> (Key -> KeySet -> KeySet) -> KeySet -> (KeySet -> msg) -> Json.Decoder msg
keyUpdater config op previous msgMapper =
  Json.field "key" Json.string
    |> Json.map
      (config
        >> Maybe.map (\key -> op key previous)
        >> Maybe.withDefault previous
        >> msgMapper
      )



-- KEY SET

type KeySet
  = KeySet Int

empty : KeySet
empty =
  KeySet 0

member : Key -> KeySet -> Bool
member key (KeySet n) =
  mask key
    |> and n
    |> (/=) 0

insert : Key -> KeySet -> KeySet
insert key (KeySet n) =
  mask key
    |> or n
    |> KeySet

remove : Key -> KeySet -> KeySet
remove key (KeySet n) =
  mask key
    |> complement
    |> and n
    |> KeySet

fold : (Key -> a -> a) -> a -> KeySet -> a
fold acc init keyset =
  let
    acc_ = \k a ->
      if member k keyset
      then acc k a
      else a
  in
    List.foldl acc_ init all

toList : KeySet -> List Key
toList =
  fold (::) []

fromList : List Key -> KeySet
fromList =
  List.foldl insert empty
