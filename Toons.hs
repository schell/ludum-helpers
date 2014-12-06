{-# LANGUAGE DeriveDataTypeable #-}
module Toons where

import Types
import Linear
import Data.Typeable

data Displayable = CleanFrame
                 | FullSheet
                 | Reaper CardinalDirection
                 | Text String
                 | Box Width Height
                 deriving (Typeable, Show)

data CardinalDirection = North
                       | South
                       | East
                       | West
                       deriving (Eq, Show, Typeable)

velocityToDirections :: Velocity -> [Direction]
velocityToDirections (V2 x y) = [x',y']
    where x' = if x == 0 then None
                 else if x > 0 then Right'
                        else Left'
          y' = if y == 0 then None
                 else if y > 0 then Down
                        else Up

tupleContains :: Eq a => (a,a) -> a -> Bool
tupleContains (a,b) c = a == c || b == c

cardinalToDirections :: CardinalDirection -> (Direction,Direction)
cardinalToDirections North = (Up, Right')
cardinalToDirections East  = (Down, Right')
cardinalToDirections South = (Down, Left')
cardinalToDirections West  = (Up, Left')

cardinalPlusDirection :: CardinalDirection -> Direction -> CardinalDirection
cardinalPlusDirection c None = c
cardinalPlusDirection c d
    | cardinalToDirections c `tupleContains` d = c
    | otherwise = case (c,d) of
                      (North, Down)  -> East
                      (North, Left')  -> West
                      (East, Left')   -> South
                      (East, Up)     -> North
                      (South, Up)    -> West
                      (South, Right') -> East
                      (West, Down)   -> South
                      (West, Right')  -> North
                      _              -> c

displayPlusVelocity :: Displayable -> Velocity -> Displayable
displayPlusVelocity (Reaper c) v = Reaper c'
    where c' = foldl cardinalPlusDirection c $ velocityToDirections v
displayPlusVelocity d _ = d
