{-# LANGUAGE RecordWildCards #-}

module ProcGen.Turtle where

-- linear
import Linear hiding (angle, distance)

data Turtle = Turtle
  { turtleDir :: V3 Double
  , turtlePos :: V3 Double
  , turtleRight :: V3 Double
  }
  deriving (Show, Eq)

turnRight :: Double -> Turtle -> Turtle
turnRight angle Turtle {..} =
  let axis = normalize $ cross turtleDir turtleRight
      rotQuat = axisAngle axis angle
      newDir = normalize $ rotate rotQuat turtleDir
      newRight = normalize $ rotate rotQuat turtleRight
  in Turtle { turtleDir = newDir, turtleRight = newRight, .. }

turnLeft :: Double -> Turtle -> Turtle
turnLeft angle = turnRight (-angle)

pitchUp :: Double -> Turtle -> Turtle
pitchUp angle Turtle {..} =
  Turtle { turtleDir = normalize (rotate (axisAngle turtleRight angle) turtleDir), .. }

pitchDown :: Double -> Turtle -> Turtle
pitchDown angle = pitchUp (-angle)

rollRight :: Double -> Turtle -> Turtle
rollRight angle Turtle {..} =
  Turtle { turtleRight = normalize (rotate (axisAngle turtleDir angle) turtleRight), .. }

rollLeft :: Double -> Turtle -> Turtle
rollLeft angle = rollRight (-angle)

move :: Double -> Turtle -> Turtle
move distance Turtle {..} =
  Turtle { turtlePos = turtlePos + turtleDir ^* distance, .. }
