module Examples where

import           Main
import qualified Data.Map.Strict               as Map
import           Data.Functor                   ( (<&>) )

run :: String -> IO String
run input =
  parseInput input
    <&> multiLambda
    <&> lift
    <&> Map.map eta
    <&> removeRedundant
    <&> compileSupercombinators
    <&> Map.map removeRedundantInstructions
    <&> printProgram

-- No idea what happens if we try to run this
yc = "\\ f. (\\ x. f (x x)) (\\ x. f (x x))"

inc = "\\ x. + x 1"
dec = "\\ x. - x 1"
