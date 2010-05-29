import Prelude hiding ((+))
import Data.Monoid.Owns

main = do
  putStrLn ( "Hello" + " " + "world" )
  print (1 + 2 :: Int)
  print (3.0 + 4.0 :: Double)
