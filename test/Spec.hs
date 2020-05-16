import Protolude

import qualified Test as T

main :: IO ()
main = do
  -- T.go T.omegaTy1
  T.go T.natSuc'
  -- T.go T.impredTest2
  putStrLn ("Test suite not yet implemented" :: Text)
