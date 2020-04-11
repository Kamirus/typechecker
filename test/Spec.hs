import Protolude

import qualified Test as T

main :: IO ()
main = do
  T.go T.const
  putStrLn ("Test suite not yet implemented" :: Text)
