module Sed where

import qualified Data.text as T
import qualified Data.text.IO as TIO


sed :: T.Text -> T.Text
sed t = t


main :: IO ()
main = TIO.interact sed

