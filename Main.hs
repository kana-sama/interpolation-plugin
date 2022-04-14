{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Plugin #-}

import Data.String (IsString)
import Data.Text.IO qualified as Text

main = do
  print "hello world"
  putStrLn "hello #{world}"
  putStrLn "hello \\#{world}"
  Text.putStrLn "hello #{world}"
  where
    world :: IsString a => a
    world = "WORLD"
