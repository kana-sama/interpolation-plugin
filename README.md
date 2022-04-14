```haskell
{-# OPTIONS_GHC -fplugin=Plugin #-}

main = putStrLn "hello #{world}"
  where world = "world"
```
