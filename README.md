Tryme is a file that stores functions that will help you generate lists of numbers and words for easy learning of Haskell.

``` 
import System.Random (randomRIO)
```

```
generateIntRange :: Int -> (Int, Int) -> IO [Int]
generateIntRange len (min, max) = sequence $ replicate len $ randomRIO (min, max)
```

```
generateFloatRange :: Int -> (Float, Float) -> IO [Float]
generateFloatRange len (min, max) = sequence $ replicate len $ randomRIO (min, max)
```

```
prefixes :: [String]
prefixes = [
  "un", "re", "in", "im", "dis", "en", "em", "non", "de", "ex",
  "pre", "pro", "anti", "auto", "bi", "co", "counter", "semi", "sub", "super"]

suffixes :: [String]
suffixes = [
  "ing", "ed", "er", "ism", "ist", "ment", "ness", "tion", "ity", "ance",
  "ence", "dom", "ess", "ful", "ic", "ical", "ious", "ish", "ive", "less", "ly"]

wordRoots :: [String]
wordRoots = [
  "act", "all", "aud", "bel", "cap", "cert", "cred", "duc", "fac", "fer",
  "form", "fort", "grad", "graph", "ject", "jud", "log", "man", "mit", "mov",
  "nat", "pend", "port", "pos", "rect", "scrib", "sect", "sent", "spec", "struct",
  "tend", "tract", "ven", "vers", "vid", "vis", "voc", "vol"]

getRandomElement :: [a] -> IO a
getRandomElement xs = do
    randomIndex <- randomRIO (0, length xs - 1)
    return (xs !! randomIndex)

yesOrNo :: IO Bool
yesOrNo = do
    num <- randomRIO (0, 1) :: IO Int
    return $ num == 1

generateWord :: IO String
generateWord = do
    root <- getRandomElement wordRoots
    needPrefix <- yesOrNo
    withPrefix <- if needPrefix
                  then do
                      prefix <- getRandomElement prefixes
                      return $ prefix ++ root
                  else return root

    needSuffix <- yesOrNo
    result <- if needSuffix
              then do
                  suffix <- getRandomElement suffixes
                  return $ withPrefix ++ suffix
              else return withPrefix

    return result
```
