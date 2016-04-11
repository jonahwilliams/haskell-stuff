module Cipher where
  import Data.Char

  -- 0 to 57

  shift :: Int -> Int -> Int
  shift n v = rem (n + v) 57

  shiftChar :: Int -> Char -> Char
  shiftChar n c = chr ((shift n ((ord c) - 65)) + 65)


  caesarCipher :: String -> Int -> String
  caesarCipher word n =
    fmap (\c -> if (c /= ' ') then shiftChar n c else c) word
