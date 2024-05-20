module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-


type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- put both 'm' and 'n' into '#' then takes both answers and only selects the second answer
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- put both 'm' and 'n' into '#' then takes both answers and only selects the first answer
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- 'space' checks one char at a time, and the 'char' function is so the output is a parsed value
spaces :: Parser String
spaces = iter space
        where space :: Parser Char
              space = char ? isSpace

token :: Parser a -> Parser a
token m = m #- spaces

-- uses infix operator ? to check if the input is a char and if it is a letter
letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- uses '#' to check if both char and chars return a correct input
-- when 'chars' is called it recursively applies 'char' to every char in the string
-- it then uses '>->' to take the input from the recursion into 'cons' to concatenate it
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

{-
chars :: Int -> Parser String
chars 0 cs = return "" cs
chars n (c:cs) = do
      x1 <- char [c]
      x2 <- chars (n-1) cs
      x1:x2 -}

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- pretty much same as accept but reports an error if it doesnt succeed
require :: String -> Parser String
require w = accept w ! err "Program error: "

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

