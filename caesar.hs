module Caesar where

import Prelude
import Data.String
import Data.List
import Data.Char
import Data.Maybe

--pledged Andrew Ladd 1/24/19
--this is a solution for 'Decoding Globehead Transmissions'
--probably a bit more complicated than need be, but it works

--array containing lower-case alphabet for encryption scheme
alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyz"

--test input and output, key is 13
output1 :: [Char]
output1 = "ha ha ha flat earth ppl r dumb illuminati is smart earthisroundlol"
input1 :: [Char]
input1 = "un un un syng rnegu ccy e qhzo vyyhzvangv vf fzneg rneguvfebhaqyby"

--test case 2, key is 22
output2 :: [Char]
output2 = "the earth isnt actually round but nasa pays us not to say anything shh dont tell anyone earthisroundlol"
input2 :: [Char]
input2 = "pda awnpd eojp wypqwhhu nkqjz xqp jwow lwuo qo jkp pk owu wjupdejc odd zkjp pahh wjukja awnpdeonkqjzhkh"

--test case 3, key is -9
output3 :: [Char]
output3 = "if the earth is flat then how come the other planets r not also flat wtf earthisroundlol"
input3 :: [Char]
input3 = "zw kyv vriky zj wcrk kyve yfn tfdv kyv fkyvi gcrevkj i efk rcjf wcrk nkw vrikyzjifleucfc"


--caesarShift takes a character and a ciper key and returns a char rotates by the key (positive or negative, but not 0)
caesarShift :: Char -> Int -> Char
caesarShift ch key = let newKey = abs $ (fromIntegral((fromJust(elemIndex ch alphabet) + key)) `mod` fromIntegral 26)
                      in alphabet !! newKey


--checkStr is a very very simple search function that checks for the Globeheads' message tag in a given string, returning true or false
checkStr :: [Char] -> Bool
checkStr str = isSuffixOf "earthisroundlol" str

--stringShift takes a string and key and shifts each character one by one recursively, ignoring spaces
stringShift :: [Char] -> Int -> [Char]
stringShift "" i = ""
stringShift str i = if not (isSpace (head str))
                    then [(caesarShift (head str) i)] ++ (stringShift (tail str) i)
                    else [' '] ++ (stringShift (tail str) i)

--allStrings generates a list of all possible caesar shifts of the encrypted input, used in next function.
--this is a helper function which helps the program find the cipher key as easily as possible
allStrings :: String -> [String]
allStrings input = [stringShift input x | x <- [1..25]]

--findPlainText takes a list of strings (should be generated by allStrings) and uses helper function checkStr to identify the decoded message and return a tuple of the decoded message and an int representing its key
findPlainText :: [String] -> Int -> (String, Int)
findPlainText [] n = ("you done goofed", -1)
findPlainText lst n = if (checkStr (head lst))
                    then ((head lst), n)
                    else findPlainText (tail lst) (n+1)


--get input immediately on running the program (as hackerrank does)
--convert and spit it back out in the format listed on the doc
main = do
  newInput <- getLine
  let newOutput = findPlainText (allStrings newInput) 1
  putStrLn $ (fst newOutput) ++ "\n" ++ (show (snd newOutput))