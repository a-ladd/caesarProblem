module Caesar where

import Data.String
import Data.List
import Data.Char
import Data.Maybe


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
input2 = "pda awnpd eojp wypqwhhu nkqjz xqp jwow lwuo qo jkp pk owu wjupdejc odd zkj'p pahh wjukja awnpdeonkqjzhkh"


caesarShift1 :: Char -> Int -> Char
caesarShift1 ch key = let newKey = ((fromJust(elemIndex ch alphabet) + key) % 26)
                      in alphabet !! newKey

--shift one char by specified key (letters only)
caesarShift :: Char -> Int -> Char
caesarShift c i = let shiftNum = (fromJust(elemIndex c alphabet) + i)
                  in if shiftNum > 25
                  then alphabet !! (shiftNum - 26)
                  else if (shiftNum < 0)
                  then alphabet !! (shiftNum + 26)
                  else alphabet !! shiftNum


--check a potentially decoded string for the tag at the end: 'earthisroundlol'
checkStr :: [Char] -> Bool
checkStr str = isSuffixOf "earthisroundlol" str

--shift the whole string by key
stringShift :: [Char] -> Int -> [Char]
stringShift "" i = ""
stringShift str i = if not (isSpace (head str))
                    then [(caesarShift (head str) i)] ++ (stringShift (tail str) i)
                    else [' '] ++ (stringShift (tail str) i)

--generate a list of all possible caesar shifts of the encrypted input, used in next function
allStrings :: String -> [String]
allStrings input = [stringShift input x | x <- [1..25]]

--find and retrieve the string which contains plaintext, put it in a tuple with the key used to encrypt it
findPlainText :: [String] -> Int -> (String, Int)
findPlainText [] n = ("", -1)
findPlainText lst n = if (checkStr (head lst))
                    then ((head lst), n)
                    else findPlainText (tail lst) (n+1)
                    
main = do
  findPlainText (allStrings input2) 1