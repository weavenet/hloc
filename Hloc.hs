module Hloc (extensions, getComments, getCountResult, getLanguage, getLines, getSuccess, getErrorComment, languages, lineComment, Line, processText, Result, version) where

import Prelude hiding (lookup)
import Text.Printf
import System.IO
import Control.Monad
import Data.List hiding (lookup)
import Data.List.Split
import Data.Char
import Data.Map (fromList, lookup, toList)
import Data.Text (justifyRight, pack, unpack)

version = "1.0.0"

data Line = Line Int String Bool
instance Show Line where show (Line num content comment) = printf content
lineNumber (Line x _ _) = x
lineContent (Line _ x _) = x
lineComment (Line _ _ x) = x
wsTrimmedLineContent line = trimLeadingWs $ lineContent line

data Exception = Exception Bool Bool
exceptionApplied (Exception x _) = x
exceptionResult (Exception _ y) = y

type ExceptionFn = (Int -> String -> Exception)

data MultiLinePair = MultiLinePair { start :: String, end :: String }
data Language = Language String [String] [MultiLinePair] [ExceptionFn] [String]
instance Show Language where show (Language x _ _ _ _) = x

name (Language x _ _ _ _) = x
commentStrings (Language _ x _ _ _) = x
multiLinePairs (Language _ _ x _ _) = x
supportMultiLine (Language _ _ x _ _) = length x > 0
commentExceptionFns (Language _ _ _ x _) = x
extensions (Language _ _ _ _ x) = x

-- list of known languages
firstLineBangBashException :: ExceptionFn
firstLineBangBashException lineNumber lineContent =
  let exceptionApplied = lineNumber == 1
      isBashSignature  = take 2 lineContent /= "#!"
    in Exception (exceptionApplied && not isBashSignature) False
bash = Language "bash" ["#"] [] [firstLineBangBashException] ["sh", "bash"]

java = Language "java" ["//"] [(MultiLinePair "/*" "*/")] [] ["java"]
php  = Language "php" ["//", "#"]  [(MultiLinePair "/*" "*/"), (MultiLinePair "<!--" "-->")] [] ["php"]
unknown  = Language "unknown" [] [] [] [] -- only used in error messages
languages = [bash, java, php]

languageNames = map name languages
languageMap = fromList $ map (\x -> (name x, x)) languages

data Error = Error { failure :: Bool , comment :: String }

data Result = Result CountResult [Line] Language Error
getCountResult (Result x _ _ _) = x
getLines (Result _ x _ _) = x
getLanguage (Result _ _ x _) = x
getSuccess (Result _ _ _ x) = not $ failure x
getErrorComment (Result _ _ _ x) = comment x

validResult countResult lines lng = Result countResult lines lng $ Error False ""
isValidResult (Result _ _ _ error) = Error

errorResult txt = Result (CountResult 0 0 0) [] unknown (Error True txt)
unableToDetermineLanguageResult = errorResult "Unable to determine language"
unknownLanguageResult language = 
  let t1 = "Unknown language '" ++ language ++ "'."
      t2 = "Known languages:" 
      t3 = "'" ++ intercalate ", " languageNames ++ "'."
    in errorResult $ intercalate " " [t1, t2, t3]

data CountResult = CountResult Int Int Int
instance Show CountResult where show (CountResult x y z) = intercalate "" $ map justify8 [x,y,z]
resultLoc (CountResult x _ _) = x
resultBlank (CountResult _ x _) = x
resultComment (CountResult _ _ x) = x

justify8 x = unpack $ justifyRight 8 ' ' $ pack $ show x

addComment :: Line -> Bool -> Line
addComment (Line lineNumber lineContent _) isComment = Line lineNumber lineContent isComment

isException :: [ExceptionFn] -> Int -> String -> Bool
isException exceptionFns lineNumber lineContent =
  length (processExceptions exceptionFns lineNumber lineContent) /= 0

getException :: [ExceptionFn] -> Int -> String -> Bool
getException exceptionFns lineNumber lineContent = 
  let exceptionResults = processExceptions exceptionFns lineNumber lineContent
    in exceptionResult $ head exceptionResults

processExceptions :: [ExceptionFn] -> Int -> String -> [Exception]
processExceptions exceptionFns lineNumber lineContent =
  let exceptionResults = map (\fn -> fn lineNumber lineContent) exceptionFns
    in filter (\e -> exceptionApplied e) exceptionResults 

isSingleLineComment :: [ExceptionFn] -> [String] -> Int -> String -> Bool
isSingleLineComment exceptionFns commentStrings lineNumber lineContent
  -- Check for exception, for example in bash #! lines
  | isException exceptionFns lineNumber lineContent
    = getException exceptionFns lineNumber lineContent

  -- If remaining string is less than the comment string
  | length lineContent < length commentStrings
    = False

  -- Evaluate if it is a comment string
  | [] /= (filter (\cs -> let len = length cs
                              y = take len lineContent
                              z = take len cs
                            in y == z ) commentStrings)
    = True

  -- Otherwise the value is not a comment
  | otherwise = False

processRawTextToLines :: String -> [Line]
processRawTextToLines contentLineString =
  let contentLines = lines contentLineString 
      numberLines = zip [1..(length contentLines)] contentLines
    in map (\x -> Line (fst x) (snd x) False) numberLines

trimLeadingWs :: String -> String
trimLeadingWs [] = ""
trimLeadingWs str =
  if head str == head " "
  then (trimLeadingWs (tail str))
  else str

lineStartMultiLineComment line lng =
  filter (\x -> let s = start x
                    l = length s
                  in ((take l $ lineContent line) == s))
         (multiLinePairs lng)

doesLineStartMultiLineComment line lng = length (lineStartMultiLineComment line lng) /= 0 

getMultiLineCommentStartEndString :: Line -> Language -> Maybe String
getMultiLineCommentStartEndString line lng =
  if doesLineStartMultiLineComment line lng then
    Just (end $ head ( lineStartMultiLineComment line lng))
  else
    Nothing

doesLineEndMultiLineComment _ Nothing = False
doesLineEndMultiLineComment line (Just e) =
  let c = lineContent line
      x = length c
      y = length e
      z = drop (x - y) c
    in z == e

addComments' :: [Line] -> Language -> Maybe String -> [Line] -> [Line]
addComments' [] lng state acc = acc
addComments' (line:xs) lng state acc
  -- is line in side multi line comment
  | supportMultiLine lng &&
      state /= Nothing &&
      not (doesLineEndMultiLineComment line state) =
    addComments' xs lng state $ addComment line True:acc

  -- does line start multi line comment IE /*
  | supportMultiLine lng &&
      doesLineStartMultiLineComment line lng &&
      not (doesLineEndMultiLineComment line (getMultiLineCommentStartEndString line lng)) =
    addComments' xs lng (getMultiLineCommentStartEndString line lng) $ addComment line True:acc

  -- does line start and end multi line comment IE /* some stuff */
  | supportMultiLine lng &&
      doesLineStartMultiLineComment line lng &&
      doesLineEndMultiLineComment line (getMultiLineCommentStartEndString line lng) =
    addComments' xs lng Nothing $ addComment line True:acc

  -- does line end multi line comment IE */
  | supportMultiLine lng && 
      doesLineEndMultiLineComment line state =
    addComments' xs lng Nothing $ (addComment line True):acc

  -- Line is not multi line comment, evaluate as single line comment
  | True =
      let cs = commentStrings lng
          ef = commentExceptionFns lng
          ws = wsTrimmedLineContent line
          ln = lineNumber line
          isComment = isSingleLineComment ef cs ln ws
      in addComments' xs lng Nothing $ addComment line isComment:acc

countBlankLines lns = length $ filter (\x -> all isSpace $ lineContent x) lns

getComments :: Language -> String -> [Line]
getComments language text =
 let lines = processRawTextToLines text
     fullyCommented = addComments' lines language Nothing []
       in reverse fullyCommented

countComments :: Language -> String -> CountResult
countComments lng text = 
  let lns = getComments lng text
      numComments = sum (map (\x -> if lineComment(x) then 1 else 0) lns)
      total = length lns
      blank = countBlankLines lns
    in CountResult numComments blank total

fileLanguage fileName =
  let ext = last (splitOn "." fileName)
      lngs = filter (\x -> ext `elem` extensions x) $ map snd $ toList languageMap
    in if length lngs == 0 then Nothing else Just $ head lngs

processText' fileName content language =
   let x = countComments language content
       y = getComments language content
     in validResult x y language

processText fileName content "auto" =
  let language = fileLanguage fileName
    in case language of
      Nothing -> unableToDetermineLanguageResult
      Just l -> processText' fileName content l

processText fileName content language =
  let lng = lookup language languageMap
    in case lng of
      Nothing -> unknownLanguageResult language
      Just l -> processText' fileName content l
