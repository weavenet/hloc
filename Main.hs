-- File: Main.hs
{-# LANGUAGE BangPatterns #-}
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import Control.Monad
import Data.List

import Hloc

data Options = Options  { optVerbose       :: Bool
                        , optCount         :: Bool
                        , optLanguage      :: String
                        , optShowLanguages :: Bool
                        , optToggle        :: Bool
                        }

startOptions :: Options
startOptions = Options  { optVerbose       = False
                        , optCount         = False
                        , optLanguage      = "auto"
                        , optShowLanguages = False
                        , optToggle        = False
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [  
      Option "c" ["count"]
        (NoArg
            (\opt -> return opt { optCount = True }))
        "Return count of comments, blank lines and total lines of code"

    , Option "l" ["language"]
        (ReqArg
            (\ d opt -> return opt { optLanguage = d }) "LANGUAGE")
        "Specify language"

    , Option "L" ["languages"]
        (NoArg
            (\opt -> return opt { optShowLanguages = True }))
        "Show supported languages and their known file exetensions"

    , Option "t" ["toggle"]
        (NoArg
            (\opt -> return opt { optToggle = True }))
        "Toggle to show non-comments"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr version
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

filteredResults :: [Hloc.Line] -> Bool -> [Hloc.Line]
filteredResults lns toggle = filter (\x -> lineComment x /= toggle) lns

formatCount' cnt fileName result = cnt ++ " " ++ fileName ++ " (" ++ (show $ getLanguage result) ++ ")"
formatCount result fileName True  = 
  let cnt = (show $ getCountResult result)
    in formatCount' cnt fileName result

formatCount result fileName False =
  let cnt = "       -       -       -"
    in formatCount' cnt fileName result

displayCount :: [Char] -> Result -> IO ()
displayCount fileName result = hPutStrLn stdout $ formatCount result fileName (getSuccess result)

displayLines _      []    = return ()
displayLines toggle lines = hPutStrLn stdout $ intercalate "\n" $ map show $ filteredResults lines toggle

display :: [Char] -> Hloc.Result -> Bool -> Bool -> IO ()
display fileName result True   _      = displayCount fileName result
display _        result False  toggle = displayLines toggle $ getLines result

processResult True result count toggle =
  do
    display "<STDIN>" result count toggle
    exitWith ExitSuccess
processResult False result _ _=
  exitError $ getErrorComment result

processInput :: [FilePath] -> String -> Bool -> Bool -> [Char] -> IO ()
processInput [] inputFromStdIn count toggle language =
  let result = processText "<STDIN>" inputFromStdIn language
    in processResult (getSuccess result) result count toggle

processInput fileNames _ count toggle language =
  forM_ fileNames $ \fileName -> do
    file <- readFile fileName
    let result = processText fileName file language
      in display fileName result count toggle

exitError :: [Char] -> IO b
exitError txt = do
  hPutStrLn stdout $ "ERROR: " ++ txt
  exitWith $ ExitFailure 1

process _ _ _ _ _ True =
  let showLanguageExtensionFns = (\x -> (show x) ++ ": " ++ intercalate ", " (extensions x))
      languageExtensionTxt = (intercalate "\n" $ map showLanguageExtensionFns languages)
    in hPutStrLn stdout $ "Supported languages and corresponding extensions: \n\n" ++ languageExtensionTxt

process files inputFromStdIn language count toggle _
  | language /= "auto" && length files > 1  =
    exitError "-l flag can only be used with <STDIN> or on single file"
  | language /= "auto" && length files > 1  =
    exitError "-l flag can only be used with <STDIN> or on single file"
  | length files == 0 && language == "auto" =
    exitError "-l flag required when reading from <STDIN>"
  | toggle == True && count == True =
    exitError "-t flag cannot be used with -c flag"
  | otherwise = processInput files inputFromStdIn count toggle language

getStdIn True = do 
  !inputFromStdIn <- getContents
  return inputFromStdIn
getStdIn False = do 
  inputFromStdIn <- getContents
  return inputFromStdIn

main :: IO ()
main = do
    args <- getArgs

    let (actions, files, _) = getOpt Permute options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose       = verbose,
                  optCount         = count,
                  optLanguage      = language,
                  optShowLanguages = showLanguages,
                  optToggle        = toggle } = opts

    inputFromStdIn <- getStdIn (length files == 0 && not showLanguages) -- only force evaluation of stdin in certain conditions

    process files inputFromStdIn language count toggle showLanguages
