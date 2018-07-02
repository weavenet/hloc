-- File: test.hs

import Hloc
import Test.HUnit

testBashSuccess = processText "test.sh" "#1234" "bash"
testAutoBashSuccess = processText "test.sh" "#1234" "auto"
testAutoFailed1 = processText "boom" "#1234" "auto"
testAutoFailed2 = processText "test.sh" "#1234" "boom"

testBash f = show $ getCountResult $ processText "test.sh" f "bash"
testJava f = show $ getCountResult $ processText "test.java" f "java"
testPhp f = show $ getCountResult $ processText "test.php" f "php"

main :: IO ()
main = do
  bashFile <- readFile "test_data/test.sh"
  javaFile <- readFile "test_data/test.java"
  phpFile <- readFile "test_data/test.php"

  let basicTests = [
            TestCase (assertEqual "Testing successfully processing file"
                                  True
                                  $ getSuccess $ testBashSuccess),
            TestCase (assertEqual "Testing successfully showing count"
                                  "       1       0       1"
                                  $ show $ getCountResult $ testBashSuccess),
            TestCase (assertEqual "Testing succesfully getting lines with language"
                                  "[#1234]"
                                  $ show $ getLines $ testBashSuccess),
            TestCase (assertEqual "Testing succesfully getting lines with auto"
                                  "[#1234]"
                                  $ show $ getLines $ testAutoBashSuccess),
            TestCase (assertEqual "Testing failed auto language lookup"
                                  False
                                  $ getSuccess $ testAutoFailed1),
            TestCase (assertEqual "Testing failed auto language lookup 2"
                                  False
                                  $ getSuccess $ testAutoFailed2)]
      languageTests = [
            TestCase (assertEqual "Testing bash"
                                  "       4       1       8"
                                  $ testBash bashFile),
            TestCase (assertEqual "Testing java"
                                  "      12      19      67"
                                  $ testJava javaFile),
            TestCase (assertEqual "Testing php"
                                  "       9       7      25"
                                  $ testPhp phpFile),
            TestCase (assertEqual "Testing language list"
                                  "[bash,java,php]"
                                  $ show languages)]
    in runTestTT $ TestList $ concat [basicTests, languageTests]
    -- in runTestTT $ TestList (concat basicTests languageTests)
  return ()
