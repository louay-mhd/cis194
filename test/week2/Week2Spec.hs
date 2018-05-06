import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import CsSpring13.Week2.Log
import CsSpring13.Week2.LogAnalysis

main = defaultMain unitTests

unitTests =
  testGroup "Week2 tests"
    [ testGroup "parseMessage" parseMessageTests
    ]

parseMessageTests =
      [testCase "E 2 562 help help" $ assertEqual [] (LogMessage (Error 2) 562 "help help") (parseMessage "E 2 562 help help")
      ,testCase "I 29 la la la" $ assertEqual [] (LogMessage Info 29 "la la la") (parseMessage "I 29 la la la")
      ,testCase "W 100 another warning!" $ assertEqual [] (LogMessage Warning 100 "another warning!") (parseMessage "W 100 another warning!")
      ,testCase "This is not in the right format" $ assertEqual [] (Unknown "This is not in the right format") (parseMessage "This is not in the right format")
      ,testCase "T" $ assertEqual [] (Unknown "T") (parseMessage "T")
      ]
