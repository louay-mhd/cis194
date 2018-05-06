import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import CsSpring13.Week1.Hw

main = defaultMain unitTests

unitTests =
  testGroup "Week1 tests"
    [ testGroup "toDigits" toDigitsTests
     ,testGroup "toDigitsRev" toDigitsRevTests
     ,testGroup "doubleEveryOther" doubleEveryOtherTests
     ,testGroup "sumDigits" sumDigitsTests
     ,testGroup "validate" validateTests
     ,testGroup "hanoi" hanoiTests
    ]

{-- Week2 --}

{-- Week1--}
toDigitsTests =
  [testCase "1234 == [1,2,3,4]" $ assertEqual [] [1,2,3,4] (toDigits 1234)
   ,testCase "0 == []" $ assertEqual [] [] (toDigits 0)
   ,testCase "negative number = []" $ assertEqual [] [] (toDigits (-17))
  ]
toDigitsRevTests =
    [testCase "1234 == [4,3,2,1]" $ assertEqual [] [4,3,2,1] (toDigitsRev 1234)
    ]
doubleEveryOtherTests =
    [testCase "[8,7,6,5] == [16,7,12,5]" $ assertEqual [] [16,7,12,5] (doubleEveryOther [8,7,6,5])
    ,testCase "[1,2,3] == [1,4,3]" $ assertEqual [] [1,4,3] (doubleEveryOther [1,2,3])
    ]
sumDigitsTests =
    [testCase "[16,7,12,5] == 22" $ assertEqual [] 22 (sumDigits [16,7,12,5])
    ]
validateTests =
        [testCase "4012888888881881 == True" $ assertEqual [] True (validate 4012888888881881)
        ,testCase "4012888888881882 == False" $ assertEqual [] False (validate 4012888888881882)
        ]
hanoiTests =
    [testCase "hanoi 2 a b c == [(a,c), (a,b),(c,b)]" $ assertEqual [] [("a","c"), ("a","b"), ("c","b")] (hanoi 2 "a" "b" "c")
    ]
