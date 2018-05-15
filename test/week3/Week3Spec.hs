import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import CsSpring13.Week3.Golf

main = defaultMain unitTests

unitTests =
  testGroup "Week2 tests"
    [ testGroup "skips" skipsTests
      ,testGroup "localMixma" localMaximaTests
      ,testGroup "histogram" histogramTests
    ]

skipsTests =
      [testCase "skips \"ABCD\"" $ assertEqual "" ["ABCD", "BD", "C", "D"] $ skips "ABCD"
      ,testCase "skips \"hello!\"" $ assertEqual "" ["hello!", "el!", "l!", "l", "o", "!"] $ skips "hello!"
      ,testCase "skips [True, False]" $ assertEqual "" [[True,False], [False]] $ skips [True, False]
      ,testCase "skips [1]" $ assertEqual "" [[1]] $ skips [1]
      ,testCase "skips []" $ assertEqual "" [] $ skips ([]::[Int])
      ]

localMaximaTests =
    [testCase "localMaxima [2,9,5,6,1] == [9,6]" $ assertEqual "" [9,6] $ localMaxima [2,9,5,6,1]
    ,testCase "localMaxima [2,3,4,1,5] == [4]" $ assertEqual "" [4] $ localMaxima [2,3,4,1,5]
    ,testCase "localMaxima [1,2,3,4,5] == []" $ assertEqual "" [] $ localMaxima [1,2,3,4,5]
    ]

histogramTests =
    [testCase "histogram [3,5] == \"   * *    \\n==========\\n0123456789\\n\"" $ assertEqual "" "   * *    \n==========\n0123456789\n" $ histogram [3,5]
    ,testCase "histogram [1,1,1,5] == \" *        \\n *        \\n *   *    \\n==========\\n0123456789\\n\"" $ assertEqual "" " *        \n *        \n *   *    \n==========\n0123456789\n" $ histogram [1,1,1,5]
    ,testCase "histogram [1,4,5,4,6,6,3,4,2,4,9] == \"    *     \\n    *     \\n    * *   \\n ******  *\\n==========\\n0123456789\\n\"" $ assertEqual "" "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n" $ histogram [1,4,5,4,6,6,3,4,2,4,9]
    ]
