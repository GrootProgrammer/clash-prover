module Tests.Language.Expression where

import Language.Expression
import Test.HUnit

tests =
  TestList
    [ TestLabel "testForallReduction" testForallReduction,
      TestLabel "altReduction" testAltReduction,
      TestLabel "caseReduction" testCaseReduction,
      TestLabel "varReduction" testVarReduction,
      TestLabel "appReduction" testAppReduction
    ]

simpleVar :: String -> VarRep
simpleVar s = VarRepped s undefined undefined undefined

testReduction =
  TestList
    [ testForallReduction
    ]

testForallReduction =
  TestList
    [ TestCase
        ( assertEqual
            "forallSimple"
            (alphaReduction (TypeForall (simpleVar "s") (TypeVariable $ simpleVar "s")) (simpleVar "s") (ExprType $ TypeVariable $ simpleVar "a"))
            (TypeVariable $ simpleVar "a")
        ),
      TestCase
        ( assertEqual
            "forallInternal"
            ( alphaReduction
                (TypeForall (simpleVar "b") (TypeVariable $ simpleVar "s"))
                (simpleVar "s")
                (ExprType $ TypeVariable $ simpleVar "a")
            )
            (TypeForall (simpleVar "b") (TypeVariable $ simpleVar "a"))
        )
    ]

testAltReduction =
  TestList
    [ TestCase
        ( assertEqual
            "altMatchReduction"
            ( alphaReduction
                ( Alt
                    (ExprDataCon $ simpleVar "a")
                    []
                    (ExprVar $ simpleVar "b")
                )
                (simpleVar "a")
                (ExprVar $ simpleVar "c")
            )
            (Alt (ExprDataCon $ simpleVar "c") [] (ExprVar $ simpleVar "b"))
        )
    ]

testCaseReduction =
  TestList
    [ TestCase
        ( assertEqual
            "caseExprReduction"
            ( alphaReduction
                ( ExprCase
                    (ExprVar $ simpleVar "a")
                    (simpleVar "b")
                    (TypeVariable $ simpleVar "c")
                    []
                )
                (simpleVar "a")
                (ExprVar $ simpleVar "c")
            )
            ( ExprCase
                (ExprVar $ simpleVar "c")
                (simpleVar "b")
                (TypeVariable $ simpleVar "c")
                []
            )
        )
    ]

testVarReduction =
  TestList
    [ TestCase
        ( assertEqual
            "completeVar"
            ( alphaReduction
                (ExprVar $ simpleVar "a")
                (simpleVar "a")
                (ExprVar $ simpleVar "b")
            )
            (ExprVar $ simpleVar "b")
        )
    ]

testAppReduction =
  TestList
    [ TestCase
        ( assertEqual
            "appArgReduction"
            ( alphaReduction
                ( ExprApp
                    (ExprVar $ simpleVar "a")
                    (ExprVar $ simpleVar "b")
                )
                (simpleVar "b")
                (ExprVar $ simpleVar "c")
            )
            (ExprApp (ExprVar $ simpleVar "a") (ExprVar $ simpleVar "c"))
        )
    ]
