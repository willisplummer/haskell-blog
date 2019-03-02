module Main where
  import Test.HUnit

  foo :: Int -> (Int, Int)
  foo x = (1, x)

  test1 = TestCase (assertEqual "for (foo 3)," (1,3) (foo 3))

  tests :: Test
  tests = TestList [TestLabel "test1" test1]

  main :: IO Counts
  main = runTestTT tests
