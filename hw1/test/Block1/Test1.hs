{-# LANGUAGE BlockArguments #-}

module Block1.Test1
  ( test
  ) where

import Block1.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Day" $ do
    describe "nextDay" $ do
      it "Monday    -> Tuesday" $ nextDay Monday `shouldBe` Tuesday
      it "Tuesady   -> Wednesday" $ nextDay Tuesday `shouldBe` Wednesday
      it "Wednesday -> Thursday" $ nextDay Wednesday `shouldBe` Thursday
      it "Thursday  -> Friday" $ nextDay Thursday `shouldBe` Friday
      it "Friday    -> Saturday" $ nextDay Friday `shouldBe` Saturday
      it "Saturday  -> Sunday" $ nextDay Saturday `shouldBe` Sunday
      it "Sunday    -> Monday" $ nextDay Sunday `shouldBe` Monday

    describe "afterDays" $ do
      it "Monday is Monday after 0 days" $ (afterDays Monday 0) `shouldBe` Monday
      it "Monday is Monday after 7 days" $ (afterDays Monday 7) `shouldBe` Monday
      it "Monday is Monday after 14 days" $ (afterDays Monday 14) `shouldBe` Monday
      it "Tuesday is Monday after 1 days" $ (afterDays Monday 1) `shouldBe` Tuesday
      it "Tuesday is Monday after 8 days" $ (afterDays Monday 8) `shouldBe` Tuesday
      it "Tuesday is Monday after 15 days" $ (afterDays Monday 15) `shouldBe` Tuesday
      it "Wednesday is Monday after 2 days" $ (afterDays Monday 2) `shouldBe` Wednesday
      it "Wednesday is Monday after 9 days" $ (afterDays Monday 9) `shouldBe` Wednesday
      it "Wednesday is Monday after 16 days" $ (afterDays Monday 16) `shouldBe` Wednesday
      it "Thursday is Monday after 3 days" $ (afterDays Monday 3) `shouldBe` Thursday
      it "Thursday is Monday after 10 days" $ (afterDays Monday 10) `shouldBe` Thursday
      it "Thursday is Monday after 17 days" $ (afterDays Monday 17) `shouldBe` Thursday
      it "Friday is Monday after 4 days" $ (afterDays Monday 4) `shouldBe` Friday
      it "Friday is Monday after 11 days" $ (afterDays Monday 11) `shouldBe` Friday
      it "Friday is Monday after 18 days" $ (afterDays Monday 18) `shouldBe` Friday
      it "Saturday is Monday after 5 days" $ (afterDays Monday 5) `shouldBe` Saturday
      it "Saturday is Monday after 12 days" $ (afterDays Monday 12) `shouldBe` Saturday
      it "Saturday is Monday after 19 days" $ (afterDays Monday 19) `shouldBe` Saturday
      it "Sunday is Monday after 6 days" $ (afterDays Monday 6) `shouldBe` Sunday
      it "Sunday is Monday after 13 days" $ (afterDays Monday 13) `shouldBe` Sunday
      it "Sunday is Monday after 20 days" $ (afterDays Monday 20) `shouldBe` Sunday

    describe "isWeekend" $ do
      it "Monday    is not weekend" $ isWeekend Monday `shouldBe` False
      it "Tuesady   is not weekend" $ isWeekend Tuesday `shouldBe` False
      it "Wednesday is not weekend" $ isWeekend Wednesday `shouldBe` False
      it "Thursday  is not weekend" $ isWeekend Thursday `shouldBe` False
      it "Friday    is not weekend" $ isWeekend Friday `shouldBe` False
      it "Saturday  is weekend" $ isWeekend Saturday `shouldBe` True
      it "Sunday    is weekend" $ isWeekend Sunday `shouldBe` True

    describe "daysToParty" $ do
      it "from Friday to party is 0 days" $ daysToParty Friday `shouldBe` 0
      it "from Saturday to party is 6 days" $ daysToParty Saturday `shouldBe` 6
      it "from Sunday to party is 5 days" $ daysToParty Sunday `shouldBe` 5
      it "from Monday to party is 4 days" $ daysToParty Monday `shouldBe` 4
      it "from Tuesday to party is 3 days" $ daysToParty Tuesday `shouldBe` 3
      it "from Wednesday to party is 2 days" $ daysToParty Wednesday `shouldBe` 2
      it "from Thursday to party is 1 days" $ daysToParty Thursday `shouldBe` 1