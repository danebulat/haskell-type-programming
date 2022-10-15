{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where 

import DynamicLanguage
import Data.Maybe      (fromJust)
import Test.Hspec

liftD2Test :: Bool
liftD2Test = let d1 = Dynamic (22 :: Int)
                 d2 = Dynamic (4  :: Int)
                 md = liftD2 @Int @Int d1 d2 (+)
                 r  = fromDynamic @Int (fromJust md)
             in fromJust r == 26

dynMultTest :: Bool
dynMultTest = let d1 = Dynamic ("foo" :: String)
                  d2 = Dynamic (3     :: Integer)
                  rd = dynMult d1 d2
              in fromJust (fromDynamic @String rd) == "foofoofoo"

dynPlusTest :: Bool 
dynPlusTest = let d1 = Dynamic ("foo" :: String)
                  d2 = Dynamic (3     :: Integer)
                  rd = dynPlus d1 d2
              in fromJust (fromDynamic @String rd) == "foo3"

-- --------------------------------------------------------------------------------
-- Main

main :: IO ()
main = hspec $ do

  describe "Binary lift function liftD2" $ do

    it "Performs addition on two Dynamic objects storing Int" $ do
      liftD2Test
    
  describe "Dynamic function dynMult" $ do

    it "Can multiply a String and an Integer into a String" $ do
      dynMultTest
    
  describe "Dynamic function dynPlus" $ do

    it "Can add a String and an Integer into a String" $ do
      dynPlusTest

