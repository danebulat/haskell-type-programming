{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DynamicLanguage where 

import Data.Foldable (asum)
import Data.Maybe    (fromJust, fromMaybe)
import Data.Typeable

-- --------------------------------------------------------------------------------
-- Dynamic type and eliminator

data Dynamic where
  Dynamic :: Typeable a => a -> Dynamic 

-- Extracts value from dynamic and applies function
elimDynmaic :: (forall a. Typeable a => a -> r)
            -> Dynamic
            -> r
elimDynmaic f (Dynamic a) = f a

-- Attempts to cast a Dynamic object into 'a'
fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynmaic cast

-- --------------------------------------------------------------------------------
-- Lift function 

-- Attempt to lift a regular, strongly-typed function
-- into a function over dynamic types.
liftD2 :: forall a b r.
          ( Typeable a
          , Typeable b
          , Typeable r
          )
       => Dynamic
       -> Dynamic
       -> (a -> b -> r)
       -> Maybe Dynamic
liftD2 d1 d2 f =
  Dynamic <$> (f <$> fromDynamic d1 <*> fromDynamic d2)

-- --------------------------------------------------------------------------------
-- Dynamic (+) function

dynPlus :: Dynamic -> Dynamic -> Dynamic
dynPlus d1 d2 = fromMaybe err $ asum
    [ liftD2 @Int     @Int     d1 d2 (+)
    , liftD2 @Integer @Integer d1 d2 (+)
    , liftD2 @String  @String  d1 d2 (++)

    , liftD2 @Int     @String  d1 d2 (\i s -> show i ++ s)
    , liftD2 @Int     @Integer d1 d2 (\x y -> y + fromIntegral y)
    
    , liftD2 @String  @Int     d1 d2 (\s i -> s ++ show i)
    , liftD2 @String  @Integer d1 d2 (\s i -> s ++ show i)

    , liftD2 @Integer @Int     d1 d2 (\x y -> fromIntegral x + y)
    , liftD2 @Integer @String  d1 d2 (\i s -> show i ++ s)
    ]
  where
    err = error "bad types for dynPlus"

-- --------------------------------------------------------------------------------
-- Dynamic (*) function 

dynMult :: Dynamic -> Dynamic -> Dynamic
dynMult d1 d2 = fromMaybe err $ asum
    [ liftD2 @Int     @Int     d1 d2 (*)
    , liftD2 @Integer @Integer d1 d2 (*)

    , liftD2 @Int     @String  d1 d2 (\i s -> concat $ replicate i s)
    , liftD2 @Int     @Integer d1 d2 (\x y -> y * fromIntegral y)
    
    , liftD2 @String  @Int     d1 d2 (\s i -> concat $ replicate i s)
    , liftD2 @String  @Integer d1 d2 (\s i -> concat $ replicate (fromIntegral i) s)

    , liftD2 @Integer @Int     d1 d2 (\x y -> fromIntegral x * y)
    , liftD2 @Integer @String  d1 d2 (\i s -> concat $ replicate (fromIntegral i) s)
    ]
  where
    err = error "bad types for dynMult"
