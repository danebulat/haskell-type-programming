# Data.Typeable and Data.Dynamic Notes

## Data.Typeable 

`Typeable` class: Associates type representations to types.

- Type representations can be compared; allows for type-safe cast operation.
- Module `Data.Dynamic` uses `Typeable` for an implementation of dynamics.
- Module `Data.Data` uses `Typeable` and type-safe cast to support the 
  "scrap your boilerplate" style of generic programming.

- Since GHC 7.10, all types automatically have a `Typeable` instance derived.<br>
  The `DeriveDataTypeable` language extensions was required in the past.

## Data.Dynamic

- Interface providing basic support for dynamic types.
- Operations of **injecting values of an arbitrary type** into a dynamically 
  typed value, `Dynamic.`
- Operatios for converting dynamic values into a concrete (monomorphic) type.

- Basic implementation:

  ```haskell
  data Dynamic where 
    Dynamic :: forall a. Typeable a -> a -> Dynamic
  ```

- An object encapsulated together with its type.

- May only represent a **monomorphic value**. A polymorphic expression causes 
  ambiguity errors.

- Showing a value of type `Dynamic` returns a pretty-printed representation of 
  the object's type - useful for debugging.

### Converting to and from Dynamic

`toDyn :: Typeable a => a -> Dynamic`
- Convert an arbitrary value into an object of type `Dynamic`.
- `Typeable` instance ensures only monomorphically-types objects can be 
  converted to `Dynamic`.
  
  ```haskell 
  -- need to give a polymorphic object a monomorphic type signature
  toDyn (id :: Int -> Int)
  ```

`fromDyn :: Typeable a => Dynamic -> a -> a`
- Takes a dynamic object and a default value.
- Converts back to an ordinary Haskell value of the correct type.

`fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a`
- Converts back to an ordinary Haskell value of the correct type.

## Misc

- **Note:** The `cast` function "checks" if the value inside a `Dynamic` 
  is a certain type, and returns the value if it's the expected type.

  ```haskell 
  cast (22   :: Integer) :: Maybe Integer  -- Just 22
  cast ("22" :: String)  :: Maybe Integer  -- Nothing
  ```

- Basic implementation of `Dynamic` and its eliminator:

  ```haskell
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE RankNTypes #-}
  
  data Dynamic where 
    Dynamic :: Typeable a => a -> Dynamic
    
  elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
  elimDynamic f (Dynamic a) = f a
  
  -- function that provides Dynamic objects
  fromDynamic :: Typeable a => Dynamic -> Maybe a
  fromDynamic = elimDynamic cast
  ```

- The data constructor's type variable must be a monomorphic type.
- The compiler needs to know the expected return type from 
  `fromDynamic`.
  - `fromDynamic` knows nothing about the object encapsulated in `Dynamic`.

```haskell 
  let d = Dynamic (1 :: Int)  -- monomorphic type 
  fromDynamic d :: Maybe Int
  
  -- alternatively
  (elimDynamic cast d) :: Maybe Int 
  
  -- or 
  :set -XTypeApplications
  fromDynamic @Int d
  ```
