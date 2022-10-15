# The ST Monad

## Haskell/ST

- Provides support for **strict** state threads.
- Lets you update-in-place, but is escapable (unlike IO).

`ST s a`<br>
- They return a value of type `a` and execute in "thread" `s`.
- All reference types are tagged with the thread, so that actions can 
  only affect references in their own "thread".

`runST :: (forall s. ST s a) -> a`<br>
- The type of the function used to escape `ST`. Type has rank-2.

## Haskell/Mutable Objects

### IORefs

- `IORef`s are just boxes containing mutable values.
- Create one as follows:

```haskell
import Data.IORef 
:t newIORef
newIORef :: a -> IO (IORef a)
box <- newIORef (4 :: Int)
```
- Results in an `IORef` initialised to value `4 :: Int`.
- Use `readIORef` to retrieve the value in it:

```haskell
:t readIORef 
readIORef :: IORef a -> IO a
readIORef box >>= print
```

- Use `modifyIORef` and `writeIORef` to change it:

```haskell
modifyIORef box (2*)
readIORef box >>= print  -- 8

writeIORef box 0
readIORef box >>= print  -- 0
```

- The only functions in `Data.IORef` that provide extra safety in 
  concurrent code are `atomicModifyIORef`, `atomicModifyIORef'` and 
  `atomicWriteIORef`.

  - For very simple situations where there's just one `IORef` meant to
    be used as a shared resource between computations.

### The ST Monad (`Control.Monad.ST`)

- An ST computation is one that uses an internal state to produce results. 
  The state it **mutable**.
- For that purpose, `Data.STRef` provides `STRef`.
- `STRef s a` is exactly like an `IORef a`, but it lives in the `ST s` monad 
  rather than `IO`.

### Mutable Data Structures 

- Mutable arrays can be found in the `vector` and `array` packages.
- Muutable hashtables in the `hashtable` package.
- In all cases mentioned, both `ST` and `IO` versions are provided.

- Unlike the `IO` monad, the `ST` monad can be escaped. Sometimes this is
  called **thawing** and **freezing** - the process of going into and out 
  of the monad.

## References 

- [Monad/ST](https://wiki.haskell.org/Monad/ST)
- [Haskell/Mutable Objects](https://en.wikibooks.org/wiki/Haskell/Mutable_objects)
- [Simple ST Monad Examples](https://www.philipzucker.com/simple-st-monad-examples/)
- [Stack Overflow: Relationship Between Unboxed Types and Strictness](https://stackoverflow.com/questions/3131554/what-is-the-relationship-between-unboxed-types-and-strictness)
