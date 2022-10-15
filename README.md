# Haskell Type Programming Examples 

The aim of this repository is to include examples of Haskell type programming 
concepts. Examples will demonstrate how to use various Haskell language extensions, 
in addition to monads that are somehow related to modifying Haskell's default 
type-system behavior.

## Modules 

`src/DynamicLanguage.hs`<br>

- Demonstrates dynamic addition (`dynPlus`) and multiplication (`dynMult`) functions.
- Allows passing `Dynamic` objects containing heterogeneous types to the function. For
  example, you can multiple a dynamic object containing a `String` with another dynamic 
  object containing an `Integer`, which returns a new `String`.
- Required language extensions: `GADTs`, `Scopedtypedvariables`, `RankNTypes`, `TypeApplications`
