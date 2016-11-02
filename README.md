# quickcheck-special: Edge cases and special values for QuickCheck Arbitrary instances

The standard Arbitrary instances of QuickCheck don't generate
special values. This is fixed by this package which provides the newtype Special
with an Arbitrary instance. The special values are given by the SpecialValues typeclass.

* https://hackage.haskell.org/package/quickcheck-special
