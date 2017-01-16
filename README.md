# quickcheck-special: Edge cases and special values for QuickCheck Arbitrary instances

[![Hackage](https://img.shields.io/hackage/v/quickcheck-special.svg)](https://hackage.haskell.org/package/quickcheck-special)
[![Build Status](https://secure.travis-ci.org/minad/quickcheck-special.png?branch=master)](http://travis-ci.org/minad/quickcheck-special)

The standard Arbitrary instances of QuickCheck don't generate
special values. This is fixed by this package which provides the newtype Special
with an Arbitrary instance. The special values are given by the SpecialValues typeclass.

* https://hackage.haskell.org/package/quickcheck-special
