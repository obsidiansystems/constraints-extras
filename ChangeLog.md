# Revision history for constraints-extras

## 0.3.1.0

* Allow deriving instances with `deriveArgDict` for data and newtype family instances by supplying the name of one of its constructors
* Provide `ArgDict` instances for sums of functors.
* Support GHC 9.0.1

## 0.3.0.3 - 2020-06-22

* Update version bounds for GHC 8.10

## 0.3.0.2 - 2019-09-30

* Update version bounds for GHC 8.8

## 0.3.0.1 - 2019-05-17

* Drop markdown-unlit in favor of using regular "Bird"-style LHS to avoid some cross-compilation problems

## 0.3 - 2019-05-16

* Added a parameter for the type class, to allow for custom not-fully-polymorphic instances of ArgDict in cases where e.g. your key type contains dictionaries for specific classes. You will now need FlexibleInstances, MultiParamTypeClasses for the instances created by deriveArgDict.

## 0.2.3.5 - 2019-05-04

* Bumped version bounds on base and template-haskell to admit the versions from GHC 8.6.x

## 0.2.3.4 - 2019-03-22

* Added ChangeLog.md
* Replaced some occurrences of <> in Data.Constraint.Extras.TH with ++ so that the module will hopefully build with GHC 8.0.2 and 8.2.2 without needing to import Data.Semigroup.
