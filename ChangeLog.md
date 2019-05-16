# Revision history for constraints-extras

## 0.3 - 2019-05-16

* Added a parameter for the type class, to allow for custom not-fully-polymorphic instances of ArgDict in cases where e.g. your key type contains dictionaries for specific classes. You will now need FlexibleInstances, MultiParamTypeClasses for the instances created by deriveArgDict.

## 0.2.3.5 - 2019-05-04

* Bumped version bounds on base and template-haskell to admit the versions from GHC 8.6.x

## 0.2.3.4 - 2019-03-22

* Added ChangeLog.md
* Replaced some occurrences of <> in Data.Constraint.Extras.TH with ++ so that the module will hopefully build with GHC 8.0.2 and 8.2.2 without needing to import Data.Semigroup.
