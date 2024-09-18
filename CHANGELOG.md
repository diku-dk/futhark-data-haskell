# Revision history for futhark-data

## 1.1.1.0

* Trailing commas are now allowed in array notation.

* The `PutValue` and `PutValue1` classes now have instances for
  `Data.ByteString.Lazy.Char8.ByteString`.

## 1.1.0.1 -- 2023-03-21

* Minor fix for GHC 9.6 changes.

## 1.1.0.0 -- 2022-05-02

* String literals are now supported in the textual value format.

* The `PutValue1` typeclass now has instances for `Text` and `ByteString`.

## 1.0.3.0 -- 2021-12-06

* The `GetValue [t]` instance no longer produces an empty list on any
  non-array type.

* New typeclass `PutValue1` as a version of `PutValue` that cannot
  fail.

## 1.0.2.0 -- 2021-08-12

* Support underscores in numeric literals.

## 1.0.1.1 -- 2021-08-04

* Support the `f16` type.

## 1.0.0.1 -- 2021-06-09

* Fixed crash in value comparison.

## 1.0.0.0 -- 2021-06-08

* First version. Released on an unsuspecting world.
