# BiocProject 0.0.5

## Unreleased

## Added

* methods: `is` (overwrites this method bahavior just for the class `Annotated`), `.is.project`, `samples`, `config` for signiture `Annotated`
* functions: `.insertPEP` and `BiocProject` (the workhorse of the package)

## Changed

* **complete concept redesign**: no `BiocProject` class. The objects returned by the custom data reading function have to be of class `Annotated` and the `PEP` is inserted as the first element of its `metadata()` list

# BiocProject 0.0.4

## 2019-01-25

## Changed

* better custom data loading function error/warning communication
* all exceptions are caught with `BiocProject` constructor
* fix `.updateSubconfig(.Object@config, sp) : Subproject not found:` warning in `toProject` method

# BiocProject 0.0.3

## 2018-12-21

## Changed

* the default values for all optional arguments are `NULL`
* change `lambda function` to `anonymous function`

# BiocProject 0.0.2

## 2018-12-01

## Changed

* the object constructor does not fail if the `pepr::Project` object is provided in the `funcArgs` arguments list
* if the user-supplied function errors or throws a warning, appropriate messages are nicely displayed
* errors and warnings (if any) are returned instead of the data
* if the object constructor can't find the function file, the message is more informative

# BiocProject 0.0.1

## 2018-11-20

## Added

* add `BiocProject::BiocProject` constructor function
* add `BiocProject::getData` method to extract the data from the object
* allow for passing additional arguments for user-provided functions in `BiocProject::BiocProject`
* allow to use lambda functions with `func` parameter

## Changed

* make `BiocProject` class inherit from `pepr::Project` and `base::list`
* the `initialize` method can read in the data with the provided `func`
* the object constructor does not fail if the `pepr::Project` object is provided in the `funcArgs` arguments list