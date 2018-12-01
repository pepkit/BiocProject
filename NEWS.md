# BiocProject 0.0.2

## 01-12-2018

## Added


## Changed

* the object constructor does not fail if the `pepr::Project` object is provided in the `funcArgs` arguments list
* if the user-supplied function errors or throws a warning, appropriate messages are nicely displayed
* errors and warnings (if any) are returned instead of the data
* if the object constructor can't find the function file, the message is more informative

# BiocProject 0.0.1

## 20-11-2018

## Added

* add `BiocProject::BiocProject` constructor function
* add `BiocProject::getData` method to extract the data from the object
* allow for passing additional arguments for user-provided functions in `BiocProject::BiocProject`
* allow to use lambda functions with `func` parameter

## Changed

* make `BiocProject` class inherit from `pepr::Project` and `base::list`
* the `initialize` method can read in the data with the provided `func`
* the object constructor does not fail if the `pepr::Project` object is provided in the `funcArgs` arguments list