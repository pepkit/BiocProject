# BiocProject 0.0.1

## TBA

## Added

* add `BiocProject::BiocProject` constructor function
* add `BiocProject::getData` method to extract the data from the object
* allow for passing additional arguments for user-provided functions in `BiocProject::BiocProject`
* allow to use lambda functions with `func` parameter

## Changed

* make `BiocProject` class inherit from `pepr::Project` and `base::list`
* the `initialize` method can read in the data with the provided `func`
* the object constructor does not fail if the `pepr::Project` object is provided in the `funcArgs` arguments list
