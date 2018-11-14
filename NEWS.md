# BiocProject 0.0.1

## TBA

## Added

* add `BiocProject::BiocProject` constructor function
* add `BiocProject::getData` method to extract the data from the object
* allow for passing additional metabolites for `func` in `BiocProject::BiocProject`

## Changed

* make `BiocProject` class inherit from `pepr::Project` and `base::list`
* the `initialize` method can read in the data with the provided `func`
