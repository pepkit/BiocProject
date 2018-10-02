[![Travis-CI Build Status](https://travis-ci.org/pepkit/BiocProject.svg?branch=master)](https://travis-ci.org/pepkit/BiocProject)


# BiocProject
R package that implements an interface class for the [`Project`](http://code.databio.org/pepr/) and [`SummarizedExperiment`](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) classes

## Install

```R
devtools::install_github("pepkit/BiocProject")
```

## Quick start

```R
library('BiocProject')

# Get a sample PEP project_config
projectConfig = system.file(
"extdata",
"example_peps-master",
"example_implied",
"project_config.yaml",
package = "pepr"
)

# Create simple BiocProject object
bp = BiocProject(file=projectConfig)

# Inspect it!
bp
```
