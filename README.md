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

# Get a sample data for SummarizedExperiment
nrows = 200
ncols = 6
counts = matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowRanges = GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                    IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                    strand=sample(c("+", "-"), 200, TRUE),
                    feature_id=sprintf("ID%03d", 1:200))
colData = DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                    row.names=LETTERS[1:6])
                    
                    
# Create simple BiocProject object
bp = BiocProject(file=projectConfig, assays=list(counts=counts), rowRanges=rowRanges, colData=colData)

# Inspect it!
bp
```
