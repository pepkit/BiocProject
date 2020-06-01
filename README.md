[![Travis-CI Build Status](https://travis-ci.org/pepkit/BiocProject.svg?branch=master)](https://travis-ci.org/pepkit/BiocProject)
[![Coverage Status](https://coveralls.io/repos/github/pepkit/BiocProject/badge.svg?branch=piface)](https://coveralls.io/github/pepkit/BiocProject?branch=piface)
[![PEP compatible](http://pepkit.github.io/img/PEP-compatible-green.svg)](http://pepkit.github.io)

# Description of the BiocProject package

The BiocProject package is a [Bioconductor](https://www.bioconductor.org/)-oriented project management package. It wraps the generic [pepr](http://code.databio.org/pepr/) R package for project metadata.

**BiocProject allows you to read in project metadata and data for an entire project with a single line of R code.**

### Quick start:

Install from GitHub:

```r
devtools::install_github("pepkit/BiocProject")
```

Read in both the metadata and data by passing your [PEP configuration file](http://pep.databio.org):
```r
bp = BiocProject(file=ProjectConfig)
```

For complete documentation and vignettes, see [code.databio.org/BiocProject](http://code.databio.org/BiocProject/).
