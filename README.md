
# ClinSight

<!-- badges: start -->
[![R-CMD-check](https://github.com/LDSamson/clinsight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LDSamson/clinsight/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/LDSamson/clinsight/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/LDSamson/clinsight/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/LDSamson/clinsight/graph/badge.svg?token=T63MIKBP8L)](https://codecov.io/github/LDSamson/clinsight)
<!-- badges: end -->

The goal of `ClinSight` is to provide a consistent platform for monitoring patient safety during clinical
trials. 

The Shiny application in the `clinsight` package provides  
visualizations and timelines that simplify comparing multiple related data points 
within and between patients in a clinical trial. The app highlights new/updated data 
and shows the clinical significance, as reported by the responsible 
investigator/clinician, of each data point that is out of range.

Furthermore, the application contains a query system with audit trail and has an 
option to create PDF reports of monitoring activities. 

The application is modular and can be customized for specific studies using a 
metadata Excel file, that controls which data will be shown in which 
tab/form in the application.

## Installation

You can install the development version of ClinSight from 
[GitHub](https://github.com/) with:

```
# install.packages("devtools")
remotes::install_github("LDSamson/clinsight")
```

To run the application with the intended R environment, you should first open 
the project (clinsight.Rproj) and then restore the project environment:

```
renv::restore()
```

After all the required packages are successfully installed, the application can 
be run in development mode using the following code:


``` 
golem::run_dev()
```

Alternatively, you can also run it with the following script: 

``` 
pkgload::load_all(".")
eval(global)
run_app()
```

If the data and the package is already loaded, a simple call to `run_app()` will 
be enough. 

More information will follow here.

