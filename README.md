---
editor_options: 
  markdown: 
    wrap: 72
---

# ClinSight <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/openpharma/clinsight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openpharma/clinsight/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/openpharma/clinsight/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/openpharma/clinsight/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/openpharma/clinsight/graph/badge.svg?token=T63MIKBP8L)](https://codecov.io/github/openpharma/clinsight)

<!-- badges: end -->

## Overview

The goal of `ClinSight` is to provide a production-ready and easy to use
application for medically monitoring patient safety and data integrity
during clinical trials. It provides smart and interactive
visualizations, so that a better overview can be created of each patient
during a trial.

For example, ClinSight contains an interactive timeline, which makes it
easy to relate clinical trial procedures such as treatments with adverse
events that occur within a patient.

![](images/clipboard-4158535778.png){width="700"}

Furthermore, within the application, data patterns over time within
patients can be easily visualized with interactive figures, highlighting
with different colors what data points are out of the normal limits, and
highlighting with the size of each dot whether a data point is newly
entered/changed since the last review session.

![](images/clipboard-1746986169.png){width="700"}

Data can be saved as being reviewed either by row, or by the entire form
that is active:

![](images/clipboard-504659256.png){width="700"}

The application contains a query system, in which queries can be raised
for the study site.

![](images/clipboard-2558473331.png){width="482"}

Lastly, PDF reports can be created of review activity:

with audit trail and has an option to create PDF reports of monitoring
activities.

The application is modular and can be customized for specific studies
using a metadata Excel file, that controls which data will be shown in
which tab/form in the application.

## Installation

There are several ways to install `ClinSight`. To run the application
with the intended R environment, you can git clone the package
repository, then open the project (`clinsight.Rproj`) and restore the
project environment with:

```{r}
renv::restore()
```

This will install the packages with their intended version. Note that
the R version needs to match the R version in the `renv.lock` file,
otherwise errors might occur during installation.

Alternatively, you can simply install the latest stable version of
ClinSight directly from [GitHub](https://github.com/) with:

```{r}
# install.packages("pak")
pak::pkg_install("openpharma/clinsight")
```

Or install the development version with the latest features and updates:

```{r }
pak::pkg_install("openpharma/clinsight@dev")
```

After installation, the application can be tested with internal data
using the function `test_clinsight()`.

```         
library(clinsight)
test_clinsight()
```
