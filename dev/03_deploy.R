# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
#devtools::check()
#rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
#pkgbuild::build()

# renv::activate(profile = "full")
# Use this to create a production-ready Docker image, with minimal number of dependencies:
renv::activate(profile = "minimal")

# ignore certain packages in the 'minimal' renv environment:
renv::settings$ignored.packages(c("pak", "pkgdown", "kableExtra", "usethis", "cowplot"))
utils::install.packages("pkgload")
renv::snapshot()
# renv::install("pkgload")

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv(lockfile = "renv.lock", repos = "https://packagemanager.posit.co/cran/2023-11-01")

## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy(lockfile = "renv.lock", repos = "https://packagemanager.posit.co/cran/2023-11-01")

