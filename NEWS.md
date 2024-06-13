# clinsight (development version)

## Changed

- Created two renv profiles, one for development and one for production. Goal is 
to minimize the package dependencies of the production version.
- Removed development package dependencies (for example devtools) that were not needed to run the application.  
- Improved data anonimization.
- Changed license.

- Updated Description file.
- Improved reading of data files within clinsight::run_app()
- Improved creating test result report.

## Bug fixes

- Fixed error of creating adverse events table with empty data frame input. 
