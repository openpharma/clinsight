# clinsight (development version)

## Changed

- Improved metadata so that external file can be used. 
- Improved data loading by using a config file.
- Created two renv profiles, one for development and one for production. Goal is 
to minimize the package dependencies of the production version.
- Removed development package dependencies (for example devtools) that were not needed to run the application.  
- Improved data anonymization.
- Changed license.
- Updated Description file.
- Improved reading of data files within clinsight::run_app()
- Improved creating test result report.
- Added data specification to `run_app()` documentation

## Bug fixes

- Fixed error of creating adverse events table with empty data frame input. 
- Properly handled zero regions selected in review configuration and provided user feedback.
