# clinsight 0.1.1

## Changed 

- Added `pkgdown` GHA workflow to automatically update documentation site with pushes to `main`

## Bug fixes

- Fixed inconsistencies in app messages when saving a review for a form with items with different review states (with some items reviewed previously by a different reviewer, and some items being completely new).
- Fixed a bug where clinsight deployed with `shinyproxy` would crash when a user with non-ASCII letters in their name would attempt to login. In this new version, when using the `shinyproxy` deployment configuration, the user name is now expected to be base64 encoded, and will now be base64 encoded by `clinsight` by default, so that the app can also handle non-ASCII signs in user names that are stored in HTTP headers. To display the user name correctly, use base64 encoding in the `application.yml` in ShinyProxy settings (for example: `http-headers.X_SP_USERNAME: "#{T(java.util.Base64).getEncoder().encodeToString(oidcUser.getFullName().getBytes())}"`).

# clinsight 0.1.0

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
- Improved function `get_test_results()`.
- Merged data now has an attribute 'synch_time' which will be used to update the synchronization information in the side bar.
- user roles as defined in the config.yml will now be shown at several places in the application and will be documented alongside the user name if needed. 
- Provided scaffolding for developers who plan to deploy on Posit Connect
- Improved dataTable outputs
- Move `shinymanager` to suggests since it is optional to use it.
- Add functionality to mark which forms are required to review and which are not.
- Improved required column verification of metadata columns when reading metadata using `get_metadata()`.
- Improved visibility of queries, especially with longer query text and with smaller screen resolutions. Adds option to view query details in full screen.

## Bug fixes

- Fixed error of creating adverse events table with empty data frame input. 
- Properly handled zero regions selected in review configuration and provided user feedback.
- Fixed issue with the timeline figure when a subject had no completed events.
- Fixed pre-processing bug where `get_metadata()` would error when a metadata.xlsx tab exist, but were empty
- Fixed error when running `golem::run_dev()` when `clinsight` was not installed.
- Fixed error that the app did not recognize the user roles when using `shinymanager`. 
- Fixed issue that item names and query types were not showing up in queries in PDF report.
- Fixed an issue in the query system that duplicate item names in the same person and same form could not be distinguished from each other. 
