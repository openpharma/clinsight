# clinsight (development version)

## Developer notes

- It is now easier to adjust the status label of each participant (#217).

## Changed

- Added options to review on form level. With form-level review, subject-level graphics and tiles will be hidden, and all data will be shown in the tables, and review is enable on all rows. All data in a form can be reviewed at once; if a user tries to do so, an additional confirmation will be requested (#198).
- A double click on a row in the start page now shows a modal with all data that needs review instead of directly going to the first page of the patient even if there is no new data on that page (#216).
- Removed the requirement for common_forms to have a 'Name' column. In addition, a 'Name' column can be provided in study data tabs, indicating a common name per row, which will improve the query selector items (#207).

# clinsight 0.2.0

## Changed 

- Added a feature where, in applicable tables, a user can navigate to a form by double-clicking a table row (#103).
- Merging raw study data with metadata is made more flexible. It is now possible to run multiple user-defined, custom functions during the process of creating clinsight-compatible user data. These custom functions can be declared in the metadata's `settings` tab (#119, #120).
- Functionality is added to declare item pairs in metadata that will be merged if needed. Useful to add additional information to a variable in a table that is only sometimes available (#171).
- The standard interactive tables are now more flexible. For example, the columns can now be re-organized by the user (#124).
- Added form type as a class to be used in `create_table()` to display tables, enabling more flexibility in creating study data tables.
- The way data is stored and pulled from the database is simplified by leveraging native SQL table update logging mechanisms (#115, #135), and by reviewing records by IDs instead of subject and form    
- Reviewing data is simplified by reviewing data by records IDs instead of subject & form (#135).
- It is now much easier to select which event names should show up in the application and in which order. In addition, it is easier to edit the event's short and long labels (#140).
- Query handling is now a configurable option (#156).
- Figure legend is improved to display 'significance pending' instead of 'significance unknown' (#154).
- It is now possible to enable a button for downloading tables in ClinSight (#153). 
- The interactive timeline now has more consistent labels, will center an item on click, and has customizable treatment labels (by setting `settings$treatment_label` in the metadata) (#152).
- Filters in mod_study_forms are now only triggered after a delay, improving the user experience when trying to select/deselect multiple items (#168).
- A company-independent graphic is now shown as a logo in the top left corner (#184).
- The version of ClinSight will now show up in the application's sidebar (#194).
- Settings and data/clinsight information in the sidebar is now aligned at the bottom (#194).
- Tables in all forms now keep showing data of the active subject first by default, even when changing table view to show all subject's data in the table (#190).
- Engineered a `study_name` field in `meta$settings` to display an official study name in the app (#197).

## Bug fixes

- The test-coverage GHA workflow is updated so that codecov uploads work again (#139).
- Display all rows for tables where `Scroller` is disabled (#150).
- Tables with continuous data now show reason for missing data again when this information is new, instead of showing `NA` (#168). 
- Fixed ordering for adverse events when bold HTML tags are added. Adverse events now show the newest event first again by default (#168).
- Fixed issue that newly added columns for the SAE table do not show up (#206).
- Items that are not yet reviewed and are displayed in bold will now again show up as intended, by removing some custom logic in `create_table` (#168). 


## Developer notes

- Added a more recent repo snapshot for `chromote` v0.5.0 used in Shiny tests (#180). This [resolves errors](https://github.com/rstudio/chromote/issues/204) when using the latest version of Chrome (v135 or later) for shinytest2 tests. 
- Added the `test_clinsight()` function for developing and testing custom data and metadata for use with ClinSight (#185).
- From now on,the new Chrome headless browser mode will be used for `shinytest2` tests so that unit tests can be run with Chrome v132 or later (#161). 
- Added raw data that can be used to completely recreate the internal dataset (`clinsightful_data`) with the merge functions in the package (#162).
- Refactored `mod_study_forms`, `mod_common_forms`, and `mod_review_forms_tbl`, so that they now only need data of one form instead of all study data. Moved some business logic for the form tables to helper functions for `mod_review_forms_tbl`. This reduces unnecessary refreshing of data after saving a review.
- Added two helper functions (`create_clinsight_metadata()` and `create_clinsight_config`) to create custom `ClinSight` metadata and config files (#175).
- Added a feature test for row level review (#182).
- Added a "deploy" profile for deployments. Includes additional dependencies not included in the "deploy_minimal" profile for deployments utilizing `shinymanager` or the `app.R` file, while being more restrictive than the "full" profile (#191).
- Renamed some internal review record objects for improved clarity (#202).

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
