# mod_review_forms. Feature 2 | Save review of a form. As a user, I want to be able to save a review of a form in the database. After saving the review, all items of that form that are not yet reviewed should get a tag that the value was reviewed.: Scenario 1 - Save a review. Given test review data with at least an 'Adverse event' form with patient '885',and [user_name] set to 'test_name' and [user_role] to 'Medical Monitor'and [active_patient] set to '885', and [active_form] set to 'Adverse events', and [active_tab] set to 'Common forms', and [form_reviewed] set to FALSE, I expect that I can save a new review properly, with the result saved in the application being the same as the one saved in the database.

    Code
      dplyr::select(r$review_data, -timestamp)
    Output
        id subject_id event_name     item_group form_repeat               item_name
      1  1        885  Any visit Adverse events           1               AE Number
      2  2        361    Visit 5    Vital signs           4 Systolic blood pressure
        event_date      edit_date_time reviewed comment                    reviewer
      1 2023-08-15 2023-09-30 01:01:01      Yes         test_name (Medical Monitor)
      2 2023-07-01 2023-08-30 01:01:01       No                                    
        status
      1    old
      2    new

---

    Code
      dplyr::select(r$review_data, -timestamp)
    Output
        id subject_id event_name     item_group form_repeat               item_name
      1  1        885  Any visit Adverse events           1               AE Number
      2  2        361    Visit 5    Vital signs           4 Systolic blood pressure
        event_date      edit_date_time reviewed     comment
      1 2023-08-15 2023-09-30 01:01:01       No test review
      2 2023-07-01 2023-08-30 01:01:01       No            
                           reviewer status
      1 test_name (Medical Monitor)    new
      2                                new

