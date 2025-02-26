describe("add_events_to_data() works", {
  
  it("adds events to the data using declared 'event_id' as expected",{
    df <- data.frame(
      subject_id = c(rep("S1", times = 7), rep("S1", times = 7)),
      event_id = rep(c("SCR", "START", "UNV1", "V1", "UNV2", "UNV3", "V2"), times = 2),
      day = rep(c(0, 0, 2, 4, 5, 7, 8), times = 2)
    )
    # create an example events table as created with the function get_metadata:
    events <- data.frame(
      event_id = c("SCR", "START", "V1", "V2", "FU1", "FU2", "EXIT", "UNV1", "UNV2", "UNV3"),
      event_id_pattern = NA_character_,
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
      event_label_custom = NA_character_,
      event_name_custom = NA_character_,
      is_baseline_event = c(TRUE, rep(FALSE, times = 9))
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- data.frame(
      event_id   = c("SCR", "START", "UNV1", "UNV2", "UNV3", "V1", "V2"),
      event_name = c("SCR", "START", "UNV1", "UNV2", "UNV3", "V1", "V2"),
      event_label= factor(
        c("SCR", NA, NA, NA, NA, "V1", "V2"), 
        levels = c("SCR", "V1",  "V2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("If event_id is used, it keeps correct order if events on the same day or 
      the same event on multiple days occur.",{
        df <- dplyr::tribble(
          ~subject_id, ~event_id,  ~day, 
          "S1",            "SCR",    0, 
          "S1",            "SCR",    1,
          "S1",          "START",    1, 
          "S1",           "UNV1",    2, 
          "S1",             "V1",    4, 
          "S1",             "V2",    5
        )
        # create an example events table as created with the function get_metadata:
        events <- data.frame(
          event_id = c("SCR", "START", "V1", "V2", "FU1", "FU2", "EXIT", "UNV1", "UNV2", "UNV3"),
          event_id_pattern = NA_character_,
          is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
          event_label_custom = NA_character_,
          event_name_custom = NA_character_,
          is_baseline_event = c(TRUE, rep(FALSE, times = 9))
        ) |> 
          clean_event_metadata()
        output <- add_events_to_data(df, events)
        selected_output <- output[c("event_id", "event_name", "event_label")] |> 
          dplyr::arrange(event_id, event_name, event_label) |> 
          dplyr::distinct()
        expected_output <- dplyr::tibble(
          event_id   = c("SCR", "START", "UNV1", "V1", "V2"),
          event_name = c("SCR", "START", "UNV1", "V1", "V2"),
          event_label= factor(
            c("SCR", NA, NA, "V1", "V2"), 
            levels = c("SCR", "V1",  "V2",  "FU1", "FU2")
          )
        )
        expect_equal(selected_output, expected_output)
      })
  it("If event_id_pattern is used, the pattern will be used to match event_ids,
     and the order of events that were found in the data will be determined by 
     date of appearance", {
       df <- dplyr::tribble(
         ~subject_id, ~event_id,  ~day, 
                "S1",      "SCR",    0, 
                "S1",    "START",    0, 
                "S1",      "UNV",    2, 
                "S1",       "V1",    4, 
                "S1",      "UNV",    5, 
                "S1",      "UNV",    7, 
                "S1",       "V2",    8, 
                "S2",      "SCR",    0, 
                "S2",    "START",    0, 
                "S2",      "UNV",    4, 
                "S2",       "V1",    3, 
                "S2",      "UNV",    7, 
                "S2",       "V2",    10
       )
       # create an example events table as created with the function get_metadata:
       events <- data.frame(
         event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
         event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
         is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
         is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
         event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
         event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
       ) |> 
         clean_event_metadata()
       output <- add_events_to_data(df, events)
       selected_output <- output[c("event_id", "event_name", "event_label")] |> 
         dplyr::arrange(event_id, event_name, event_label) |> 
         dplyr::distinct()
       expected_output <- dplyr::tibble(
         event_id = c("SCR", "START", "UNV", "UNV", "UNV", "V1", "V2"),
         event_name = c("SCR", "START", "UV 1", "UV 2", "UV 3", "Visit 1", "Visit 2"),
         event_label= factor(
           c("SCR", NA, NA, NA, NA, "Vis1", "Vis2"), 
           levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
         )
       )
       expect_equal(selected_output, expected_output)
  })
  
  it("handles the visit numbers correctly if one subject has the same event 
     multiple times but on different days (for example, screening spread out 
     over two days)", {
       df <- dplyr::tribble(
         ~subject_id, ~event_id,  ~day, 
         "S1",            "SCR",    0, 
         "S1",            "SCR",    1,
         "S1",          "START",    1, 
         "S1",            "UNV",    2, 
         "S1",             "V1",    4, 
         "S1",            "UNV",    5, 
         "S1",            "UNV",    7, 
         "S1",             "V2",    8, 
         "S2",            "SCR",    0, 
         "S2",          "START",    0, 
         "S2",            "UNV",    4, 
         "S2",             "V1",    3, 
         "S2",            "UNV",    7, 
         "S2",             "V2",    10
       )
       # create an example events table as created with the function get_metadata:
       events <- data.frame(
         event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
         event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
         is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
         is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
         event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
         event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
       ) |> 
         clean_event_metadata()
       output <- add_events_to_data(df, events)
       selected_output <- output[c("event_id", "event_name", "event_label")] |> 
         dplyr::arrange(event_id, event_name, event_label) |> 
         dplyr::distinct()
       expected_output <- dplyr::tibble(
         event_id = c("SCR", "START", "UNV", "UNV", "UNV", "V1", "V2"),
         event_name = c("SCR", "START", "UV 1", "UV 2", "UV 3", "Visit 1", "Visit 2"),
         event_label= factor(
           c("SCR", NA, NA, NA, NA, "Vis1", "Vis2"), 
           levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
         )
       )
       expect_equal(selected_output, expected_output)
     })
  it("If event_id_pattern is used and two events occur on the same day, 
      it will select the least occurring order number per event.", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, 
      "S1",      "SCR",    0, 
      "S1",      "SCR",    1,
      "S1",    "START",    1, 
      "S1",      "UNV",    2, 
      "S1",       "V1",    4, 
      "S1",       "V2",    4, 
      "S2",      "SCR",    0, 
      "S2",      "UNV",    4, 
      "S2",       "V1",    3, 
      "S2",      "UNV",    7, 
      "S2",       "V2",    10
    )
    events <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    ) |> 
      clean_event_metadata()
    expect_output(
      output <- add_events_to_data(df, events),
      "Event order is not unique based on event dates" 
    )
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- dplyr::tibble(
      event_id = c("SCR", "START", "UNV", "UNV", "V1", "V2"),
      event_name = c("SCR", "START", "UV 1", "UV 2", "Visit 1", "Visit 2"),
      event_label= factor(
        c("SCR", NA, NA, NA, "Vis1", "Vis2"), 
        levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
  it("does not error in the if baseline visit is missing", {
    df <- dplyr::tribble(
      ~subject_id, ~event_id,  ~day, 
      "S1",    "START",    1, 
      "S1",      "UNV",    2, 
      "S1",       "V1",    4, 
      "S1",       "V2",    5
    )
    events <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    ) |> 
      clean_event_metadata()
    output <- add_events_to_data(df, events)
    selected_output <- output[c("event_id", "event_name", "event_label")] |> 
      dplyr::arrange(event_id, event_name, event_label) |> 
      dplyr::distinct()
    expected_output <- dplyr::tibble(
      event_id = c("START", "UNV", "V1", "V2"),
      event_name = c("START", "UV 1", "Visit 1", "Visit 2"),
      event_label= factor(
        c(NA, NA, "Vis1", "Vis2"), 
        levels = c("SCR", "Vis1",  "Vis2",  "FU1", "FU2")
      )
    )
    expect_equal(selected_output, expected_output)
  })
})


describe("clean_event_metadata() works", {
  it("cleans event metadata as expected, adding the required columns if needed", {
    df <- data.frame(
      event_id = c("SCR", "START", NA, "FU1", "FU2", "EXIT", NA),
      event_id_pattern = c(NA, NA, "^V[[:digit:]]+$", NA, NA, NA, "^UNV"),
      is_regular_visit= c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
      is_baseline_event = c(TRUE, rep(FALSE, times = 6)),
      event_name_custom = c(NA, NA, "Visit", NA, NA, NA, "UV"),
      event_label_custom = c(NA, NA, "Vis", NA, NA, NA, NA)
    )
    
    output <- clean_event_metadata(df)
    expect_equal(
      names(output), 
      c(names(df), "generate_labels", "meta_event_order", "add_visit_number", 
        "add_event_repeat_number")
      )
    expect_true(is.logical(output$add_visit_number))
    expect_true(is.logical(output$add_event_repeat_number))
    expect_true(is.logical(output$generate_labels))
    expect_true(is.logical(output$is_baseline_event))
    expect_equal(output$meta_event_order, 1:7)
  })

})