describe("add_events_to_data() works", {
  
  it("adds events to the data as expected",{
    # test that the function add_events_to_data() works as expected)
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
      add_sequence_to_name = NA_character_,
      is_baseline_event = c(TRUE, rep(FALSE, times = 9)),
      generate_labels = FALSE,
      meta_event_order = 1:10,
      add_visit_number = FALSE,
      add_event_repeat_number = FALSE
    )
    browser()
    add_events_to_data(df, events )
    
  })
  it("handles the visit numbers correctly if one subject has the same event 
     multiple times but on different days (for example, screening spread out 
     over two days)", {
       # in this case, the highest vis_day should be chosen, and the highest vis_num, within ONE patient and by each event_id
       # test that add_events_to_data handles the visit numbers correctly if one person has the same event 
       # multiple times but on different days (for example, screening spread out over two days)
      
     })
  it("handles it correctly if vis_num differs between subjects, 
     by adjusting the visit to be the highest number", {
    
  })
  it("verifies if event_id correctly identifies a single event", {
    
  })
  
  # df <- data.frame(
  #   event_id = c("SCR", "START", "UNV1", "V1", "UNV", "UNV5"),
  #   
  #   form_id form_repeat var     item_value edit_date_time      event_name_edc day    vis_day vis_num 
  # )
  
})
