describe("add_events_to_data() works", {
  
  it("adds events to the data as expected",{
    # test that the function add_events_to_data() works as expected)
    df <- data.frame(
      event_id = c("SCR", "START", "UNV1", "V1", "UNV", "UNV5"),
      form_id = c("SCR", "START", "UNV1", "V1", "UNV", "UNV5"),
      form_repeat = c(1, 1, 1, 1, 1, 1),
      item_value = c("SCR", "START", "UNV1", "V1", "UNV", "UNV5")
    )
    # create an example events table as created with the function get_metadata:
    events <- data.frame(
      event_id = c("SCR", "START", "UNV1", "V1", "UNV", "UNV5"),
      event_name_edc = c("Screening", "Start", "Unscheduled Visit 1", "Visit 1", "Unscheduled Visit", "Unscheduled Visit 5"),
      day = c(0, 0, 0, 1, 2, 5),
      vis_day = c(0, 0, 0, 1, 2, 5),
      vis_num = c(0, 0, 0, 1, 2, 5)
    )
    add_events_to_data(df, )
    
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
