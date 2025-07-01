# The columns required to define unique records are not user defined, they
# should be captured by an internal object to keep from having to simplify some
# functions.
key_columns <- c(
  "subject_id",
  "event_name",
  "item_group",
  "form_repeat",
  "item_name"
)

usethis::use_data(key_columns, overwrite = TRUE)
