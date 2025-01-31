devtools::load_all()
library(dplyr)
library(tidyr)

clinsight_data <- clinsightful_data 
clinsight_names <- metadata$column_names

varnames <- metadata$items_expanded |> 
  distinct(var, item_name) |> 
  filter(
    n() == 1 | grepl("LBORRES$|VSORRES$", var), 
    .by = item_name
  ) 

cd_new <- dplyr::left_join(varnames, clinsight_data, by = "item_name")

labvars <- c(
  "LBORRES" = "item_value", 
  "LBORNR_Lower" = "upper_lim", 
  "LBORNR_Upper" = "lower_lim", 
  "LBORRESU" = "item_unit", 
  "LBCLSIG"  = "significance", 
  "LBREASND" = "reason_notdone"
)
names(labvars)

lab_data <- cd_new |> 
  filter(grepl("_LBORRES$|VSORRES", var)) |> 
  rename(all_of(labvars)) |>
  mutate(
    var = gsub("_VSORRES$|_LBORRES$", "", var)
  ) |> 
  mutate(across(all_of(names(labvars)), as.character)) |> 
  pivot_longer(
    all_of(names(labvars)),
    names_to = "suffix", values_to = "item_value"
    ) |> 
  mutate(
    var = ifelse(is.na(suffix), var, paste0(var, "_", suffix))
  ) 

other_data <- cd_new |> 
  filter(!grepl("_LBORRES$|VSORRES", var)) 

all_data <- dplyr::bind_rows(other_data, lab_data) |> 
  # remove columns that are created during metadata merging:
  select(-region, -suffix, -db_update_time, -day, -vis_day, 
         -vis_num, -item_type, -item_group) |> 
  rename(
    c(
      all_of(setNames(clinsight_names$name_new, clinsight_names$name_raw)),
      "ItemName" = "item_name",
      "EventName" = "event_name",
      "EventLabel" = "event_label"
    )
  ) 

# check names: 
data.frame(name_raw = names(all_data)) |> 
  mutate(missing = ifelse(name_raw %in% clinsight_names$name_raw, FALSE, TRUE)) |> 
  arrange(missing)
