devtools::load_all()
library(dplyr)
library(tidyr)

clinsight_names <- metadata$column_names

varnames <- metadata$items_expanded |> 
  distinct(var, item_name) |> 
  filter(
    n() == 1 | grepl("LBORRES$|VSORRES$", var), 
    .by = item_name
  ) 

# Variables not in clinsightful_data but are mentioned in metadata.xlsx:
# dplyr::anti_join(varnames, clinsightful_data, by = "item_name")
# var            item_name              
# 1 AE_AESER_AEOUT SAE outcome            
# 2 WHO_WHOCAT     WHO.subclassification  
# 3 DMOD_DAT       DoseModificationDate   
# 4 DMOD_REAS      DoseModificationReason 
# 5 DMOD_DOSE      DoseModificationNewDose

# All items in clinsightful_data are also in metadata, as expected:
# dplyr::anti_join(clinsightful_data, varnames, by = "item_name")
# # A tibble: 0 × 24

cd_new <- dplyr::left_join(clinsightful_data, varnames, by = "item_name")

labvars <- c(
  "LBORRES" = "item_value", 
  "LBORNR_Lower" = "lower_lim", 
  "LBORNR_Upper" = "upper_lim", 
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
  filter(
    # since other vars in vital signs do not exist in the data:
    !(item_group == "Vital signs" & !suffix %in% c("LBORRES", "LBREASND")),
    # remove derived vars:
    !var == "VS_WEIGHTCHANGE" 
    ) |> 
  mutate(
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBORRES", "VSORRES", suffix),
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBREASND", "VSREASND", suffix),
    var = ifelse(is.na(suffix), var, paste0(var, "_", suffix))
  ) 

other_data <- cd_new |> 
  filter(!grepl("_LBORRES$|VSORRES", var)) |> 
  # keep item_value of course, but remove other lab values:
  select(-all_of(labvars[-1]))

all_data <- dplyr::bind_rows(other_data, lab_data) |> 
  # remove columns that are created during metadata merging:
  select(-region, -suffix, -db_update_time, -day, -vis_day, 
         -vis_num, -item_type, -item_group, -region) |> 
  mutate(
    item_value = ifelse(item_value == "(unit missing)", NA_character_, item_value),
  ) |> 
  rename(
    c(
      all_of(setNames(clinsight_names$name_new, clinsight_names$name_raw)),
      "ItemName" = "item_name",
      "EventName" = "event_name",
      "EventLabel" = "event_label"
    )
  ) |> 
  mutate(
    SiteCode = gsub("_[[:digit:]]+$", "", SubjectId),
    SiteCode = sub("_", "", SiteCode)
  )

# check names: 
data.frame(name_raw = names(all_data)) |> 
  mutate(missing = ifelse(name_raw %in% clinsight_names$name_raw, FALSE, TRUE)) |> 
  arrange(missing)

#### Verify if outcome is the same as clinsightful_data after merging:
merged_data <- all_data |> 
  merge_meta_with_data(meta = metadata)
attr(merged_data, "synch_time") <- "2023-09-15 10:10:00 UTC"

###### Check what is needed to get exactly the same dataset:
###### 

old_clinsight_data <- clinsightful_data |> 
  # db_update_time is not used anymore and should be removed 
  select(-db_update_time) |> 
  # more realistic site codes:
  mutate(
    site_code = simplify_string(gsub("_[[:digit:]]+$", "", simplify_string(subject_id))),
    site_code = toupper(sub("_", "", site_code))
  ) |> 
  # because day and vis_day are incorrect in the current clinsight_data:
  select(-day, -vis_day) |> 
  # because old weight change since screening was incorrect. 
  # In the new dataset it is actually calculated:
  filter(item_name != "Weight change since screening") |> 
  arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat)

new_clinsight_data <- merged_data |> 
  select(c(any_of(names(clinsightful_data)), "form_type")) |> 
  select(-form_type, -day, -vis_day) |> 
  filter(item_name != "Weight change since screening") |> 
  # same column order makes comparisons easier
  arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat)

waldo::compare(old_clinsight_data, new_clinsight_data)

# clinsightful_data <- merged_data 
# usethis::use_data(clinsightful_data, overwrite = TRUE)

load_and_run_app <- function(){
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "test")
  saveRDS(merged_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(metadata, file.path(temp_folder, "metadata.rds"))
  
  run_app(
    data_folder = temp_folder,
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
    })}
  )
} 

load_and_run_app()

