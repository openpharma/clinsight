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
  filter(!(item_group == "Vital signs" & !suffix %in% c("LBORRES", "LBREASND"))) |> 
  mutate(
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBORRES", "VSORRES", suffix),
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBREASND", "VSREASND", suffix),
    var = ifelse(is.na(suffix), var, paste0(var, "_", suffix))
  ) 
# significance labels need to be checked
# significance = dplyr::case_when(
# significance == "NCS"                   ~ "out of limits, clinically insignificant",
# significance == "CS"                    ~ "out of limits, clinically significant",
# is.na(out_of_lim) & is.na(significance) ~ "limits unknown",
# out_of_lim == 0                         ~ "within limits",
# is.na(significance) & out_of_lim == 1   ~ "out of limits, significance pending",
# TRUE   ~ significance


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

load_and_run_app <- function(){
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "test")
  merged_data <- all_data |> 
    merge_meta_with_data(meta = metadata)
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

