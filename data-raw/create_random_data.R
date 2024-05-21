data_path <- file.path(Sys.getenv("RAW_DATA_PATH"), "Old/20230915")
library(dplyr)
#library(clinsight)
set.seed(2023)

raw_data <- get_raw_data(data_path, column_specs = metadata$column_specs)

length(unique(raw_data$subject_id))

####################### Randomize all subjects ####################### 
random_subjects <- raw_data |> 
  dplyr::distinct(subject_id) 
random_subjects$new_id <- as.character(sample(1:999, nrow(random_subjects), 
                                              replace = FALSE))
random_subjects$new_sitecode <-  sample(1:9, nrow(random_subjects), replace = TRUE)
  
randomized_raw_data <- raw_data |> 
  dplyr::left_join(random_subjects) |> 
  dplyr::select(
    site_code = new_sitecode, 
    subject_id = new_id, 
    dplyr::everything(), -subject_id, -site_code
    )
merged_data <- merge_meta_with_data(randomized_raw_data)

####################### Randomize continuous data #######################
random_continuous <- merged_data |> 
  dplyr::filter(item_type == "continuous") |> 
  dplyr::mutate(
    item_value = sample(item_value, dplyr::n()) |> as.numeric(),
    item_value = ifelse(item_value == 0, 0.01, item_value),
    # remove giant outliers:
    item_value = ifelse(
      is.na(upper_lim), 
      item_value,
      ifelse(
        item_value > upper_lim & (item_value/upper_lim) > 3, 
        round(runif(1, 1, 3)* upper_lim, digits = 2),
        item_value
        )
      ),
    item_value = ifelse(
      is.na(lower_lim), 
      item_value,
      ifelse(
        item_value < lower_lim & (item_value/lower_lim) < 1/3, 
        round(runif(1, 1/3, 1)* lower_lim, digits = 2),
        item_value
      )
    ),
    item_value = as.character(item_value),
    .by = c(item_name, item_group, site_code, item_unit)
  )

####################### Randomize general data #######################
random_generaldata <- merged_data |> 
  dplyr::filter(item_group == "General") |> 
  dplyr::mutate(
    item_value = ifelse(item_name %in% c("DrugAdminDose", "DrugAdminDate"), 
                        item_value, 
                        sample(item_value, dplyr::n(), replace = FALSE)),
    # use a completely random age, diagnosis, and diagnosis details:
    item_value = ifelse(item_name == "Age", 
                        as.character(sample(25:95, dplyr::n(), replace = T)), 
                        item_value),
    item_value = ifelse(item_name == "WHO.classification", 
                        sample(paste0("Syndrome ", LETTERS), dplyr::n()), 
                        item_value),
    item_value = ifelse(item_name == "WHO.subclassification", 
                        "Syndrome details", 
                        item_value),
    .by = c(item_name, event_name)
  ) 

####################### Randomize AE data #######################

SAEs <- c("Sepsis", "Pneumothorax", "Myocardial infarction", 
            "Atelectasis", "Hypertensive crisis", "Seizure", "Stroke", "Meningitis")

AEs <- c("Diarrhea", "Hypotension", "Fever", "Lower back pain", "Nausea", 
         "Allergic reaction", "Anemia", "Joint pain", "Urinary incontinence", 
         "Tachycardia", "Atrial fibrillation", "Urinary tract infection")

AES_masked <- merged_data |> 
  dplyr::filter(item_group == "Adverse events") |>
  dplyr::mutate(
    is_SAE = ifelse(
      !any(item_name == "Serious Adverse Event"), FALSE,
      ifelse(
        item_value[item_name == "Serious Adverse Event"] == "Yes",
        TRUE, 
        FALSE
      )
    ),
    .by = c(subject_id, site_code, event_repeat, event_id)
  ) |> 
  dplyr::mutate(
    item_value = ifelse(
      item_name == "AE Name", 
      ifelse(
        is_SAE, 
        sample(SAEs, size = n(), replace = TRUE), 
        sample(AEs, size = n(), replace = TRUE)
      ),
      item_value
    )
  ) |> 
  dplyr::select(-is_SAE)

####################### Randomize Medical History data #######################

mh_names <- c("Hypertension", "Diabetes mellitus", "COPD", "Atrial fibrillation", 
              "Hypothyroidism", "Congestive heart failure", 
              "Familial Mediterranean fever", "Malaria", "Osteoporosis", 
              "Arhtritis", "Migraine", "Chronic hepatitis", "Epilepsy", 
              "Sickle cell anemia")
mh_masked <- merged_data |> 
  dplyr::filter(item_group == "Medical History") |> 
  dplyr::mutate(
    item_value = case_when(
      item_name == "MH Name" ~ sample(mh_names, size = n(), replace = TRUE),
      item_name == "MH Comment" & !is.na(item_value) ~ "Test comment",
      TRUE  ~ item_value
    )
  )


####################### Randomize Medication data #######################

medication <- c("Metformin", "Acetaminophen", "Paracetamol", "Ibuprofen", 
                "Oxycodone", "Hydroxychloroquine", "Metoprolol", "Lisinopril", 
                "Loratadine", "Pantoprazole", "Meloxicam", "Mefloquine", 
                "Canakinumab")
# I needed inspiration. Names below created by using a 
# random drug name generator: https://perchance.org/fake-drug-name
mednames <- c("bolidermin", "sulfatoxipladib", "nifurasiban", "guanatoran",
              "salsteridone", "bolidilanil", "gliigestatrilat", "predacortef",
              "gliaetanide", "sulfaipoetin", "estrizotan", "ioidiliterol", 
              "rifatrodast", "sulfaipoetin") |> 
  tools::toTitleCase()

indications <- c("Pain", "Nausea", "Prophylaxis", "Insomnia", "Adverse event", 
                 "Infection", "Hypertension")

medication_masked <- merged_data |> 
  dplyr::filter(item_group == "Medication") |> 
  dplyr::mutate(
    item_value = case_when(
      item_name == "CM Trade Name" ~ sample(mednames, size = n(), replace = TRUE), 
      item_name == "CM Active Ingredient" ~ sample(medication, size = n(), replace = TRUE),
      item_name == "CM Indication" ~ sample(indications, size = n(), replace = TRUE),
      item_name == "CM Related Medical History" ~ ifelse(
        is.na(item_value) | !subject_id %in% unique(mh_masked$subject_id), 
        "", 
        sample(
          mh_masked[
            mh_masked$subject_id == unique(.data[["subject_id"]]) & 
              mh_masked$item_name == "MH Name", 
          ]$item_value,
          size = n(), 
          replace = TRUE
        )
      ),
      item_name == "CM Related AE" ~ ifelse(
        is.na(item_value) | !subject_id %in% AES_masked$subject_id,
        "",
        sample(
          AES_masked[
            AES_masked$subject_id == unique(.data[["subject_id"]]) &
              AES_masked$item_name == "AE Name",
          ]$item_value,
          size = n(),
          replace = TRUE
        )
      ),
      TRUE  ~ item_value
    ),
    .by = subject_id
  )


####################### Randomize Conc. Procedures #######################
cp_names <- c("Aortica valve replacement", "Thrombolysis", "CABG", 
              "Amputation", "Cholecystectomy", "Craniotomy", "Appendectomy")
cp_masked <- merged_data |> 
  dplyr::filter(item_group == "Conc. Procedures") |> 
  dplyr::mutate(
    item_value = case_when(
      item_name == "CP Name" ~ sample(cp_names, size = n(), replace = TRUE),
      item_name == "CP Indication" ~ sample(indications, size = n(), replace = TRUE),
      item_name == "CP Related Medical History" ~ ifelse(
        is.na(item_value) | !subject_id %in% mh_masked$subject_id, 
        "", 
        sample(
          mh_masked[
            mh_masked$subject_id == unique(.data[["subject_id"]]) & 
              mh_masked$item_name == "MH Name", 
          ]$item_value,
          size = n(), 
          replace = TRUE
        )
      ),
      item_name == "CP Related AE" ~ ifelse(
        is.na(item_value) | !subject_id %in% AES_masked$subject_id,
        "",
        sample(
          AES_masked[
            AES_masked$subject_id == unique(.data[["subject_id"]]) &
              AES_masked$item_name == "AE Name",
          ]$item_value,
          size = n(),
          replace = TRUE
        )
      ),
      TRUE  ~ item_value
    ),
    .by = subject_id
  )

####################### Randomize response data ####################### 
response_names <- c("Progressive disease (PD)", "Stable disease (SD)", "Cured")

response_masked <- merged_data |> 
  dplyr::filter(item_group == "Response") |> 
  dplyr::mutate(
    item_value = case_when(
      item_name == "Response" ~ sample(response_names, size = 1),
      TRUE  ~ item_value
    ),
    item_value = case_when(
      item_name == "Responder" ~ ifelse("Cured" %in% item_value, "Yes", "No"),
      TRUE ~ item_value
    ),
    .by = subject_id
  )

####################### Combine and fine-tune datasets ####################### 

random_combined <- dplyr::bind_rows(
  random_continuous, 
  random_generaldata,
  AES_masked, 
  medication_masked, 
  mh_masked, 
  cp_masked,
  response_masked
  ) |> 
  # add regions: 
  dplyr::mutate(
    region = sample(c("DEU", "NLD", "BEL"), 1, replace = TRUE),
    .by = site_code
  ) |> 
  # purely aesthetic, improve subject_id codes (adding applicable 
  # (randomly created) site and country codes):
  dplyr::mutate(
    subject_id = paste0(region, "_0", site_code, "_", subject_id),
    site_code = paste0("Site 0", site_code),
    item_value = ifelse(
      item_group == "Adverse events" & item_name == "AE Name", 
      tools::toTitleCase(tolower(item_value)), item_value
      )
  ) |> 
  # Fix eligibility (everyone with at least one study visit should be eligible):
  dplyr::mutate(
    item_value = ifelse(
      any(event_name %in% "Visit 1") & item_name == "Eligible", "Yes", item_value),
    .by = subject_id
  )

# Fix event dates (Screening should be first event date), so that they make more sense:
random_combined <- random_combined |>
  dplyr::mutate(
    date_base = as.Date(ifelse(event_name == "Screening", event_date, NA)),
    date_max = as.Date(ifelse(grepl("Visit ", event_name), max(event_date),  NA)),
    date_min = as.Date(ifelse(grepl("Visit ", event_name), min(event_date),  NA)),
  ) |>
  tidyr::fill(c(date_base, date_max, date_min), .direction = "downup") |>
  dplyr::ungroup()

random_combined <- random_combined |>
  dplyr::mutate(
    event_date = as.Date(event_date),
    event_date = ifelse(event_name == "Screening" & event_date > date_min, date_min, event_date), 
    event_date = ifelse(
        event_date < date_min | event_date > date_max,
        sample(seq(date_min, date_max, by = "day"), dplyr::n()), 
        event_date
        ) |> 
      as.Date()
  ) |> 
  dplyr::select(-date_base, -date_max, -date_min) 

#### Rename dataset and save results:
clinsightful_data <- random_combined
usethis::use_data(clinsightful_data, overwrite = TRUE)
saveRDS(clinsightful_data, test_path("fixtures/clinsightful_data.rds"))
