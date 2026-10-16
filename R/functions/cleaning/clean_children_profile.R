
clean_children_profile <- function(children_profile_data){
  
  data_cleaned <- children_profile_data |>
    
    filter(!is.na(id)) |>
    
    mutate(
      
      locality = case_when(
        zoneResidence == "urbaine" ~ "Urban",
        zoneResidence == "rurale" ~ "Rural",
        .default = NA
      ),
      locality = relevel(factor(locality), ref = "Urban"),
      
      # Admission status
      admission_type = case_when(
        tAdmCorrige == "nouvelle" ~ "New admission",
        tAdmCorrige == "readmission" ~ "Readmission",
        .default = NA
      ),
      admission_type = relevel(factor(admission_type), ref = "New admission"),
      
      # Relapse status
      relapse_status = case_when(
        rechuCorrige == "oui" ~ "Yes",
        rechuCorrige == "non" ~ "No",
        .default = NA
      ),
      relapse_status = relevel(factor(relapse_status), ref = "No"),
      
      # Gender
      gender = case_when(
        sexe == "F" ~ "Female",
        sexe == "M" ~ "Male",
        .default = NA
      ),
      gender = relevel(factor(gender), ref = "Male"),
      
      # Breastfeeding Status
      is_breastfed = case_when(
        allait   == "oui" ~ "Yes",
        allait   == "non" ~ "No",
        .default = NA
      ),
      is_breastfed = relevel(factor(is_breastfed), ref = "Yes"),
      
      # Vaccination Status
      # is_vaccinated
      vaccination_status  = case_when(
        vaccAJ == "oui" ~ "Up to date",
        vaccAJ == "non" ~ "Out of date",
        .default = NA
      ),
      vaccination_status = relevel(factor(vaccination_status), ref = "Up to date"),
      
      # Discharge type
      discharge_type = case_when(
        modeSorCorrige == "guerit/tranfert" ~ "Recovered/Transferred",
        modeSorCorrige == "abandon" ~ "Absconded",
        modeSorCorrige == "ref_medicale" ~ "Medical referral",
        modeSorCorrige == "decede" ~ "Death",
        .default = NA
      ),
      discharge_type = relevel(factor(discharge_type), ref = "Recovered/Transferred"),
      
      # ,sexe,rechuCorrige,allait,vaccAJ,modeSorCorrige
    ) |>

    arrange(id)
  
  return(data_cleaned)
}