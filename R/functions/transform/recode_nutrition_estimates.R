
recode_nutrition_estimates <- function(data, cohort, phase=NULL){
  
  if(cohort %in% c("c659_noedema", "c659_edema")){
    
    if(phase == "1"){
      
      data <- data |>
        
        # filter(!is.na(nutri_treat_series_p1)) |>
        mutate(
          
          nutri_treat_p1 = case_when(
            nutri_treat_p1 == is.na(nutri_treat_p1) ~ "NA_character_",
            nutri_treat_p1 == "F75" ~ "F75",
            nutri_treat_p1 == "F75F" ~ "F75F",
            nutri_treat_p1 %in% c("F75-F75F", "F75F-F75") ~ "F75-F75F/F75F-F75",
            TRUE ~ "Others"
            # .default = "Others"
          ),
          nutri_treat_p1 = factor(nutri_treat_p1, levels = c("F75", "F75F", "F75-F75F/F75F-F75", "Others")),
          
          prop_f75x_adh_less_level_p1 = case_when(
            prop_f75x_adh_less_p1 == 0 ~ "0",
            prop_f75x_adh_less_p1 > 0 ~ "]0;100]",
          ),
          prop_f75x_adh_less_level_p1 = factor(prop_f75x_adh_less_level_p1, levels = c("0", "]0;100]")),
          
          prop_f75x_adh_more_level_p1 = case_when(
            prop_f75x_adh_more_p1 == 0 ~ "0",
            prop_f75x_adh_more_p1 > 0 ~ "]0;100]",
          ),
          prop_f75x_adh_more_level_p1 = factor(prop_f75x_adh_more_level_p1, levels = c("0", "]0;100]"))
          
        )
      
    }else{
      
      data <- data |>

        # filter(!is.na(nutri_treat_series_pt)) |>
        mutate(
          nutri_treat_pt = case_when(
            nutri_treat_pt == is.na(nutri_treat_pt) ~ NA_character_,
            nutri_treat_pt %in% c("F75+RUTF", "F75F+RUTF") ~ "F75+RUTF/F75F+RUTF",
            nutri_treat_pt %in% c("F100") ~ "F100",
            nutri_treat_pt %in% c("F75", "F75F") ~ "F75/F75F",
            # nutri_treat_pt %in% c("F75/F75F-F100") ~ "F75/F75F-F100",
            TRUE ~ "Others"
            # .default = "Others"
          ),
          nutri_treat_pt = factor(nutri_treat_pt, levels = c("F75+RUTF/F75F+RUTF",  "F100", "F75/F75F", "F75/F75F-F100", "Others")),
          
          prop_f75x_adh_less_level_pt = case_when(
            prop_f75x_adh_less_pt == 0 ~ "0",
            prop_f75x_adh_less_pt > 0 ~ "]0;100]",
          ),
          prop_f75x_adh_less_level_pt = factor(prop_f75x_adh_less_level_pt, levels = c("0", "]0;100]")),
          
          prop_f75x_adh_more_level_pt = case_when(
            prop_f75x_adh_more_pt == 0 ~ "0",
            prop_f75x_adh_more_pt > 0 ~ "]0;100]",
          ),
          prop_f75x_adh_more_level_pt = factor(prop_f75x_adh_more_level_pt, levels = c("0", "]0;100]")),
          
          prop_f100_adh_less_level_pt = case_when(
            prop_f100_adh_less_pt == 0 ~ "0",
            prop_f100_adh_less_pt > 0 ~ "]0;100]",
          ),
          prop_f100_adh_less_level_pt = factor(prop_f100_adh_less_level_pt, levels = c("0", "]0;100]")),
          
          prop_f100_adh_more_level_pt = case_when(
            prop_f100_adh_more_pt == 0 ~ "0",
            prop_f100_adh_more_pt > 0 ~ "]0;100]",
          ),
          prop_f100_adh_more_level_pt = factor(prop_f100_adh_more_level_pt, levels = c("0", "]0;100]"))
          
        )
      
    }
    
  }else{
    
    data <- data |>
      
      mutate(
        nutri_treat_under6 = case_when(
          nutri_treat_under6 == is.na(nutri_treat_under6) ~ NA_character_,
          nutri_treat_under6 == "F100d" ~ nutri_treat_under6,
          TRUE ~ "Others"
        ),
        nutri_treat_under6 = factor(nutri_treat_under6, levels = c("F100d", "Others"))
      )
        
    
  }
  
  
  return(data)
}

