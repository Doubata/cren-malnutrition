
phase_configs <- list(
  
  p1 = list(
    X = c("age_group","sexe","locality","vaccAJ","tAdm",
          "phase_length","f75_to_f75f","edema_incident","length_tubefeeding","is_death_in_phase","death_day1_in_phase",
          "is_breastfed","prop_whz_error_p1","nutri_treat_p1",
          "prop_f75x_freq_less_p1","prop_f75x_freq_more_p1","prop_f75x_qty_less_p1","prop_f75x_qty_more_p1",
          "prop_f75x_adh_less_p1","prop_f75x_adh_more_p1","prop_familymeals_p1","prop_food_intolerance_p1",
          "morbidite","has_gastroenteritis","has_shock","has_pneumonia","has_severe_malaria_anemia",
          "has_neuro_malaria","has_other_severe_malaria","has_severe_malaria","has_other_anemia",
          "has_severe_anaemia","has_digestive_candidiasis","has_tuberculosis","has_hiv"),
    
    X_cat_baseline = c("age_group","sexe","locality","vaccAJ","tAdm",
                       "f75_to_f75f","edema_incident","is_death_in_phase","death_day1_in_phase",
                       "is_breastfed", "nutri_treat_p1",
                       "morbidite","has_gastroenteritis","has_shock","has_pneumonia","has_severe_malaria_anemia",
                       "has_neuro_malaria","has_other_severe_malaria","has_severe_malaria","has_other_anemia",
                       "has_severe_anaemia","has_digestive_candidiasis","has_tuberculosis","has_hiv"),
    
    X_num_baseline = c("phase_length", "length_tubefeeding", "prop_whz_error_p1",
                       "prop_f75x_freq_less_p1","prop_f75x_freq_more_p1","prop_f75x_qty_less_p1","prop_f75x_qty_more_p1",
                       "prop_f75x_adh_less_p1","prop_f75x_adh_more_p1","prop_familymeals_p1","prop_food_intolerance_p1"),
    
    X_univariate = c("age_group", "sexe", "locality", "tAdm", "nutri_treat_p1", "prop_whz_error_p1",
                     "prop_f75x_adh_less_level_p1",
                     "has_gastroenteritis","has_shock","has_pneumonia", "has_severe_malaria",
                     "has_severe_anaemia","has_digestive_candidiasis","has_tuberculosis","has_hiv"),
    
    y = "nonresponse_p1"
  )

  # pt = list(
  #   X = c("age_group","sexe","locality","vaccAJ","tAdm",
  #         "phase_length","f75_to_f75f","edema_incident","length_tubefeeding","is_death_in_phase","death_day1_in_phase",
  #         "is_breastfed","prop_whz_error_p1","nutri_treat_pt",
  #         "prop_f75x_freq_less_pt","prop_f75x_freq_more_pt","prop_f75x_qty_less_pt","prop_f75x_qty_more_pt",
  #         "prop_f75x_adh_less_pt","prop_f75x_adh_more_pt","prop_familymeals_pt","prop_food_intolerance_pt",
  #         "morbidite","has_gastroenteritis","has_shock","has_pneumonia","has_severe_malaria_anemia",
  #         "has_neuro_malaria","has_other_severe_malaria","has_severe_malaria","has_other_anemia",
  #         "has_severe_anaemia","has_digestive_candidiasis","has_tuberculosis","has_hiv"),
  #   y = "nonresponse_pt"
  # )

)

# nonReponseTTTnutP1,trancheAge6a59,sexe,zoneResidence,tAdm,TTTnutE6a59sOP1,ptZscoreError,
# f75xPropFreqInexact.2, f75xPropQteInf.2, f75xPropQteSup.2, f75xPropAdherenceOMSMoins.2,
# f75xPropAdherenceOMSPlus.2, plaFamilPropOui.2,
# gastroenterite,choc,pneumoVirBact, paluGraveAnemique2, paluGraveNeurologique2,
# anemie, candidoseDigestive

# setdiff(phase_configs$p1$X_univariate, names(cohort_nonresponse_regression_prep_c659_noedema))
# table(cohort_nonresponse_prep_c659_noedema$nonresponse_p1, phase_configs$p1$X_univariate, useNA = "ifany")

# library(dplyr)
# cohort_nonresponse_prep_c659_noedema %>%
#   select(nonresponse_p1, phase_configs$p1$X_univariate) %>% 
#   summarise(across(everything(), ~ sum(is.na(.))))


