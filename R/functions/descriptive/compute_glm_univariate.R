

tbl_uni_e6a59sOC1P1 <- e6a59sOC1P1 |>
  tbl_uvregression(
    y = nonReponseTTTnutP1,
    include = c(trancheAge6a59, sexe, zoneResidence, tAdm, TTTnutE6a59sOP1, ptZscoreError,
                # f75xPropFreqInexact, f75xPropQteInf, f75xPropQteSup, f75xPropAdherenceOMSMoins, f75xPropAdherenceOMSPlus,
                # plaFamilPropOui, 
                f75xPropFreqInexact.2, f75xPropQteInf.2, f75xPropQteSup.2, f75xPropAdherenceOMSMoins.2,
                f75xPropAdherenceOMSPlus.2, plaFamilPropOui.2,
                gastroenterite, choc, pneumoVirBact, paluGraveAnemique2, paluGraveNeurologique2,
                autrePaluGrave2, anemie, candidoseDigestive, tuberculose, VIH),
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
  ) |>
  add_nevent(location = "level") |>
  bold_labels() |>
  modify_column_hide("stat_n")



print(tbl_uni_e6a59sOC1P1)