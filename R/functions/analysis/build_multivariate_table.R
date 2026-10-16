# 
# ### Cacul de la regression logistique multivariée avec suppression des variables
# # tuberculose,VIH supprimer evenements rares
# # autrePaluGrave2 autrePaluGrave2
# 
# # Variables continues en instance
# # f75xPropFreqInexact,f75xPropQteInf,f75xPropQteSup,f75xPropAdherenceOMSMoins,
# e6a59sOC1P1.narm <- e6a59sOC1P1 %>% tidyr::drop_na(nonReponseTTTnutP1,trancheAge6a59,sexe,zoneResidence,tAdm,TTTnutE6a59sOP1,ptZscoreError,
#                                                    f75xPropFreqInexact.2, f75xPropQteInf.2, f75xPropQteSup.2, f75xPropAdherenceOMSMoins.2,
#                                                    f75xPropAdherenceOMSPlus.2, plaFamilPropOui.2,
#                                                    gastroenterite,choc,pneumoVirBact, paluGraveAnemique2, paluGraveNeurologique2,
#                                                    anemie, candidoseDigestive)
# 
# describe(e6a59sOC1P1.narm$nonReponseTTTnutP1)
# write.csv2(e6a59sOC1P1.narm, file = "C:/Users/DELL/Documents/CREN/base/e6a59sOC1P1_narm.csv", sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
# # f75xPropFreqInexact+f75xPropQteInf+f75xPropQteSup+f75xPropAdherenceOMSMoins+
# reg1.e6a59sOC1P1 <- glm(nonReponseTTTnutP1~trancheAge6a59+sexe+zoneResidence+tAdm+TTTnutE6a59sOP1+ptZscoreError+
#                           f75xPropFreqInexact.2+f75xPropQteInf.2+f75xPropQteSup.2+f75xPropAdherenceOMSMoins.2+
#                           f75xPropAdherenceOMSPlus.2+plaFamilPropOui.2+
#                           gastroenterite+choc+pneumoVirBact+paluGraveAnemique2+paluGraveNeurologique2+
#                           anemie+candidoseDigestive,
#                         data = e6a59sOC1P1.narm, family = binomial)
# 
# # 164 observations effacées parce que manquantes
# summary(reg1.e6a59sOC1P1)
# 
# reg1.e6a59sOC1P1 %>%
#   tbl_regression(exponentiate = T) %>%
#   add_significance_stars() %>%
#   modify_column_unhide(c("ci", "p.value")) %>%
#   add_nevent(location = "level") %>%
#   bold_labels()
# car::Anova(reg1.e6a59sOC1P1)
# 
# ### Ajustement du modele logistique binaire
# reg2.e6a59sOC1P1 <- step(reg1.e6a59sOC1P1)
# summary(reg2.e6a59sOC1P1)
# 
# reg2.e6a59sOC1P1 %>%
#   tbl_regression(exponentiate = TRUE) %>%
#   add_significance_stars() %>%
#   modify_column_unhide(c("ci", "p.value")) %>%
#   add_nevent(location = "level") %>%
#   bold_labels()
# car::Anova(reg2.e6a59sOC1P1)
# 
# # Resume de la regression
# tbl1_reg1_e6a59sOC1P1 <- reg1.e6a59sOC1P1 |>
#   tbl_regression(exponentiate = TRUE) |>
#   add_nevent(location = "level")
# 
# tbl2_reg2_e6a59sOC1P1 <- reg2.e6a59sOC1P1 |> 
#   tbl_regression(exponentiate = TRUE) |>
#   add_nevent(location = "level")
# 
# list(tbl_uni_e6a59sOC1P1, tbl1_reg1_e6a59sOC1P1) |>
#   tbl_merge(
#     tab_spanner = c(
#       "**Univariate analysis**",
#       "**Multivariate analysis**"
#     )
#   ) |> 
#   bold_labels()
# 
# list(tbl1_reg1_e6a59sOC1P1, tbl2_reg2_e6a59sOC1P1) |>
#   tbl_merge(
#     tab_spanner = c(
#       "**Complete Multivariate model**",
#       "**Parsimonious Multivariate model**"
#     )
#   ) |> 
#   bold_labels()
# 
# ### Evaluation des multicollinéarités par les VIF/FIV (facteurs d'inflation de la Variance)
# library(performance)
# check_collinearity(reg2.e6a59sOC1P1)
# # Alternative à check_collinearity
# library(carData)
# library(car)
# vif(reg2.e6a59sOC1P1)
# 
# ### Analyse de la variance du model de depart et du model d'arriver
# anova(reg1.e6a59sOC1P1, reg2.e6a59sOC1P1, test = "Chisq")
# 
# library(ggstats)
# ggcoef_compare(
#   list("Complete model" = reg1.e6a59sOC1P1, "Parsimonious model" = reg2.e6a59sOC1P1),
#   type = "faceted",
#   exponentiate = TRUE
# )
# 
# ### Evaluation des hypothèses de normalité et d’homoscédasticité des résidus
# library(see)
# check_model(reg2.e6a59sOC1P1)
# plot(check_normality(reg2.e6a59sOC1P1))
# 
# library(ggeffects)
# cowplot::plot_grid(plotlist = plot(ggeffect(reg2.e6a59sOC1P1)))
# 
