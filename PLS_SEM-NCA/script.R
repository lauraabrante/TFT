#Loading libraries
library(seminr)
library(NCA)
library(openxlsx)

#Directorio
setwd("C:/Users/aleja/OneDrive/Escritorio/Laura/TFG/Code")

#Load the data
data_csv <- read.csv(file = "data.csv", header = TRUE, sep = ";")
head(data_csv)

#Create Measurement Model (Reflective Model)
m_model <- constructs(composite("Actitud", multi_items("A", 1:3), weights = mode_A),
                      composite("Norma Subjetiva", multi_items("NS", 1:3), weights = mode_A),
                      composite("Control Conductual Percibido", multi_items("CCP", 1:3), weights = mode_A),
                      composite("Intención", multi_items("I", 1:3), weights = mode_A),
                      composite("Ausencia Burnout", multi_items("AB", 1:5), weights = mode_A),
                      composite("Sensación de Logro", multi_items("SL", 1:5), weights = mode_A),
                      composite("Ausencia Desvalorización", multi_items("AD", 1:5), weights = mode_A),
                      composite("Liga", single_item("Liga")),
                      composite("Soporte del Club", multi_items("SC", 1:8), weights = mode_A))

#Create Structural Model
s_model <- relationships(
  paths(from = c("Norma Subjetiva", "Ausencia Burnout", "Sensación de Logro",
        "Ausencia Desvalorización", "Soporte del Club"), to = "Actitud"),
  paths(from = c("Actitud", "Norma Subjetiva", "Control Conductual Percibido",
                 "Liga", "Soporte del Club"), to = "Intención"),
  paths(from = "Norma Subjetiva", to = "Control Conductual Percibido"),
  paths(from = "Soporte del Club", to = c("Ausencia Burnout", "Sensación de Logro",
                                          "Ausencia Desvalorización"))
)

#Estimate model
estimate_model <- estimate_pls(data = data_csv,
                               measurement_model = m_model,
                               structural_model = s_model,
                               inner_weights = path_weighting,
                               missing = mean_replacement,
                               missing_value = "-99")

#Summarize the model results
summary_model <- summary(estimate_model)

#Bootstrap the model
boot_model <- bootstrap_model(seminr_model = estimate_model,nboot = 5000)

#Summarize the bootstrap model result (10% dos colas equivale a un 5% de una cola)
summary_boot_model <- summary(boot_model, alpha = 0.1)

#Save the important results
list_of_datasets <- list("Loadings" = summary_model$loadings,
                         "Reliability" = summary_model$reliability,
                         "Discriminamt Validity_fl" = summary_model$validity$fl_criteria,
                         "Discriminant Validity_HTMT" = summary_model$validity$htmt,
                         "Discriminant Validity_intervals" = summary_boot_model$bootstrapped_HTMT,
                         "Significance paths" = summary_boot_model$bootstrapped_paths,
                         "R^2" = summary_model$paths,
                         "f^2" = summary_model$fSquare,
                         "Total indirect effects" = summary_model$total_indirect_effects
)

write.xlsx(list_of_datasets, file = "TFG.xlsx", rowNames = TRUE, colNames = TRUE)

#Inspect the collinearity
summary_model$vif_antecedents

#Model to Predictive power
predict_model <- predict_pls(
  model = estimate_model,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)

#Summarize the prediction results
summary_predidct_model <- summary(predict_model)

#Inspect the prediction results
summary_predidct_model

#---------------------------NCA Analysis-------------------------------

#Obtain the values of latent variables
scores <- estimate_model$construct_scores

#NCA Soporte del Club - Ausencia Burnout
nca_model_1 <- nca_analysis(scores, "Soporte del Club", "Ausencia Burnout",
                            test.rep = 10000)

#NCA Soporte del Club - Sensación de Logro 
nca_model_2 <- nca_analysis(scores, "Soporte del Club", "Sensación de Logro",
                            test.rep = 10000)

#NCA Soporte del Club - Ausencia Desvalorización
nca_model_3 <- nca_analysis(scores, "Soporte del Club", "Ausencia Desvalorización",
                            test.rep = 10000)

#NCA for Actitud
nca_model_4 <- nca_analysis(scores, c("Norma Subjetiva", "Ausencia Burnout", 
                                      "Sensación de Logro",
                                      "Ausencia Desvalorización",
                                      "Soporte del Club"), "Actitud",
                            test.rep = 10000)

#NCA Norma Subjetiva - Control Conductual Percibido
nca_model_5 <- nca_analysis(scores, "Norma Subjetiva", "Control Conductual Percibido",
                            test.rep = 10000)

#NCA for Intención
nca_model_6 <- nca_analysis(scores, c("Actitud", "Norma Subjetiva",
                                      "Control Conductual Percibido", 
                                      "Liga","Soporte del Club"), "Intención",
                            test.rep = 10000)

#Create a List of models
nca_models <- list(nca_model_1, nca_model_2, nca_model_3, nca_model_4,
                   nca_model_5, nca_model_6)

#Print the results
for (i in nca_models) {
  print(i)
  nca_output(i, plots = TRUE, bottlenecks = TRUE, summaries = FALSE)
}
