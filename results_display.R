# Load necessary packages
library(SqlRender) # SQL editor
library(DatabaseConnector) # SQL connector
library(ggplot2)
library(dplyr)
library(tensorflow)
library(data.table)
library(gt) # nicer output format
library(PatientLevelPrediction)
library(PredictionComparison)

# original data
plpdata1 = PatientLevelPrediction::loadPlpData("stroke_in_af_data1")
plpData2 = PatientLevelPrediction::loadPlpData("D:/andromedaTemp/PredictionsPLP/stroke_in_af_data2")

# calculate % Occurrence Rate in original population
Orate1 = length(plpData1$outcomes) / length(plpData1$cohorts)
Orate2 = length(plpData2$outcomes) / length(plpData2$cohorts)

plpdata1_LassoResult = PatientLevelPrediction::loadPlpResult("C:\\Users\\Admin_EWang17\\Desktop\\BayesMeasurements\\PredictionsPLP\\StrokeTest_Lasso\\Stroke\\plpResult")
plpdata1_GBMResult = PatientLevelPrediction::loadPlpResult("C:\\Users\\Admin_EWang17\\Desktop\\BayesMeasurements\\PredictionsPLP\\StrokeTest_GBM\\Stroke\\plpResult")
plpdata2_LassoResult = PatientLevelPrediction::loadPlpResult("D:/andromedaTemp/PredictionsPLP/StrokeTest_Lasso2/Stroke/plpResult")
plpdata2_GBMResult = PatientLevelPrediction::loadPlpResult("D:/andromedaTemp/PredictionsPLP/StrokeTest_GBM2/Stroke/plpResult")

viewPlp(runPlp=plpdata1_LassoResult)
viewPlp(runPlp=plpdata1_GBMResult)
viewPlp(runPlp=plpdata2_LassoResult)
viewPlp(runPlp=plpdata2_GBMResult)

NetBenefit_lasso = extractNetBenefit(plpdata1_LassoResult)
NetBenefit_GMB = extractNetBenefit(plpdata1_GBMResult)
NetBenefit_lasso2 = extractNetBenefit(plpdata2_LassoResult)

IDI(plpModel1 = plpdata1_LassoResult, plpModel2 = plpdata1_GBMResult)
NRI(plpModel1 = plpdata1_LassoResult, plpModel2 = plpdata1_GBMResult, thresholds = seq(0, 1, 0.01))
IDI(plpModel1 = plpdata2_LassoResult, plpModel2 = plpdata2_GBMResult)
NRI(plpModel1 = plpdata2_LassoResult, plpModel2 = plpdata2_GBMResult, thresholds = seq(0, 1, 0.01))