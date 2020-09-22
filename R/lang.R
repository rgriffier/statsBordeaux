#' @title Get all available languages
#' @description Get all available languages
#' @return a character vector containing all available languages
#' @export
lang.available <- function(){
  return(c('FR', 'ENG'))
}

#' @title Check if in available languages
#' @description Check if in available languages
#' @param lang a character vector of length one
#' @return a boolean vector of length one
#' @noRd
is.lang.available <- function(lang){
  if(is.null(getOption('lang.value'))){
    options('lang.value' = 'ENG')
  }

  if(!all(lang %in% lang.available())){
    stop(paste0('Language available are ', paste0("'", paste0(lang.available(), collapse = "', '"), "'")), call. = FALSE)
  }
}

#' @noRd
missingData_int <- function(){
  return(c('FR' = 'd.m.', 'ENG' = 'm.d.'))
}

#' @noRd
sampleSize_int <- function(){
  return(c('FR' = 'N', 'ENG' = 'N'))
}

#' @noRd
mean_int <- function(){
  return(c('FR' = 'Moyenne', 'ENG' = 'Mean'))
}

#' @noRd
stdv_int <- function(){
  return(c('FR' = 'Écart-type', 'ENG' = 'SD'))
}

#' @noRd
confint_int <- function(){
  return(c('FR' = 'IC95%', 'ENG' = '95% CI'))
}

#' @noRd
median_int <- function(){
  return(c('FR' = 'Médianne', 'ENG' = 'Median'))
}

#' @noRd
range_int <- function(){
  return(c('FR' = 'Min ; Max', 'ENG' = 'Min ; Max'))
}

#' @noRd
mode_int <- function(){
  return(c('FR' = 'Mode', 'ENG' = 'Mode'))
}

#' @noRd
allModalities_int <- function(){
  return(c('FR' = 'Toutes les modalités', 'ENG' = 'All modalities'))
}

#' @noRd
getMissingData_text <- function(lang){
  is.lang.available(lang)
  return(unname(missingData_int()[lang]))
}

#' @noRd
getSampleSize_text <- function(lang){
  is.lang.available(lang)
  return(unname(sampleSize_int()[lang]))
}

#' @noRd
getSampleSizeMissingData_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getSampleSize_text(lang), ' (', getMissingData_text(lang), ')'))
}

#' @noRd
getSampleSizeMissingDataPercent_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getSampleSize_text(lang), ' (', getMissingData_text(lang), ' ; %)'))
}

#' @noRd
getModailtySizePercent_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getSampleSize_text(lang), ' (%)'))
}

#' @noRd
getMean_text <- function(lang){
  is.lang.available(lang)
  return(unname(mean_int()[lang]))
}

#' @noRd
getStdv_text <- function(lang){
  is.lang.available(lang)
  return(unname(stdv_int()[lang]))
}

#' @noRd
getConfint_text <- function(lang){
  is.lang.available(lang)
  return(unname(confint_int()[lang]))
}

#' @noRd
getMeanStdv_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getMean_text(lang), ' (', getStdv_text(lang), ')'))
}

#' @noRd
getConfintMean_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getConfint_text(lang), ' [', getMean_text(lang), ']'))
}

#' @noRd
getMedian_text <- function(lang){
  is.lang.available(lang)
  return(unname(median_int()[lang]))
}

#' @noRd
getMedianIRQ_text <- function(lang){
  is.lang.available(lang)
  return(paste0(getMedian_text(lang), ' [Q1 ; Q3]'))
}

#' @noRd
getRange_text <- function(lang){
  is.lang.available(lang)
  return(unname(range_int()[lang]))
}

#' @noRd
getMode_text <- function(lang){
  is.lang.available(lang)
  return(unname(mode_int()[lang]))
}

#' @noRd
getAllModailties_text <- function(lang){
  is.lang.available(lang)
  return(unname(allModalities_int()[lang]))
}

#' @noRd
getNormalityInterpretation_text <- function(lang){
  is.lang.available(lang)
  if(lang == 'FR'){
    return('H0 : la variable suit une loi normale.\nH1 : la variable ne suit pas une loi normale.\np-value < 0,05 : on rejette H0, la variable ne suit pas une loi normale')
  } else if (lang == 'ENG'){
    return('H0: the variable follows a normal distribution.\nH1: the variable does not follow a normal distribution.\np-value < 0.05: we reject H0, the variable does not follow a normal distribution')
  }
}

#' @noRd
inequalVarianceStudent_int <- function(){
  return(c('FR' = 'Test de Student pour variances inégales', 'ENG' = 't-test for inequal variances'))
}

#' @noRd
equalVarianceStudent_int <- function(){
  return(c('FR' = 'Test de Student pour variances égales', 'ENG' = 't-test for equal variances'))
}

#' @noRd
wilcoxon_int <- function(){
  return(c('FR' = 'Test de Wilcoxon', 'ENG' = 'Wilcoxon test'))
}

#' @noRd
anova_int <- function(){
  return(c('FR' = 'Analyse de la variance (ANOVA)', 'ENG' = 'Analysis of variance (ANOVA)'))
}

#' @noRd
kruskal_int <- function(){
  return(c('FR' = 'Test de Kruskal-Wallis', 'ENG' = 'Kruskal-Wallis test'))
}

#' @noRd
chiSquared_int <- function(){
  return(c('FR' = 'Test du Chi²', 'ENG' = "Pearson's Chi-squared test"))
}

#' @noRd
chiSquaredYates_int <- function(){
  return(c('FR' = 'Test du Chi² avec correction de Yates', 'ENG' = "Pearson's Chi-squared test with Yates' continuity correction"))
}

#' @noRd
fisher_int <- function(){
  return(c('FR' = 'Test exact de Fisher', 'ENG' = "Fisher's Exact Test for Count Data"))
}

#' @noRd
getInequalVarianceStudent_text <- function(lang){
  is.lang.available(lang)
  return(unname(inequalVarianceStudent_int()[lang]))
}

#' @noRd
getEqualVarianceStudent_text <- function(lang){
  is.lang.available(lang)
  return(unname(equalVarianceStudent_int()[lang]))
}

#' @noRd
getWilcoxon_text <- function(lang){
  is.lang.available(lang)
  return(unname(wilcoxon_int()[lang]))
}

#' @noRd
getAnova_text <- function(lang){
  is.lang.available(lang)
  return(unname(anova_int()[lang]))
}

#' @noRd
getKruskal_text <- function(lang){
  is.lang.available(lang)
  return(unname(kruskal_int()[lang]))
}

#' @noRd
getChiSquared_text <- function(lang){
  is.lang.available(lang)
  return(unname(chiSquared_int()[lang]))
}

#' @noRd
getChiSquaredYates_text <- function(lang){
  is.lang.available(lang)
  return(unname(chiSquaredYates_int()[lang]))
}

#' @noRd
getFisher_text <- function(lang){
  is.lang.available(lang)
  return(unname(fisher_int()[lang]))
}

#' @noRd
variabililtyNeeded_int <- function(){
  return(c('FR' = 'Variabilité insuffisante pour réaliser un test',
           'ENG' = "Insufficient variability to perform test"))
}

#' @noRd
smallSample_int <- function(){
  return(c('FR' = 'Effectif trop faible pour réaliser un test',
           'ENG' = "Sample size not large enough to perform test"))
}

#' @noRd
notEnought_int <- function(){
  return(c('FR' = 'Pas assez de modalitées pour réaliser un test',
           'ENG' = "Not enough modality to perform test"))
}

#' @noRd
getVariabililtyNeeded_text <- function(lang){
  is.lang.available(lang)
  return(unname(variabililtyNeeded_int()[lang]))
}

#' @noRd
getSmallSample_text <- function(lang){
  is.lang.available(lang)
  return(unname(smallSample_int()[lang]))
}

#' @noRd
getNotEnought_text <- function(lang){
  is.lang.available(lang)
  return(unname(notEnought_int()[lang]))
}

#' @noRd
sampleSizeNote_int <- function(){
  return(c('FR' = paste0(sampleSize_int()['FR'], ' : effectif'),
           'ENG' = paste0(sampleSize_int()['ENG'], ': sample size')))
}

#' @noRd
getSampleSizeNote_text <- function(lang){
  is.lang.available(lang)
  return(unname(sampleSizeNote_int()[lang]))
}

#' @noRd
missingDataNote_int <- function(){
  return(c('FR' = paste0(missingData_int()['FR'], ' : données manquantes'),
           'ENG' = paste0(missingData_int()['ENG'], ': missing data')))
}

#' @noRd
getMissingDataNote_text <- function(lang){
  is.lang.available(lang)
  return(unname(missingDataNote_int()[lang]))
}

#' @noRd
qualitativeDataNote_int <- function(){
  return(c('FR' = "Les variables qualitatives sont exprimées en 'Effectif (%)'",
           'ENG' = 'Qualitative data are expressed as group size (%)'))
}

#' @noRd
getQualitativeDataNote_text <- function(lang){
  is.lang.available(lang)
  return(unname(qualitativeDataNote_int()[lang]))
}

#' @noRd
note_int <- function(){
  return(c('FR' = "Notes :",
           'ENG' = 'Notes:'))
}

#' @noRd
getNote_text <- function(lang){
  is.lang.available(lang)
  return(unname(note_int()[lang]))
}

#' @noRd
test_int <- function(){
  return(c('FR' = "Tests :",
           'ENG' = 'Tests:'))
}

#' @noRd
getTest_text <- function(lang){
  is.lang.available(lang)
  return(unname(test_int()[lang]))
}

