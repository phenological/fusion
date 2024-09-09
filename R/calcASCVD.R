#' function to calculate ASCVD risk
#'
#' @param Gender character either "F" female or "M" male
#' @param Ethnicity_African True or False
#' @param HypertensionMeds Hypertension Medication use (True or False)
#' @param Smoking Current Smoking Status (True or False)
#' @param DM Diabetes status (True or False)
#' @param Age numeric and must be between 40 to 79
#' @param TPCH numeric and must be 130 mg/dl and above
#' @param HDCH numeric and must be above 20 mg/dl
#' @param SBP numeric and must be above 90 mmHg
#' @return numeric a ASCVD risk in percentage
#'
#' @references https://pubmed.ncbi.nlm.nih.gov/24222018/
#' @references https://clincalc.com/Cardiology/ASCVD/PooledCohort.aspx
#'
#' @examples
#'
#' risk <- calcASCVD(Gender = "M", Ethnicity_African = TRUE, HypertensionMeds = FALSE, Age = 55, TPCH = 213, HDCH = 50, SBP = 120)
#' @export
#'




calcASCVD <- function(Gender = c("F", "M"),
                             Ethnicity_African = FALSE,
                             HypertensionMeds = FALSE,
                             # True or false,
                             Smoking = FALSE,
                             # True or false
                             DM = FALSE,
                             #true or false
                             Age = NULL,
                             TPCH = NULL,
                             # in mg/dl
                             HDCH = NULL,
                             # in mg/dl
                             SBP = NULL)# in mmHg
{
  if (!Gender %in% c("F", "M")) {
    stop("Gender needs to be specified as F or M only")
  }
  if(class(Age)!="numeric"){
    stop("Age must be in numeric between 40 ~ 79")
  }
  if(as.numeric(Age)<40 | as.numeric(Age)>79){
    stop("Age must be in numeric between 40 ~ 79")
  }
  Age<-as.numeric(Age)
  if(as.numeric(TPCH)<130){
    stop("TPCH must be numeric and above 130 mg/dl")
  }
  TPCH<-as.numeric(TPCH)
  if(as.numeric(HDCH)<20){
    stop("HDCH must be numeric and above 20 mg/dl")
  }
  HDCH<-as.numeric(HDCH)
  if(as.numeric(SBP)<90){
    stop("SBP must be numeric and above 90 mmHg")
  }
  SBP<-as.numeric(SBP)
  if(HypertensionMeds=="TRUE"){
    HypertensionMeds<-1
  }else{
    HypertensionMeds<-0
  }
  if(Smoking=="TRUE"){
    Smoking<-1
  }else{
    Smoking<-0
  }

  if(DM=="TRUE"){
    DM<-1
  }else{
    DM<-0
  }

  Coef_Table <- as.data.frame(setNames(data.frame(matrix(NA, nrow = 1, ncol = 17)),
                                 c("Ln Age",
                                   "Ln Age sq",
                                   "Ln TPCH (mg/dl)",
                                   "Ln Age * Ln TPCH",
                                   "Ln HDCH (mg/dl)",
                                   "Ln Age* Ln HDCH",
                                   "Log Treated SBP (mmHg)",
                                   "Log Age * Log Treated SBP",
                                   "Log Untreated SBP (mmHg)",
                                   "Log Age * Log Untreated SBP",
                                   "Current Smoker (1 = Yes, 0 = No)",
                                   "Log Age * Current Smoker",
                                   "Diabetes (1 = Yes, 0 = No)",
                                   "Individual Sum",
                                   "Mean (Coeff * Val)",
                                   "Baseline Survival",
                                   "Estimated 10Y risk of hard ASCVD")))


  if (Gender == "F" & Ethnicity_African==FALSE) {
    Coef<-Coef_Table
    Coef[1,]<-c(-29.799,4.884,13.540,-3.114,-13.578,3.149,2.019,0,1.957,0,7.574,-1.665,0.661,0,-29.18,0.9665,0)
  }
  if (Gender == "F" & Ethnicity_African ==TRUE) {
    Coef<-Coef_Table
    Coef[1,]<-c(17.114,0,0.940,0,-18.920,4.475,29.291,-6.432,27.820,-6.087,0.691,0,0.874,0,86.61,0.9533,0)
  }
  if (Gender == "M" & Ethnicity_African==FALSE) {
    Coef<-Coef_Table
    Coef[1,]<-c(12.344,0,11.853,-2.664,-7.990,1.769,1.797,0,1.764,0,7.837,-1.795,0.658,0,61.18,0.9144,0)
  }
  if (Gender == "M" & Ethnicity_African==TRUE) {
    Coef<-Coef_Table
    Coef[1,]<-c(2.469,0,0.302,0,-0.307,0,1.916,0,1.809,0,0.549,0,0.645,0,19.54,0.8954,0)
  }

  if(Gender=="F") {
    Terms = (Coef$`Ln Age` * log(Age)) + (Coef$`Ln Age sq` * (log(Age)) ^ 2) +
      (Coef$`Ln TPCH (mg/dl)` * log(TPCH)) +
      (Coef$`Ln Age * Ln TPCH` * log(Age) * log(TPCH)) +
      (Coef$`Ln HDCH (mg/dl)` * log(HDCH)) +
      (Coef$`Ln Age* Ln HDCH` * log(Age) * log(HDCH)) +
      (Coef$`Current Smoker (1 = Yes, 0 = No)` * Smoking) +
      (Coef$`Log Age * Current Smoker` * Smoking) +
      (Coef$`Diabetes (1 = Yes, 0 = No)` * DM)

    if (HypertensionMeds==1) {
      Terms = Terms + (Coef$`Log Treated SBP (mmHg)` * log(SBP)) +
        (Coef$`Log Age * Log Treated SBP` * log(SBP))
      ASCVD_risk<-1-Coef$`Baseline Survival`^exp(Terms-Coef$`Mean (Coeff * Val)`)
    } else{
      Terms = Terms + (Coef$`Log Untreated SBP (mmHg)` * log(SBP)) +
        (Coef$`Log Age * Log Untreated SBP` * log(SBP))
      ASCVD_risk<-1-Coef$`Baseline Survival`^exp(Terms-Coef$`Mean (Coeff * Val)`)
    }

  }else{
    Terms = (Coef$`Ln Age`*log(Age))+
      (Coef$`Ln TPCH (mg/dl)`*log(TPCH))+
      (Coef$`Ln Age * Ln TPCH`* log(Age) * log(TPCH))+
      (Coef$`Ln HDCH (mg/dl)` * log(HDCH)) +
      (Coef$`Current Smoker (1 = Yes, 0 = No)` * Smoking) +
      (Coef$`Log Age * Current Smoker` * Smoking) +
      (Coef$`Diabetes (1 = Yes, 0 = No)` * DM)

    if (HypertensionMeds==1) {
      Terms = Terms + (Coef$`Log Treated SBP (mmHg)` * log(SBP))
      ASCVD_risk<-1-Coef$`Baseline Survival`^exp(Terms-Coef$`Mean (Coeff * Val)`)
    } else{
      Terms = Terms + (Coef$`Log Untreated SBP (mmHg)` * log(SBP))
      ASCVD_risk<-1-Coef$`Baseline Survival`^exp(Terms-Coef$`Mean (Coeff * Val)`)
    }


  }
  ASCVD_risk = round((ASCVD_risk)*100,1)
  return(ASCVD_risk)
}
