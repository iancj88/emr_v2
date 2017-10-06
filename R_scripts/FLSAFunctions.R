
AddFLSAColumns <- function(df_allee) {
  # 
  #
  #
  require(stringr)
  
  df_allee <- AddIsApplicableBool(df_allee)       # mark non-job rows as not applicable to FLSA 
  df_allee <- AddIsExemptBool(df_allee)           # determine if the row is currently being treated non-exempt
  df_allee <- AddTeacherExemption(df_allee)       # determine if exempt for teaching
  df_allee <- AddSeasonExemption(df_allee)        # determine if exempt for seasonal worker
  df_allee <- AddStudentExemption(df_allee)       # determine if exempt for student status
  df_allee <- AddFailsSalaryThreshold(df_allee)   # determine if the job is treated as non-exempt but fails the salary test
  #df_allee <- AddAgExempt(df_allee)              Currently no way to determine this from banner dataset
  
  return(df_allee)
}

AddIsApplicableBool <- function(df) {
  
  # create vectors of values that indicate that a row is inapplicable
  #   to FLSA analysis
  unapplicable_position_numbers <- c("4ONEPY", "4TERMS", "4ADCMP", "4OEHHD")
  unapplicable_suffixes <- c("CR", "SD", "SE", "SF", "OL", "CA")
  unapplicable_eclses <- c("TF", "PH")
  
  #get boolean vectors that show which are good, i.e. applicable, rows
  good_posns <- !df$`Position Number` %in% unapplicable_position_numbers
  good_suffixes <- !df$Suffix %in% unapplicable_suffixes
  good_eclses <- !(df$`PEAEMPL ECLS` %in% unapplicable_eclses | 
                   df$Jobs_Ecls %in% unapplicable_eclses)
  
  # multiply the vectors together to reveal which rows are 
  #   applicable versus inapplicable
  good_rows <- good_posns * good_suffixes * good_eclses
  
  df$IsFLSAApplicable <- good_rows
  return(df)
}

AddIsExemptBool <- function(df) {
  
  # these eclasses are deemed non-exempt in Banner
  nonexempt_eclses <- c("FH", "HF", "HP", "HV", "SF", 
                        "SP", "TH", "TM", "1S", "1H")
  
  # make a vector of appropriate eclses
  jobs_ecls_indexes <- which(!is.na(df$Jobs_Ecls))
  ecls_noNA <- df$`PEAEMPL ECLS`
  ecls_noNA[jobs_ecls_indexes] <- as.vector(df$Jobs_Ecls[jobs_ecls_indexes])
  df$FinalEcls <- ecls_noNA
  
  # test the ecls vector against the nonexempt eclass vector
  is_nonexempt <- ecls_noNA %in% nonexempt_eclses
  
  df$IsFLSAExemptFromOT <- !is_nonexempt
  
  return(df)
}

AddTeacherExemption <- function(df) {
  # determine if the position has teaching duties based on teh 
  df$IsTeacherExempt <- str_detect(df$`Position Number`, 
                                        "^4[X,F,A]")
  return(df)
}

AddSeasonExemption <- function(df) {
  # Ensure that the Job eclass is used when available. Default to PEAEMPLE
  #   in all other cases.
  peample_ecls_indexes <- which(is.na(df$Jobs_Ecls))
  df_ecls <- as.character(df$Jobs_Ecls)
  df_ecls[peample_ecls_indexes] <- as.character(df[peample_ecls_indexes, "PEAEMPL ECLS"])
  
  seasonal_vector <- df_ecls == "TS"
  df$IsSeasonalExempt <- seasonal_vector
  return(df)
}

AddStudentExemption <- function(df) {
  #
  df$IsStudentExempt <- str_detect(df$`Position Number`, 
                                   "^4[S,D]")
  return(df)
}

AddFailsSalaryThreshold <- function(df) {
  salary_threshold <- 23660
  
  # determine which rows are not exempt from flsa
  flsa_sal_test <- (df$IsFLSAApplicable * df$IsFLSAExemptFromOT * !df$IsTeacherExempt * !df$IsSeasonalExempt * !df$IsStudentExempt)
  
  fails_sal_test <- (df$`Annual Salary` < salary_threshold) & flsa_sal_test
  df$FailsFLSASalaryThreshold <- fails_sal_test
  
  return(df)
}

