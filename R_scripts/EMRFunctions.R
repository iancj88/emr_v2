
AddEMRColumns <- function(df) {
  df <- AddEMRJobType(df)
  df <- AddEMROrganizations(df)
  return(df)
}

AddEMROrganizations <- function(all_ee) {
  # Function to add a column of data to a dataframe corresponding to
  #   the EMR Organization of the data
  require(dplyr, read_xl, hrutilmsu)
  
  # first, load the x-walk table from the local excel file
  path_to_xwalk <- "C:/VBA_Source/Tables/TableDept.xlsx"
  emr_org_xwalk <- read_excel(path = path_to_xwalk, sheet = 1, 
                              col_names = TRUE, 
                              skip = 1)
  emr_org_xwalk <- select(emr_org_xwalk, `Dept Number`, EMROrg)
  
  #join the EMROrg to the dataset using a left join function
  all_ee <- hrutilmsu::JoinDataToDF(df = all_ee, df_lu = emr_org_xwalk, 
                          key_main = "Budget Org.", key_lu = "Dept Number")
  
  return(all_ee)
}



AddEMRJobType <- function(all_ee) {
  
  two_char_posn <- str_sub(all_ee$`Position Number`, 1, 2)
  third_char_posn <- str_sub(all_ee$`Position Number`, 3, 3)
  
  # Apply the Classified job label
  is_classified <- (two_char_posn %in% c("4M","4N"))
  all_ee[is_classified,"EMRJobType"] <- "Classified"
  
  #Apply the Fixed-Term job label
  is_fixed_term <- (two_char_posn %in% c("4M","4N") & third_char_posn %in% c("2", "3"))
  all_ee[is_fixed_term,"EMRJobType"] <- "Fixed-Term"
  
  #Apply the executive job label
  is_executive <- (two_char_posn == "4E")
  all_ee[is_executive,"EMRJobType"] <- "Executive"
  
  #Apply the faculty labels
  is_faculty_tt <- (two_char_posn %in% c("4A", "4B", "4X") & all_ee$MUS == "Y")
  is_faculty_ntt <- (two_char_posn %in% c("4A", "4B", "4X") & all_ee$MUS == "N")
  all_ee[is_faculty_tt,"EMRJobType"] <- "Faculty TT/T"
  all_ee[is_faculty_ntt,"EMRJobType"] <- "Faculty NTT"
  
  # Apply the Student and Grad Student labels
  is_student <- (two_char_posn == "4S")
  is_grad <- (two_char_posn == "4D")
  all_ee[is_student, "EMRJobType"] <- "Student"
  all_ee[is_grad, "EMRJobType"] <- "Grad Asst."
  
  # Apply the Temp job label
  is_temp <- two_char_posn %in% c("4T", "4K", "4J", "4L", "4P")
  all_ee[is_temp, "EMRJobType"] <- "Temporary"
    
  # Apply the Retiree job label
  is_retiree <- (two_char_posn == "4R")
  all_ee[is_retiree, "EMRJobType"] <- "Retiree"
  
  # Apply the Professional job label
  is_professional <- (two_char_posn %in% c("4C", "4H"))
  all_ee[is_professional, "EMRJobType"] <- "Professional"
  
  # Apply the ad hoc hourly label
  is_adhoc <- (two_char_posn %in% c("4F", "4V"))
  all_ee[is_adhoc, "EMRJobType"] <- "Ad-Hoc Hourly"
  
  # Apply the non-Job payment label
  is_non_job_payment <- (all_ee$Suffix %in% c("SD", "GP", "CR", "OT", "OL", "TF", "TM", 
                                              "LW", "TL", "TR", "RF", "OC", "L3", "GS", "SE")
                         | all_ee$`Position Number` %in% c("4ADCMP", "4ONEPY"))
  all_ee[is_non_job_payment,"EMRJobType"] <- "Non-Job Payment"
  rm(is_non_job_payment)
  
  return(all_ee)
}

