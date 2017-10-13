CleanupOutput <- function(df) {
  # replace all spaces with underscores and remove periods
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("\\.", "", names(df))

  # Standardize the naming scheme so that each field is categorized correctly
  df <- RenameColumn(df, "FullName", "Name_Full") %>%
        RenameColumn("First_Name", "Name_First") %>%
        RenameColumn("Last_Name", "Name_Last") %>%
        RenameColumn("KeyDate", "Key_Date") %>%
        RenameColumn("fy", "FY") %>%
        RenameColumn("FinalEcls", "Ecls_Final") %>%
        RenameColumn("Jobs_Ecls", "Ecls_NBAJOBS") %>%
        RenameColumn("PEAEMPL ECLS", "Ecls_PEAEMPL") %>%
        RenameColumn("EMRJobType", "Job_Type_EMR") %>%
        RenameColumn("EMROrg", "Org_EMR") %>%
        RenameColumn("IsFLSAApplicable", "FLSA_Applicable") %>%
        RenameColumn("IsFLSAExemptFromOT", "FLSA_Exempt_Ecls") %>%
        RenameColumn("IsTeacherExempt", "FLSA_Teach_Exempt") %>%
        RenameColumn("IsSeasonalExempt", "FLSA_Seasonal_Exempt") %>%
        RenameColumn("IsStudentExempt", "FLSA_Student_Exempt") %>%
        RenameColumn("FailsFLSASalaryThreshold", "FLSA_Fails_Wage_Test") %>%
        RenameColumn("Key", "Key_Job") %>%
        RenameColumn("IsStateFund", "Fund_State") %>%
        RenameColumn("Department", "PEAEMPL_Home_Department") %>%
        RenameColumn("Budget_Org_Long_Desc", "Job_Budget_Org_Long_Desc") %>%
        RenameColumn("Budget_Org" ,"Job_Budget_Org") %>%
        RenameColumn("ECLS_Description" , "PEAEMPL_ECLS_Description") %>%
        RenameColumn("MUS", "MUS_Contract") %>%
        RenameColumn("SOC_Code", "MUS_SOC_Code") %>%
        RenameColumn("SOC_Description", "MUS_SOC_Description") %>%
        RenameColumn("Annual_Salary", "Annual_Salary_Base") %>%
        RenameColumn("Assgn_Salary", "Assgn_Salary_Base") %>%
        RenameColumn("Hourly_Rate", "Hourly_Rate_Base") %>%
        RenameColumn("NameFull", "Full_Name")


  # TODO
  #   reorder columns to group like values. currently, the added columns are
  #   just stuck onto the end of the dataset.




  return(df)
}
