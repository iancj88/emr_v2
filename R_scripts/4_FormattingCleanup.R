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
    RenameColumn("IsStateFund", "Fund_State")

  # TODO
  #   reorder columns to group like values. currently, the added columns are
  #   just stuck onto the end of the dataset.

  return(df)
}
