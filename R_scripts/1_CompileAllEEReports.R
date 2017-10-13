

CompileAllEEReports <- function(path_to_files) {
  require(stringr)
  require(dplyr)

  # get the list of all ee wb file names and filepaths
  # the workbooks must match the following regex pattern.
  wb_pattern <- "All EE [0-9]{4}-[0-9]{2}-[0-9]{2}\\.xlsx"
  wb_names_and_filepaths <- list.files(path_to_files,
                                       full.names = TRUE,
                                       pattern = wb_pattern)
  wb_names_only <- list.files(path_to_files,
                              full.names = FALSE,
                              pattern = wb_pattern)



  #read each wb into a separate dataframe
  wb_col_types <- GetAllEEColTypes()
  all_ee_datasets <- lapply(wb_names_and_filepaths,
                            FUN = read_excel,
                            sheet = 1,
                            col_types = wb_col_types,
                            col_names = T)

  #Name the dataset list using the date in the file name,
  # this works because the files are read in the same order
  # above as the filenames are read here.

  #Remove the preceeding All EE prefix from the filename
  report_date_chr <- gsub("All EE ",
                          "",
                          wb_names_only) %>%
    #Remove the excel file type suffix from the filename
                          {gsub(".xlsx",
                                "",
                                . )}

  names(all_ee_datasets) <- report_date_chr

  #put the date (currently stored in the names of the dataframes) into a column of each dataframe
  all_ee_datasets <- mapply(FUN = AddDateVector,
                            df = all_ee_datasets,
                            dte = names(all_ee_datasets),
                            SIMPLIFY = F)

  #combine dataframes
  df_allEE <- bind_rows(all_ee_datasets)

  #pad the gids to ensure that they are properly treated as char scalars
  df_allEE$GID <- PadGID(df_allEE$GID)

  #add a full name
  df_allEE$FullName <- str_c(df_allEE$`Last Name`,
                             ", ",
                             df_allEE$`First Name`)

  #pad the suffix if necessary
  df_allEE$Suffix <- stri_replace_first_regex(str = df_allEE$Suffix,
                                              pattern = "(^[0-9]{1})$",
                                              replacement = "0$1")

  #add a unique key for each row from the GID + Position_Number + Suffix
  df_allEE$Key <- str_c(df_allEE$GID,
                        df_allEE$`Position Number`,
                        df_allEE$Suffix)

  # pad the fund if necessary (dropped leading zero)
  short_funds <- which(str_length(df_allEE$Fund) == 5)
  df_allEE[short_funds,"Fund"] <- str_c("0", df_allEE$Fund[short_funds])

  # determine if the line is state or non-state funded
  df_allEE$IsStateFund <- str_detect(df_allEE$Fund,
                                     "^41|^01|^91")

  #add a unique key for each date
  df_allEE$KeyDate <- str_c(df_allEE$Key,
                            df_allEE$Date)

  # add the fiscal year
  df_allEE$quarter_date <- quarter(df_allEE$Date)
  df_allEE$year <- year(df_allEE$Date)
  df_allEE$fy <- (df_allEE$quarter_date > 2) + df_allEE$year
  df_allEE <- select(df_allEE, -year, -quarter_date)

  #order the master dataframe by date
  df_allEE <- arrange(df_allEE, Date)

  #Add the Job Eclass data column (as opposed to PEAEMPL eclass)
  df_allEE <- AddJobEcls(df_allEE)

  #Add the EMR Specific columns
  df_allEE <- AddEMRColumns(df_allEE)

  # Add the FLSA Specific columns --
  # This must go after adding the EMR Columns!!!
  df_allEE <- AddFLSAColumns(df_allEE)

  # Remove non-Bozeman positions
  df_allEE <- filter(df_allEE, substr(df_allEE$`Position Number`, 1, 1) == "4")

  #fix the 'OTHER' org. heirarchy
  isProvost <- (df_allEE$`Org. Heirarchy` == "Other Provost")
  df_allEE[isProvost,"Org. Heirarchy"] <- "Provost"

  isNursing <- (df_allEE$Department %in% c("College of Nursing Billings",
                                           "College of Nursing Great Falls",
                                           "College of Nursing Missoula"))
  df_allEE[isNursing, "Org. Heirarchy"] <- "Nursing"

  isExtended <- (df_allEE$Department %in% c("Extended University NTEN",
                                            "Extended University",
                                            "Extended University Director"))
  df_allEE[isExtended, "Org. Heirarchy"] <- "Extended University"

  isAg <- (df_allEE$Department == "AES EARC")
  df_allEE[isAg, "Org. Heirarchy"] <- "Agriculture"

  isPres <- (df_allEE$Department == "Museum of the Rockies")
  df_allEE[isPres, "Org. Heirarchy"] <- "President"

  isBusiness <- (df_allEE$Department == "TS College of Business")
  df_allEE[isBusiness, "Org. Heirarchy"] <- "College of Business"


  # for (i in 1:nrow(df_allEE)) {
  #   if (df_allEE[i,"Org. Heirarchy"] %in% c("OTHER", "Other Provost")) {
  #     if (df_allEE[i,"Department"] == "AES EARC") {
  #       df_allEE[i,"Org. Heirarchy"] <- "Agriculture"
  #     } else if (df_allEE[i,"Department"] %in% c("College of Nursing Billings",
  #                                                "College of Nursing Great Falls",
  #                                                "College of Nursing Missoula")) {
  #       df_allEE[i,"Org. Heirarchy"] <- "Nursing"
  #     } else if (df_allEE[i,"Department"] %in% c("Extended University NTEN",
  #                                                "Extended University",
  #                                                "Extended University Director")) {
  #       df_allEE[i,"Org. Heirarchy"] <- "Extended University"
  #     } else if ( df_allEE[i,"Department"] == "Museum of the Rockies") {
  #       df_allEE[i,"Org. Heirarchy"] <- "President"
  #     } else if ( df_allEE[i,"Department"] == "TS College of Business") {
  #       df_allEE[i,"Org. Heirarchy"] <- "College of Business"
  #     } else if (df_allEE[i, "Org. Heirarchy"] == "Other Provost") {
  #       df_allEE[i,"Org. Heirarchy"] <- "Provost"
  #     }
  #   }
  # }

  #split apart the dataframe now that it is sorted into the right order
  df_all_split <- SplitDFIntoDFListByCol(df_allEE, "Date")

  #package both into a list
  list_out <- list(df_allEE, df_all_split)

  return(list_out)

}

AddDateVector <- function(df, dte) {
  date_vec <- rep(dte, nrow(df))
  df$Date <- as.POSIXct(date_vec)
  return(df)
}

GetAllEEColTypes <- function() {
  ### These are the column names output on the all ee report as of Nov. 23, 2016:

  # [1] "GID"                     "Last Name"               "First Name"
  # [5] "Home Street 1"           "Home Street 2"           "Home Street 3"
  # [9] "City"                    "State"                   "Zip"                     "Campus"
  # [13] "Pict Code"               "Department"              "Home Orgn Number"        "Budget Org."
  # [17] "Budget Org. Long Desc."  "Org. Heirarchy"          "Job Title"               "Status"
  # [21] "PEAEMPL ECLS"            "ECLS Description"        "MUS"                     "Position Number"
  # [25] "Suffix"                  "Position Title"          "FTE"                     "Job Type"
  # [29] "Pays"                    "Current Hire Date"       "Campus Orig. Hire"       "Longevity Date"
  # [33] "Annual Lv Accrual"       "Anniversary Date"        "Last Work Date"          "Job Begin Date"
  # [37] "Employee Group"          "Hourly Rate"             "Annual Salary"           "Assgn Salary"
  # [41] "Retirement"              "Union"                   "Union Deduction"         "BCAT"
  # [45] "Leave Category"          "Sex"                     "Race 1"                  "Birth Date"
  # [49] "SOC Code"                "SOC Description"         "Email"                   "Phone"
  # [53] "Index"                   "Fund"                    "Orgn"                    "Account"
  # [57] "Program"                 "Percent"

  # [1] "GID"                    "Last Name"              "First Name"             "Home Street 1"          "Home Street 2"
  # [6] "Home Street 3"          "City"                   "State"                  "Zip"                    "Campus"
  # [11] "Pict Code"              "Department"             "Home Orgn Number"       "Budget Org."            "Budget Org. Long Desc."
  # [16] "Org. Heirarchy"         "Job Title"              "Status"                 "PEAEMPL ECLS"           "ECLS Description"
  # [21] "MUS"                    "Position Number"        "Suffix"                 "Position Title"         "FTE"
  # [26] "Job Type"               "Pays"                   "Current Hire Date"      "Campus Orig. Hire"      "Longevity Date"
  # [31] "Annual Lv Accrual"      "Anniversary Date"       "Last Work Date"         "Job Begin Date"         "Employee Group"
  # [36] "Hourly Rate"            "Annual Salary"          "Assgn Salary"           "Retirement"             "Union"
  # [41] "Union Deduction"        "BCAT"                   "Leave Category"         "Sex"                    "Race 1"
  # [46] "Birth Date"             "SOC Code"               "SOC Description"        "Email"                  "Phone"
  # [51] "Index"                  "Fund"                   "Orgn"                   "Account"                "Program"
  # [56] "Percent"


  t <- "text"
  n <- "numeric"
  d <- "date"
  b <- "blank"
  col_types <- c(rep(t, 24), n, t, n, rep(d, 7), t, rep(n, 3), rep(t, 7), d, rep(t, 9), n)
  return(col_types)
}

AddJobEcls <- function(df) {
  require(stringr)
  #load the newest timesheet report which contains the job ecls data
  timesheet_reports <- list.files(path = "./EmployeeByTimesheetOrg/",
                                  full.names = TRUE,
                                  pattern = "Employee.*")

  timesheet_reports <- str_sort(timesheet_reports,
                                decreasing = TRUE)[1]

  employee_timesheet_rpt <- read.csv2(file = timesheet_reports,
                                      header = FALSE,
                                      skip = 6,
                                      row.names = NULL)

  employee_timesheet_rpt <- employee_timesheet_rpt[-1,]
  names(employee_timesheet_rpt) <- c("GID",
                                     "Name",
                                     "Position",
                                     "Suffix",
                                     "Ecls",
                                     "Ecls Desc.",
                                     "Salary",
                                     "Timesheet Org.",
                                     "Timesheet Org. Title",
                                     "Time Entry Method")
  employee_timesheet_rpt <- unique(employee_timesheet_rpt)
  # ensure that the GIDs are padded to allow for the data to be joined with the
  #   all ee dataset

  employee_timesheet_rpt$GID <- PadGID(employee_timesheet_rpt$GID)

  employee_timesheet_rpt$Suffix <- stri_replace_first_regex(str = employee_timesheet_rpt$Suffix,
                                                            pattern = "(^[0-9]{1})$",
                                                            replacement = "0$1")

  employee_timesheet_rpt$Key <- str_c(employee_timesheet_rpt$GID,
                                      employee_timesheet_rpt$Position,
                                      employee_timesheet_rpt$Suffix)

  employee_timesheet_rpt <- RenameColumn(employee_timesheet_rpt,
                                         old_name = "Ecls",
                                         new_name = "Jobs_Ecls")

  employee_timesheet_rpt <- select(employee_timesheet_rpt,
                                   Key, Jobs_Ecls)

  # ready to combine the data
  df <- left_join(df, employee_timesheet_rpt, by = c("Key" = "Key"))

  return(df)
}


