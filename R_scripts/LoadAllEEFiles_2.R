


CompileAllEEReports <- function(path_to_files) {
  require(lubridate)
  Rprof(tmp <- tempfile())
  #get the list of wb file names and filepaths
  wb_names_and_filepaths <- list.files(path_to_files, full.names = TRUE)
  wb_names_only <- list.files(path_to_files, full.names = FALSE)
  
  #read each wb into a separate dataframe
  wb_col_types <- GetAllEEColTypes()
  all_ee_datasets <- ReadfromExcelFiles(wb_names_and_filepaths, wb_col_types)
  
  ReadfromExcelFiles <- function(name_and_filepaths, col_types) {
    data_sets <- lapply(name_and_filepaths, FUN = read_excel, sheet = 1,
                                col_types = col_types, col_names = T)
    return(data_sets)
  }
  
  #Name the dataset list using the date in the file name, 
  # this works because the files are read in the same order
  # above as the filenames are read here.
  
  report_date_chr <- gsub("All EE ", "", wb_names_only) %>% #Remove the preceeding All EE prefix from the filename
    {gsub(".xlsx", "", . )} #Remove the excel file type suffix from the filename
  names(all_ee_datasets) <- report_date_chr
  
  #put the date (currently stored in the names of the dataframes) into a column of each dataframe

  all_ee_datasets <- add_date_col(all_ee_datasets, dates = names(all_ee_datasets))
  add_date_col <- function(dfs, dates) {
    dfs_out <- mapply(FUN = Add_Dates, dfs, dates, SIMPLIFY = F)
  }
  
  Add_Dates <- function(df, dte) {
    date_vec <- c(rep(dte, nrow(x)))
    x$Date <- as.POSIXct(date_vec)
    return(x)
  }
  
  #combine dataframes
  df_allEE <- bind_rows(all_ee_datasets)
  
  #pad the gids to ensure that they are properly treated as char scalars
  #df_allEE$GID <- sapply(df_allEE$GID, PadGID)
  
  #add a full name
  df_allEE$FullName <- str_c(df_allEE$`Last Name`,", ", df_allEE$`First Name`)
  
  #pad the suffix if necessary
  df_allEE$Suffix <- stri_replace_first_regex(df_allEE$Suffix, "([0-9])$", "0$1")
  
  #add a unique key for each row from the GID + Position_Number + Suffix
  df_allEE$Key <- str_c(df_allEE$GID, df_allEE$`Position Number`, df_allEE$Suffix)
  
  # pad the fund if necessary (dropped leading zero)
  short_funds <- which(str_length(df_allEE$Fund) == 5)
  df_allEE[short_funds,"Fund"] <- str_c("0",df_allEE$Fund[short_funds])
  
  # determine if the line is state or non-state funded
  df_allEE$IsStateFund <- str_detect(df_allEE$Fund, "^41|^01|^91")
  
  #add a unique key for each date
  df_allEE$KeyDate <- str_c(df_allEE$Key, df_allEE$Date)
  
  # add the fiscal year
  df_allEE$quarter_date <- quarter(df_allEE$Date)
  df_allEE$year <- year(df_allEE$Date)
  df_allEE$fy <- (df_allEE$quarter_date > 2) + df_allEE$year
  
  #order the master dataframe by date
  df_allEE <- arrange(df_allEE, Date)
  
  #Add the Job Eclass data column (as opposed to PEAEMPL eclass)
  df_allEE <- AddJobEcls(df_allEE)
  
  #Add the EMR Specific columns
  df_allEE <- AddEMRColumns(df_allEE)
  
  # Add the FLSA Specific columns -- 
  # This must go after adding the EMR Columns!!!
  df_allEE <- AddFLSAColumns(df_allEE)
  
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
  
  # Add a longevity raise amount column depending on the years of service
  
  longevity_date <- df_allEE$`Longevity Date`
  day(longevity_date) <- 01 # calculate the years of service from the 1st. 
                            # that's when the longevity increase goes into effect in payroll
  
  years_of_service <- interval(longevity_date, df_allEE$Date) / duration(num = 1, units = "years")
  years_of_service <- floor(years_of_service) # use the whole integer for the lookup value in the longevity multiplier percent
  
  df_allEE$`years_of_service` <- years_of_service
  longevity_mutiplier_lu <- read_xlsx(path = "C:/VBA_Src/Tables/TableLongevity.xlsx")
  JoinDataToDF(df_allEE, df_lu = longevity_mutiplier_lu, key_main = "years_of_service", key_lu = "YearsOfService")
  # there should now be a column in the dataframe titled "PercentToBase"
  df_allEE$PercentToBase <- df_allEE$PercentToBase + 1
  df_allEE[,!df_allEE$EMRJobType == "Classified"]$PercentToBase <- 1
  df_allEE$BaseAndLongHourly <- df_allEE$PercentToBase * df_allEE$`Hourly Rate`
  df_allEE$BaseAndLongAssgn <- df_allEE$PercentToBase * df_allEE$`Assgn Salary`
  df_allEE$BaseAndLongAnnual <- df_allEE$PercentToBase * df_allEE$`Annual Salary`
  
  rm(longevity_date)
  
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
  Rprof(NULL)
  saveRDS(tmp, file = "rprofiler.rds")
  return(list_out)
  
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
  timesheet_reports <- list.files(path = "C:/VBA_Source/Datasets/EmployeeByTimesheetOrg/", full.names = TRUE)
  timesheet_reports <- str_sort(timesheet_reports, decreasing = TRUE)[1]
  employee_timesheet_rpt <- read.csv2(file = timesheet_reports, header = TRUE, skip = 6, row.names = NULL)
  
  # ensure that the GIDs are padded to allow for the data to be joined with the 
  #   all ee dataset
  
  employee_timesheet_rpt$GID <- mapply(FUN = hrutilmsu::PadGID, employee_timesheet_rpt$GID)
  employee_timesheet_rpt$Key <- str_c(employee_timesheet_rpt$GID, employee_timesheet_rpt$Position, employee_timesheet_rpt$Suffix)
  employee_timesheet_rpt <- RenameColumn(employee_timesheet_rpt, old_name = "Employee.Class", new_name = "Jobs_Ecls")
  employee_timesheet_rpt <- select(employee_timesheet_rpt, Key, Jobs_Ecls)
  
  # ready to combine the data
  df <- left_join(df, employee_timesheet_rpt, by = c("Key" = "Key"))
  
  return(df)
}
  

