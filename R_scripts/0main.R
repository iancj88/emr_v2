#Load packages
pacman::p_load(readxl, magrittr, stringr,
               lubridate, dplyr, zoo,
               tidyr, readr, stringi,
               xts, tidyverse)


#load functions from scripts
scripts_files <- c("./R_Scripts/UtilityFunctions.R",
                   "./R_scripts/EMRFunctions.R",
                   #"./R_scripts/LoadAllEEFiles.R",
                   "./R_scripts/HeadCountFTECount.R",
                   "./R_scripts/FLSAFunctions.R",
                   "./R_scripts/1CompileAllEEReports.R")
lapply(scripts_files, source)
rm(scripts_files)

# load and arrange source files into a list of dataframes
# the df in the max index of the list will be the most recent dataset
path_to_src_files <- "./SrcData"
all_ee_data_list <- CompileAllEEReports(path_to_src_files)
rm(path_to_src_files)

all_ee_single_df <- all_ee_data_list[[1]]
all_ee_split <- all_ee_data_list[[2]]

#fix the names of all_ee_split list to drop the time portion of the pOSIXCT
names(all_ee_split) <- str_sub(names(all_ee_split),
                               start = 1,
                               end = 10)

# Get Headcount and FTE counts for departments and orgs
most_recent_report_indx <- length(all_ee_split)
hc_fte_summary <- GetHCandFTEbyOrgs(all_ee_split[[most_recent_report_indx]],
                                    use_emr_orgs = TRUE)

# Add Salary info to all_ee dataframe(s)
all_ee_single_df <- AddLongevityYearsCol(all_ee_single_df,
                                         long_date_col_name = "Longevity Date",
                                         emr_job_type_col_name = "EMRJobType",
                                         curr_date_col_name = "Date")

# Write most recent all ee with column updates to a new file
most_recent_all_ee <- filter(all_ee_single_df,
                             Date == max(all_ee_single_df$Date))

filename <- str_c("All_EE_EMR_v2",
                  most_recent_all_ee$Date[1],
                  ".xlsx")

WriteToFile(most_recent_all_ee,
            fname = filename,
            fpath = "./output/")
rm(filename)

########### WORK TO BE DONE >>>>

# WriteToFile(hc_fte_summary, fname = "HC_FTE_by_dept_org2.xlsx",
#             fpath = "./output/", addAsNewSht = TRUE)
#
#
# # each unique gid will be in a row with the time-series of statuses in columns
# max_col_index <- length(all_ee_split) + 1
# max_row_index <- n_distinct(all_ee_single_df$GID)
#
# # Rename the split list to remove the time stamps
# names(all_ee_split) <- sapply(names(all_ee_split), FUN = str_extract,
#                               pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
#                              simplify = TRUE, USE.NAMES = FALSE)
#
# #initialize the EE Status dataframe
# ee_status_col_classes <- c(rep("text", max_col_index + 1))
# ee_status_col_names <- str_c("GID", names(all_ee_split))
#
# #filter_out nonapplicable jobs via suffix
