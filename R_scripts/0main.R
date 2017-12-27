#Load packages 
pacman::p_load(readxl, magrittr, stringr, 
               lubridate, dplyr, zoo, 
               tidyr, readr, stringi,
               xts, hrutilmsu)


#load functions from scripts
scripts_files <- c("./R_Scripts/UtilityFunctions.R",
                   "./R_scripts/EMRFunctions.R",
                   "./R_scripts/LoadAllEEFiles_2.R",
                   "./R_scripts/HeadCountFTECount.R",
                   "./R_scripts/FLSAFunctions.R")
lapply(scripts_files, source)
rm(scripts_files)

#  Set primary values
src_file_extension <- ".xlsx"
path_to_src_files <- "./SrcData"
# load and arrange source files into a list of dataframes
# the df in the max index of the list will be the most recent dataset

all_ee_data_list <- CompileAllEEReports(path_to_src_files, file_extension = src_file_extension)
rm(path_to_src_files)

all_ee_single_df <- all_ee_data_list[[1]]
all_ee_split <- all_ee_data_list[[2]]

#fix the names of all_ee_split list to drop the time portion of the pOSIXCT
if (src_file_extension == ".xlsx") names(all_ee_split) <- str_sub(names(all_ee_split), start = 1, end = 10)

# Get Headcount and FTE counts for departments and orgs
hc_fte_summary <- GetHCandFTEbyOrgs(all_ee_split[[13]], use_emr_orgs = TRUE)

# make flsa compliance check document
current_all_ee <- filter(all_ee_single_df, Date == max(all_ee_single_df$Date))

if(src_file_extension == ".txt") {
  all_ee_single_df_daily <- all_ee_single_df
  current_all_ee_daily <- current_all_ee
} else {
  all_ee_single_df_monthly <- all_ee_single_df
  current_all_ee_monthly <- current_all_ee
}

saveRDS(all_ee_single_df, file = "./output/all_data.RDS")

#pr <- prof.tree(tmp)
#print(pr, limit = NULL, prunefun = function(x) x$Percent > .05)

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
