Get_Year_Data <- function(year, output_file) {
  all_ee_file_path <- "C:/Users/icj_work/Documents/EMR_v2/R_scripts/output/all_data.RDS"
  if(!exists(all_ee_file_path)) {
    source("C:/Users/icj_work/Documents/EMR_v2/R_scripts/0main.R")
  }
  all_ee_single_df <- readRDS(all_ee_file_path)
  year_data <- filter(all_ee_single_df, year(all_ee_single_df$Date) == year)
  
  saveRDS(year_data, file = output_file)
}