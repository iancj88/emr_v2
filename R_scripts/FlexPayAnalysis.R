library(readxl)
library(tidyr)
library(magrittr)
library(zoo)
library(lubridate)
library(dplyr)

# analyze flex pays dataframe
AnnalyzeFlexDataset <- function(df, fiscalyear) {
  df <- filter(df, fy == fiscalyear)
  
  base_increase <- (!df$`Type of FLEX pay` == "Lump-Sum Bonus")
  df$`Base-Increasing` <- base_increase
  
  oto_flex_pays <- df[!base_increase,]
  base_flex_pays <- df[base_increase,]
  
  #compute the before and after salary difference
  base_sal_change <- filter(base_flex_pays, 
                            !is.na(`Current Wage`), 
                            !is.na(`Proposed Wage`), 
                            !`EE First Name` == "NA",
                            !`EE Last Name` == "NA")
  
  base_sal_change$sal_diff <- base_sal_change$`Proposed Wage` - base_sal_change$`Current Wage`
  oto_flex_pays$sal_diff <- 0
  
  output_df <- bind_rows(oto_flex_pays, base_sal_change)
  rm(base_flex_pays, base_sal_change, oto_flex_pays)
  
  # Combine the HC data for the EMR Organizations
  # requires the output from the HeadCountFTECount.R script
  hc_emr_data <- hc_fte_summary[[2]] 
  hc_emr_data[which(hc_emr_data$EMROrg == "Information Technology Center"), "EMROrg"] <- "University Information Technology"
  hc_emr_data <- filter(hc_emr_data, EMRJobType == "Classified") %>%
    select(-EMRJobType)
  
  output_df <- JoinDataToDF(output_df, hc_emr_data, key_main = "Organization", key_lu = "EMROrg")
  
  output_df_fund_src <- GetStateFundData(output_df, fiscalyear)
  
  #create summary dataframes
  output_df <- group_by(output_df, Organization, fy) %>%
    summarize(                n = n(),
                              n_base_increasing = sum(`Base-Increasing` == TRUE),
                              n_one_time_only = sum(`Base-Increasing` == FALSE),
                              lump_sum_total = sum(`Lump Sum Amount`, na.rm = TRUE),
                              annual_sal_change = 2080 * sum(sal_diff),
                              change_per_base_flex = annual_sal_change / n) %>%
    ungroup()
  
  # temp <- group_by(output_df, `Type of FLEX pay`, fy) 
  #   summarize(temp, n = n(),
  #             lump_sum_total = sum(`Lump Sum Amount`, na.rm = TRUE),
  #             annual_sal_change = 2080 * sum(sal_diff),
  #             change_per_base_flex = annual_sal_change / n)
  
  output_df <- JoinDataToDF(output_df, hc_emr_data, key_main = "Organization", key_lu = "EMROrg")
  output_df$flex_per_ee <- output_df$n / output_df$etype.headcount
  
  output_df <- select(output_df, -etype.fte) %>%
    RenameColumn("fy", "FY") %>%
    RenameColumn("annual_sal_change", "Annual Salary Increase") %>%
    RenameColumn("change_per_base_flex", "Average Increase per Base Flex") %>%
    RenameColumn("flex_per_ee", "# Flex Pays / Org Headcount") %>%
    RenameColumn("n", "Number of Flex Pays")
  
  return(output_df)
}



# load the flex pay log form into a dataframes
path_to_log <- "./FlexPayLog/FlexPay Log.xlsm"
flex_log_shts <- excel_sheets(path_to_log)

log_sheet_number <- which(flex_log_shts == "FlexPay Log")
flex_log_data <- read_xlsx(path = path_to_log, sheet = log_sheet_number, skip = 2)

lu_sheet_number <- which(flex_log_shts == "Constants")
flex_dept_emr_lu <- read_xlsx(path = path_to_log, sheet = lu_sheet_number, skip = 2)
rm(path_to_log, flex_log_shts, log_sheet_number, lu_sheet_number)

# add a full name column
flex_log_data$`EE First Name` <- str_replace_na(flex_log_data$`EE First Name`)
flex_log_data$`EE Last Name` <- str_replace_na(flex_log_data$`EE Last Name`)
flex_log_data$FullName <- str_c(flex_log_data$`EE Last Name`, ", ", flex_log_data$`EE First Name`)

# add fiscal year to a column
# first look at effective date, if no effective date then look at received date
flex_log_data$date_all <- flex_log_data$`Date Received`
flex_log_data$`Effective Date` <- as.POSIXct(flex_log_data$`Effective Date`, 
                                             format = "%m/%d/%Y")
effective_dates <- !is.na(flex_log_data$`Effective Date`)
flex_log_data[effective_dates,"date_all"] <-  flex_log_data[effective_dates,"Effective Date"]



flex_log_data$quarter_date <- quarter(flex_log_data$date_all)
flex_log_data$year <- year(flex_log_data$date_all)
fy <- (flex_log_data$quarter_date > 2) + flex_log_data$year
flex_log_data$fy <- fy
rm(fy)

# remove progression plans with no effective date 
#   (indicating that they were never pursued, or have not yet been applied)
flex_log_data <- filter(flex_log_data, 
                        !(is.na("Effective Date") & 
                            "Type of FLEX pay" == "Progression Plan"))

# Add a State vs Non-State Funding column
# State funds are those which begin with 41%, 01%, and 91%
fy17_fp <- filter(flex_log_data, fy == 2017, 
                  !is.na(`EE First Name`),
                  !is.na(`EE Last Name`))

is_state_funded <- str_detect(flex_log_data$`Funding Source Index`, "^41|01|91") &
  (str_length(str_length(flex_log_data$`Funding Source Index`) > 6))
   
flex_log_data$StateFunding <- is_state_funded


# add the fund source info


# add the EMR org from the department. 
flex_dept_emr_lu <- select(flex_dept_emr_lu, Department, Organization)
flex_log_data <- JoinDataToDF(flex_log_data, 
                              df_lu = flex_dept_emr_lu, 
                              key_main = "Department", 
                              key_lu = "Department")
rm(flex_dept_emr_lu)

# make separate dataframes for base-increasing (base) and one-time-only(oto)
base_increase <- (!flex_log_data$`Type of FLEX pay` == "Lump-Sum Bonus")
flex_log_data$`Base-Increasing` <- base_increase

oto_flex_pays <- flex_log_data[!base_increase,] 
base_flex_pays <- flex_log_data[base_increase,]

rm(base_increase)




# make counts by fiscal year as long tables (year columns)
# use summary dataframes for graphics, table for text display

# get rid of blank and vacant names
flex_log_data <- filter(flex_log_data, 
                        !`EE First Name` == "NA", 
                        !`EE Last Name` == "NA",
                        !str_detect(`EE First Name`, "[V,v]acan[cy,t]"),
                        !str_detect(`EE Last Name`, "[V,v]acan[cy,t]"))

summary_fp_year_type <- group_by(flex_log_data, fy, `Type of FLEX pay`) %>%
  summarize(n = n())
summary_fp_year_type_sum <- group_by(flex_log_data, fy, `Type of FLEX pay`) %>%
  summarize(n = n())
summary_fp_year_baseinc <- group_by(flex_log_data, fy, `Base-Increasing`) %>%
  summarize(n = n())
table_fp_year_type <- spread(summary_fp_year_type, key = fy, value = n)
table_fp_year_baseinc <- spread(summary_fp_year_baseinc, key = fy, value = n)

summary_fp_year_type <- group_by(flex_log_data, fy, `Type of FLEX pay`) %>%
  summarize(n = n())

summary_fp_year <- group_by(flex_log_data, fy) %>%
  summarize(n = n())

# Analyze the number of repeat people getting flex pays

number_flex_pays <- nrow(flex_log_data)

# remove the same duplicate rows (multiple flex pays in same month)
repeat_analysis <- flex_log_data
repeat_analysis$Key <- str_c(repeat_analysis$FullName, repeat_analysis$date_all)
repeat_analysis <- filter(repeat_analysis, !duplicated(Key))
number_unique_people <- n_distinct(repeat_analysis$FullName)

repeat_population <- group_by(repeat_analysis, FullName) %>%
  filter(n()>1) %>%
  arrange(FullName)

total_population_hist <- group_by(repeat_analysis, FullName) %>%
  summarize(n = n())

repeat_pop_hist <- summarize(repeat_population, n = n())

primary_output <- AnnalyzeFlexDataset(flex_log_data, 
                                      fiscalyear = 2017)

state_funding_analysis <- group_by(flex_log_data, 
                                   `Base-Increasing`, 
                                   StateFunding) %>% 
  filter(fy == 2017, 
         `Base-Increasing` == TRUE) %>% 
  summarise(n = n()) 

list_to_write <- list(primary_output, 
                      repeat_pop_hist, 
                      total_population_hist, 
                      summary_fp_year, 
                      summary_fp_year_baseinc, 
                      summary_fp_year_type)

WriteToFile(list_to_write, fname = "Fy17 fp analysis2.xlsx", fpath = "./output")
