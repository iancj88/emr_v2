pacman::p_load(lubridate, 
               readxl, 
               dplyr, 
               magrittr,
               hrutilmsu,
               stringr)

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
flex_log_data$FullName <- str_c(flex_log_data$`EE Last Name`, 
                                ", ", 
                                flex_log_data$`EE First Name`)

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
rm(fy, effective_dates)

# Add a State vs Non-State Funding column
# State funds are those which begin with 41%, 01%, and 91%
is_state_funded <- str_detect(flex_log_data$`Funding Source Index`, "^41|01|91") &
  (!(str_length(flex_log_data$`Funding Source Index`) > 6))

flex_log_data$StateFunding <- is_state_funded

# add the EMR org from the department as specified in the log lookup table
flex_dept_emr_lu <- select(flex_dept_emr_lu, Department, Organization)
flex_log_data <- JoinDataToDF(flex_log_data, 
                              df_lu = flex_dept_emr_lu, 
                              key_main = "Department", 
                              key_lu = "Department")
rm(flex_dept_emr_lu)

# remove the vacant or NA name rows
flex_log_data <- filter(flex_log_data, !`EE First Name` == "NA", !`EE Last Name` == "NA") %>%
  filter(!str_detect(`EE First Name`, "[V,v]acan[cy,t]")) %>%
  filter(!str_detect(`EE Last Name`, "[V,v]acan[cy,t]")) %>%
  filter(!str_detect(`EE First Name`, "Moore")) %>%
  filter(!str_detect(`EE Last Name`, "Moore"))

# make separate dataframes for base-increasing (base) and one-time-only(oto)
base_increase <- (!flex_log_data$`Type of FLEX pay` == "Lump-Sum Bonus")
flex_log_data$`Base-Increasing` <- base_increase

oto_flex_pays <- flex_log_data[!base_increase,]
base_flex_pays <- flex_log_data[base_increase,]

rm(base_increase)

# Get the labor distribution from the all employee report
# currently set to analyze for fy17
name_posn_funding <- filter(base_flex_pays,
                            fy == 2017, 
                            !is.na(`Current Wage`),
                            !is.na(`Proposed Wage`)) %>%
  select(`Position Number`, FullName)

name_posn_funding$Key <- str_c(name_posn_funding$FullName, 
                               name_posn_funding$`Position Number`)
name_posn_funding <- select(name_posn_funding, Key)

all_ee_fy17 <- filter(all_ee_single_df, fy == 2017)
all_ee_fy17$NamePosnKey <- str_c(all_ee_fy17$`Last Name`, 
                                 ", ", 
                                 all_ee_fy17$`First Name`, 
                                 all_ee_fy17$`Position Number`)

# only examine the all employee report rows that correspond to a person 
#   receiving a flex-pay.
funding_analysis <- filter(all_ee_fy17, NamePosnKey %in% name_posn_funding$Key, Suffix == "00")

# get the data corresponding to the most recent all employee report in-
#   which the person/job appear. Use it to filter the all employee master
#   file to only the relevant rows.
max_date_per_key <- group_by(funding_analysis, NamePosnKey) %>%
  summarise(max_date = max(Date))

funding_analysis <- JoinDataToDF(funding_analysis, 
                                  df_lu    = max_date_per_key, 
                                  key_main = "NamePosnKey", 
                                  key_lu   = "NamePosnKey") %>%
  filter(max_date == Date)

# this is the subset of flex pay data which will be analyzed
fy17_fp <- filter(base_flex_pays, fy == 2017, 
                  !is.na(`EE First Name`),
                  !is.na(`EE Last Name`),
                  !is.na(`Current Wage`),
                  !is.na(`Proposed Wage`))

wage_change_data <- fy17_fp
                           
wage_change_data$Key <- str_c(wage_change_data$FullName, 
                              wage_change_data$`Position Number`)

wage_change_data$sal_diff_year_amnt <- (wage_change_data$`Proposed Wage` - 
                                          wage_change_data$`Current Wage`) * 2080

wage_change_data <- select(wage_change_data, 
                           Key,
                           sal_diff_year_amnt) #%>%
  #group_by(Key) %>%
  #summarize(sal_diff_year_amnt = sum(sal_diff_year_amnt))

# get the all ee rows and add the sal difference from the flex pay
funding_analysis_limited <- select(funding_analysis, NamePosnKey, IsStateFund, Percent, EMROrg) %>%
  JoinDataToDF(df_lu = wage_change_data, key_main = "NamePosnKey", key_lu = "Key")

# determine the cost of the yearly sal change to each fund from labor distribution
funding_analysis_limited$CostToFund <- ((funding_analysis_limited$Percent / 100)
                                        * funding_analysis_limited$sal_diff_year_amnt)

testing123 <- group_by(funding_analysis_limited, NamePosnKey) %>% summarize(total_percent = sum(Percent)) %>% filter(total_percent < 100)
testing123 <- JoinDataToDF(testing123, funding_analysis_limited, "NamePosnKey", "NamePosnKey")

# lastly, sum the costs to state vs non-state funds
funding_summary <- group_by(funding_analysis_limited, IsStateFund) %>%
  summarise(total_yearly_cost = sum(CostToFund, na.rm = TRUE))

