# This script depends on the daily All EE Reports being compiled into the all_ee_single df and 
# 
# Use this to test if the MUS Contract indicator was ever turned on in a given time period (date range)
# It will check to see if those individuals had it on at any time and if they currently have it set.
require(dplyr)

# Get a master list of all keys htat have ever had the checkbox checked and are actual job payments 

monthly_mus_true_rows <- filter(all_ee_single_df_monthly, MUS == "Y", !Suffix == "SD")
daily_mus_true_rows <- filter(all_ee_single_df_daily, MUS == "Y", !Suffix == "SD")
all_mus_true_rows <- bind_rows(monthly_mus_true_rows, daily_mus_true_rows)

unique_mus_keys <- unique(all_mus_true_rows$Key)

#now see who is currently not an mus indicated job in the most recent all ee
non_mus_current <- filter(current_all_ee_daily, MUS == "N")
non_mus_keys <- unique(non_mus_current$Key)

# now make hte comparison
no_longer_MUS_keys <- non_mus_keys[which(non_mus_keys %in% unique_mus_keys)]

# compile a history for these people, one row is one date per person
monthly_and_daily <- bind_rows(all_ee_single_df_daily, all_ee_single_df_monthly)
MUS_issue_rows <- filter(monthly_and_daily, monthly_and_daily$Key %in% no_longer_MUS_keys)

debug_MUS_issues <- select(MUS_issue_rows, GID, FullName, Department, `Job Title`, MUS, Date)

