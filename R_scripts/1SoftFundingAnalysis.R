# This analysis will pull all positions held by an employee with at least one 
# position held by an employee with a soft funded job. It will then create some
# summary stats regarding quantity etc.
#
# This assumes that the latest all_ee report has been loaded into the dataframe
#


pacman::p_load(dplyr, stringr)

# make three lists of, one that is all state, one that is all soft, and one that is both
all_ee_single_df <- group_by(all_ee_single_df, EMROrg)

all_ee_recent_class <- filter(all_ee_single_df, #frame to be used as the initial datasource
                              Date == max(all_ee_single_df$Date),
                              EMRJobType == "Classified")

all_other_soft_funded <- filter(all_ee_recent_class,
                          !IsStateFund,
                          Percent == 100,
                          !str_detect(Fund, "^4W|^42"))
all_other_soft_summary <- summarise(all_other_soft_funded, 
                                    "100% Soft Non-Grant Funded Salary" = sum(`Annual Salary`),
                                    "Head Cnt - Soft Non-Grant" = n_distinct(GID),
                                    "Position Cnt - Soft Non-Grant" = n_distinct(Key))

all_grant_soft_funded <- filter(all_ee_recent_class,
                                !IsStateFund,
                                Percent == 100,
                                str_detect(Fund, "^4W|^42"))
all_grant_soft_summary <-  summarise(all_grant_soft_funded,
                                     "100% Soft Grant Funded Salary" = sum(`Annual Salary`),
                                     "Head Cnt - Grant" = n_distinct(GID),
                                     "Position Cnt - Grant" = n_distinct(Key))

all_state_funded <- filter(all_ee_recent_class,
                           IsStateFund,
                           Percent == 100)
all_state_summary <-  summarise(all_state_funded,
                                "100% State Funded Salary" = sum(`Annual Salary`),
                                "Head Cnt - State" = n_distinct(GID),
                                "Position Cnt - State" = n_distinct(Key))

# create a vector of gids that will encompass all jobs that fall 100% into one of
# the previous categories
 
single_fund_src_jobs <- c(all_state_funded$Key, all_grant_soft_funded$Key, all_other_soft_funded$Key)

funds_from_multiple <- filter(all_ee_recent_class,
                          !Key %in% single_fund_src_jobs)
funds_from_multi_summary <- summarise(funds_from_multiple, 
                                      "Head Cnt - Split" = n_distinct(GID),
                                      "Position Cnt - Split" = n_distinct(Key))
                            #all_grant_soft_funded$Key & !Key %in% all_state_funded$Key & !Key %in% all_other_soft_funded$Key)
categorized_jobs <- c(single_fund_src_jobs, funds_from_multiple$Key)

state_from_multi <- filter(funds_from_multiple,
                           IsStateFund) %>%
  summarise("Split - State" = sum(`Annual Salary`* (100/Percent)))

softNG_from_multi <- filter(funds_from_multiple,
                            str_detect(Fund, "^4W|^42")) %>%
  summarise("Split - Soft Non-Grant" = sum(`Annual Salary`* (100/Percent)))

grant_from_multi <- filter(funds_from_multiple,
                           !IsStateFund,
                           !str_detect(Fund, "^4W|^42")) %>%
  summarise("Split - Grant" = sum(`Annual Salary`* (100/Percent)))

# must verify that all jobs are being categorized. if this dataframe has any
# rows, there are positions being dropped from the analysis
orphans <- filter(all_ee_recent_class,
                   !Key %in% categorized_jobs)
                    #all_grant_soft_funded$Key & !Key %in% all_state_funded$Key & !Key %in% all_other_soft_funded$Key, !Key %in% funds_from_multiple$Key)
output_soft <- data_frame("EMROrg" = unique(all_ee_recent_class$EMROrg))
output_soft <- JoinDataToDF(output_soft, all_state_summary, "EMROrg", "EMROrg") %>%
  JoinDataToDF(all_grant_soft_summary, "EMROrg", "EMROrg") %>%
  JoinDataToDF(all_other_soft_summary, "EMROrg", "EMROrg") %>%
  JoinDataToDF(funds_from_multi_summary, "EMROrg", "EMROrg") %>%
  JoinDataToDF(state_from_multi, "EMROrg", "EMROrg") %>%
  JoinDataToDF(softNG_from_multi, "EMROrg", "EMROrg") %>%
  JoinDataToDF(grant_from_multi, "EMROrg", "EMROrg")

output_soft[is.na(output_soft)] <- 0
View(output_soft)

WriteToFile(output_soft, fname = "fund_analysis_fingers_crossed.xlsx", fpath = "./output/")

rm(all_grant_soft_funded, 
   all_grant_soft_summary,
   grant_from_multi,
   softNG_from_multi,
   state_from_multi,
   funds_from_multi_summary,
   all_state_summary,
   orphans,
   categorized_jobs
   )


# # need to group the classified by 3 sub categories: 100% grant, 100% other soft, and
# soft_classifieds_by_position %>%
#   summarise(soft_classifieds,
#             "Position Cnt" = n(),
#             "Head Cnt" = n_distinct(GID),)
