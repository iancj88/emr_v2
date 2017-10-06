#' Analysis of 4B Research/NTT positions on campus. Where are they located, how
#' much are they paid, what types of job/position titles do they hold?
#' 
#' As usual, this analysis uses the most recent All EE Report that has been
#' loaded into the all_ee_single_df dataframe
#' 
#' In addition, it pulls rank records (when available), from ./RankCleanup/
#' A (semi) recent file should be saved here as "Rank_cleanup_yyyymmdd.csv"

pacman::p_load(dplyr, stringr, hrutilmsu, readr, readxl, xlsx)

# start by loading the 4B ranks
ranks4B4C <- read_delim(file = "./RankCleanup/Rank_cleanup_20170803.csv",
                        delim = ";",
                        col_types = cols("c", "c", "c", "i"),
                        skip = 5)    
ranks4B4C$Key <- str_c(ranks4B4C$GID, ranks4B4C$Posn, "00")

ranks4B4C <- select(ranks4B4C, Key, Rank)

# now create the mstr fourb dataset and join the rank
fourb_mstr_all <- filter(all_ee_single_df,
                         Date == max(all_ee_single_df$Date))


test <- str_detect(fourb_mstr_all$`Position Number`, "^4B")
fourb_mstr_all <- fourb_mstr_all[test,]

fourb_mstr_all <- JoinDataToDF(fourb_mstr_all, ranks4B4C, "Key", "Key")

fourb_unique_all <- filter(fourb_mstr_all, !duplicated(Key), !EMRJobType == "Non-Job Payment")


fourb_titles <- group_by(fourb_unique_all, 
                         `Position Title`) %>%
  summarise(n = n())

fourb_org_dept <- group_by(fourb_unique_all, 
                           EMROrg, 
                           Department) %>%
  summarise(n = n())

fourb_flsa <- group_by(fourb_unique_all, 
                       FailsFLSASalaryThreshold) %>%
  summarise(n = n())

fourb_ecls <- group_by(fourb_unique_all, 
                       FinalEcls) %>%
  summarise(n = n())

fourb_titles_depts <- group_by(fourb_unique_all, 
                               `Position Title`, 
                               Department) %>%
  summarise(n = n())

fourb_flsa_fail <- filter(fourb_unique_all,
                          FailsFLSASalaryThreshold) %>%
  select(FullName, `Position Number`, `Position Title`, 
         `Job Title`, FinalEcls, EMROrg, 
         Department, FTE, `Hourly Rate`, 
         `Annual Salary`)

fourb_output_list <- list("Title Cnt" = fourb_titles, 
                          "Title Dept Cnt" = fourb_titles_depts, 
                          "Ecls Cnt" = fourb_ecls, 
                          "FLSA Test Cnt"  = fourb_flsa, 
                          "Org Dept Cnt" = fourb_org_dept,
                          "Src Data" = fourb_unique_all,
                          "Fails FLSA Salary" = fourb_flsa_fail)

WriteToFile(fourb_output_list, fname = "4B Analysis.xlsx", fpath = "./output/")

rm(fourb_mstr_all, ranks4B4C)
rm(fourb_titles, fourb_titles_depts, fourb_ecls, fourb_flsa, fourb_org_dept, fourb_unique_all, fourb_flsa_fail)

write.xlsx(fourb_output_list, 
           file = "./output_fourb/",
           creator = Sys.getenv("USERNAME"),
           startCol = 1, 
           startRow = 2, 
           colNames = TRUE, 
           keepNA = TRUE,
           firstActiveRow = 3, 
           colWidths = "auto",
           overwrite = TRUE,
           sheetName = names(fourb_output_list),
           zoom = 80,
           borders = "all",
           borderStyle = "medium",
           firstActiveRow = 3, 
           header = c("4B Position Analysis", NA, Sys.Date()),
           withFilter = TRUE)


