# This assumes that the main.R EMR script has been run to create the all_ee_single_df

professionals_who_teach <- filter(all_ee_single_df, Account %in% c(61123,"61123N"), 
                                  substr(`Position Number`, 1,2) == "4C",
                                  Date > "2016-06-22") # Critical Date Value to specify 
                                                       # the year for which data will be collected

professionals_who_teach <- professionals_who_teach[!duplicated(professionals_who_teach$Key),]

professionals_who_teach <- select(professionals_who_teach, 
                                  GID, FullName, `Position Number`, Account, Orgn)

path_to_xwalk <- "C:/VBA_Source/Tables/TableDept.xlsx"
emr_org_xwalk <- read_excel(path = path_to_xwalk, sheet = 1, 
                            col_names = TRUE, 
                            skip = 1)
emr_org_xwalk <- select(emr_org_xwalk, `Dept Number`, `Sub Dept`)
professionals_who_teach <- JoinDataToDF(professionals_who_teach, 
                                        emr_org_xwalk, 
                                        "Orgn", 
                                        "Dept Number")

professionals_who_teach[24,"Sub Dept"] <- "Leadership Institute"

WriteToFile(df = professionals_who_teach,
            fname = "TeachingProfessionalsFY17.xlsx",
            fpath = "./output/")
