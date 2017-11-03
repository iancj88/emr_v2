

GetHCandFTEbyOrgs <- function(all_ee_df_or_list, use_emr_orgs = FALSE) {
  # TODO:
  #   Standardize df column names,
  #   Create excel template to handle description page
  #   Properly order output list for excel

  require(dplyr)
  #filter out non-applicable rows
  if (!class(all_ee_df_or_list) == "list") {
    tempdf <- all_ee_df_or_list
    all_ee_df_or_list <- NULL
    all_ee_df_or_list[[1]] <- tempdf
    rm(tempdf)
  }

  if (use_emr_orgs == TRUE) {
    org_col_name <- "Org_EMR"
  } else {
    org_col_name <- "Org. Heirarchy"
  }

  for (i in length(all_ee_df_or_list)) {
    all_ee <- all_ee_df_or_list[[1]]

    all_ee_filtered <- filter(all_ee, Suffix %in% c("00", "01", "03", "02", "04", "05")) %>%
      filter(!duplicated(Key_Date)) %>%
      filter(!Job_Type_EMR %in% c("Non-Job Payment")) %>%
      filter(!is.na(Job_Type_EMR)) %>%
      filter(!`Position_Number` == "4ONEPY") %>%
      filter(Status == "A") %>%
      filter(!is.na(Org_EMR))


    hc_fte_cnt_total <- summarise(all_ee_filtered, "Head Count" = n_distinct(GID), "Total FTE" = sum(FTE))
    hc_fte_cnt_total_etype <- group_by(all_ee_filtered, Job_Type_EMR) %>%
      summarise(`EE Type HC` = n_distinct(GID), `EE Type FTE` = sum(FTE)) %>%
      ungroup()

    hc_fte_cnt_orgn <- group_by_(all_ee_filtered, org_col_name) %>%
      summarise(`Orgn. HC` = n_distinct(GID), `Orgn. FTE` = sum(FTE)) %>%
      arrange_(org_col_name) %>%
      ungroup()

    hc_fte_cnt_dept <- group_by(all_ee_filtered, Job_Budget_Org_Long_Desc) %>%
      summarise(`Dept. HC` = n_distinct(GID), `Dept. FTE` = sum(FTE)) %>%
      arrange(Job_Budget_Org_Long_Desc) %>%
      ungroup()

    hc_fte_cnt_etype <- group_by_(all_ee_filtered, org_col_name, "Job_Budget_Org_Long_Desc", "Job_Type_EMR") %>%
      summarise(`EE Type HC` = n_distinct(GID), `EE Type FTE` = sum(FTE)) %>%
      arrange_(org_col_name, "Job_Budget_Org_Long_Desc", "Job_Type_EMR") %>%
      ungroup()

    #now join the etype, dept, and orgn data into a single table

    hc_fte_cnt_all <- JoinDataToDF(hc_fte_cnt_etype, hc_fte_cnt_dept, "Job_Budget_Org_Long_Desc", "Job_Budget_Org_Long_Desc") %>%
      JoinDataToDF(hc_fte_cnt_orgn, org_col_name, org_col_name)

    hc_fte_cnt_all <- RenameColumn(hc_fte_cnt_all, "Job_Type_EMR", "EE Type")
    hc_fte_cnt_org_etype <- group_by_(all_ee_filtered, org_col_name, "Job_Type_EMR") %>%
      summarise(etype.headcount = n_distinct(GID), etype.fte = sum(FTE)) %>%
      arrange_(org_col_name, "Job_Type_EMR")

    if (use_emr_orgs == TRUE) {
        RenameColumn(hc_fte_cnt_all, "Org_EMR", "EMR Organization")
      }

    hc_fte_cnt_desc <- paste("The following counts are derived from the all employees report dated ",
                             all_ee_df_or_list$Date,
                             "\r\n\r\n",
                             sep = "")
    if (i == 1) {
      output <- list(hc_fte_cnt_desc,
                     hc_fte_cnt_all,
                     hc_fte_cnt_org_etype,
                     hc_fte_cnt_total,
                     hc_fte_cnt_total_etype,
                     hc_fte_cnt_dept,
                     hc_fte_cnt_orgn)
      names(output) <- c("Description",
                         "Org - Dept - EType",
                         "Org - Etype",
                         "University Total",
                         "University - Etype",
                         "Dept Totals",
                         "Org Totals")
      } else {
      output <- list(output, hc_fte_cnt_all)
    }

    if ( is.null(names(all_ee_df_or_list)[[1]])) {
      name <- "HC & FTE"
      name_list <- list("HC & FTE")
    } else {
      name_list <- list(name_list, str_c("HC & FTE - ", names(all_ee_df_or_list)[[1]]))
    }

    ### names(output) <- name_list

  }
  return(output)
}
