  

  # figure out who ended their contracts by comparing the previous month's MUS contract employes with the current months
  # must have hte all_ee_split df updated to the current datasets
  pacman::p_load(lubridate, dplyr)
  curr_month_indx <- length(all_ee_split)
  
  # go back through the datasets until you get a prior months report
  # this protects you from accidentally grabbing two reports from the same month for the comparison
  last_month_indx <- curr_month_indx - 1
  curr_month <- month(all_ee_split[[curr_month_indx]]$Date[1])
  last_month <- month(all_ee_split[[last_month_indx]]$Date[1])
  
  while(curr_month == last_month) {
    last_month_indx <- last_month_indx - 1
    last_month <- month(all_ee_split[[last_month_indx]]$Date[1])
  }
  rm(curr_month, last_month)
  
  # testing
  # last_month_indx <- 15
  curr_df <- all_ee_split[[curr_month_indx]]
  last_df <- all_ee_split[[last_month_indx]]
  rm(curr_month_indx, last_month_indx)
  
  # now to make the comparisons
  curr_mus <- filter(curr_df, MUS == "Y")
  last_mus <- filter(last_df, MUS == "Y")
  
  curr_mus_gids <- select(curr_mus, GID)
  last_mus_gids <- select(last_mus, GID)
  
  missing_gids <- last_mus_gids[!last_mus_gids$GID %in% curr_mus_gids$GID,]
  
  termed_mus_df <- filter(last_mus, GID %in% missing_gids$GID)
  rm(curr_df, curr_mus, curr_mus_gids)
  rm(last_df, last_mus, last_mus_gids)


