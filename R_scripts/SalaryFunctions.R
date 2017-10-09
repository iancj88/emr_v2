


# Add Longevity Years column
#
#
AddLongevityYearsCol <- function(df,
                                 long_date_col_name    = "Longevity Date",
                                 emr_job_type_col_name = "EMRJobType",
                                 curr_date_col_name    = NULL)              {
  require(lubridate)

  # If a column of current dates is not given, compute the longevity
  # as of the current date according to Sys.Date
  if (is.null(curr_date_col_name)) {
    warning("a column of 'as-of' dates has not been supplied, i.e. curr_date_col_name is null \r\n",
            "defaulting to use an as-of date of the current Sys.Date")
    curr_dates <- rep(Sys.Date(), nrow(df))
  } else {
    curr_dates <- df[[curr_date_col_name]]
  }

  # grab the dates vectors to compare to the 'as-of' date
  # pull out the year and month components from each
  #   because longevity bonus kicks in at the beginning of the
  #   pay period during which the years-of-service threshold is crossed,
  #   compare make the distinction based on the month only. no day values
  #   are needed.
  long_dates <- df[[long_date_col_name]]

  long_years <- year(long_dates)
  curr_years <- year(curr_dates)

  long_months <- month(long_dates)
  curr_months <- month(curr_dates)
  pre_partial_year <- curr_months < long_months

  years_of_serv <- curr_years - long_years
  df$Longevity_Years <- years_of_serv - pre_partial_year

  # lastly, ensure that the longevity years is zero for all non-classified jobs
  non_classified_rows <- which(!df$EMRJobType == "Classified")
  df$Longevity_Years[non_classified_rows] <- 0
  return(df)
}

# Add Longevity % column
#
#
AddLongevityPercentCol <- function(df) {
  if (is.null(df$Longevity_Years)) {
    warning("AddLongevityPercentCol is being called before AddLongevityYears.\r\n",
            "Defaulting to compute from current Sys.Date()")
    df <- AddLongevityYearsCol(df)
  }

  longevity_lookup <- read_excel(path = "./LookupData/TableLongevity.xlsx",
                                 sheet = 1)
  df <- left_join(df, longevity_lookup, by = c("Longevity_Years" = "Years_Of_Service"))

  return(df)
}

# Add Actual Salary after longevity
#
#
AddLongevityPayCol <- function(df,
                                    pay_rate_col = "Annual Salary",
                                    new_col_name = NULL) {

  if (is.null(new_col_name)) {
    new_col_name <- paste(pay_rate_col,
                          "_with_Longevity",
                          sep = "")
  }

  amnt_to_add_col_name <- paste(pay_rate_col, "_Longevity_Bonus")

  df[, new_col_name] <- df[[pay_rate_col]] * (1 + df$Longevity_Percent)
  df[, amnt_to_add_col_name] <- df[[pay_rate_col]] * df$Longevity_Percent

  return(df)
}


# Add 12 month FY 1.0 FTE 'Annualized' Salary
