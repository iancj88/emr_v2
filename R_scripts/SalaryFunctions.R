# Add Longevity Years column
#
#
AddLongevityYearsCol <- function(df,
                                 long_date_col_name,
                                 emr_job_type_col_name,
                                 curr_date_col_name     ) {
  require(lubridate)
  long_dates <- df[,long_date_col_name]
  curr_dates <- df[,curr_date_col_name]

  long_years <- year(long_dates)
  curr_years <- year(curr_dates)

  long_months <- month(long_dates)
  curr_months <- month(curr_dates)
  pre_partial_year <- curr_months < long_months

  years_of_serv <- curr_years - long_years
  years_of_serv <- years_of_serv - pre_partial_year


}

# Add Longevity % column
#
#


# Add Actual Salary after longevity
#
#


# Add 12 month FY 1.0 FTE 'Annualized' Salary
