AddAdCompStipendCols <- function(df,
                                 position_number_col_name,
                                 suffix_col_name,
                                 gid_col_name,
                                 key_date_col_name,
                                 assgn_sal_col_name,
                                 date_col_name) {
  adcmp_indx <- which(df[, position_number_col_name] == "4ADCMP")
  adcmp_cols <- c(gid_col_name, key_date_col_name, date_col_name, assgn_sal_col_name)
  adcmp_df <- df[adcmp_indx, one_]
}
