#Create individual dataframes based on unique values of a single column in a dataframe
SplitDFIntoDFListByCol <- function(df, uniqueColName) {
  #get list of unique ids
  unique_names <- unique(df[[uniqueColName]])
  new_list <- lapply(unique_names, function(x, df) {df[df[[uniqueColName]] %in% c(x),]}, df)
  names(new_list) <- unique_names

  return(new_list)
}

#Pad the gid with zeros
#
PadGID <- function(gid_vec) {
  default_length_of_Gid <- 9
  if (sum(str_length(gid_vec) > default_length_of_Gid,  na.rm = TRUE) > 0) {
    return(gid_vec)
  }

  gid_numbers <- as.numeric(gid_vec)
  gid_vec <- sprintf("%09d", gid_numbers)
  return(gid_vec)
}



#Create individual dataframes based on unique values of a single column in a dataframe
SplitDFIntoDFListByCol <- function(df, uniqueColName) {
  #get list of unique ids
  unique_names <- unique(df[[uniqueColName]])
  new_list <- lapply(unique_names, function(x, df) {df[df[[uniqueColName]] %in% c(x),]}, df)
  names(new_list) <- unique_names

  return(new_list)
}

JoinDataToDF <- function(df, df_lu, key_main, key_lu)  {
  #save the original column names that will be swapped out with the constant "Key"
  temp_field_name_main <- key_main
  temp_field_name_lu <- key_lu


  #Key_Unique_To_Program_232323 should be unique, but if it's not.... fuck it, i'll come back and figure
  # something else out
  df <- RenameColumn(df, old_name = key_main, new_name = "Key_Unique_To_Program_232323")
  df_lu <- RenameColumn(df_lu, old_name = key_lu, new_name = "Key_Unique_To_Program_232323")

  df <- left_join(df, df_lu, by = c("Key_Unique_To_Program_232323" = "Key_Unique_To_Program_232323"))


  #put back the original colnames
  df <- RenameColumn(df, old_name = "Key_Unique_To_Program_232323", new_name = key_main)
  df_lu <- RenameColumn(df_lu, old_name = "Key_Unique_To_Program_232323", new_name = key_lu)
  return(df)
}

#quickly rename a column based on it's current name rather than location
RenameColumn <- function(df, old_name, new_name) {
  colnames(df)[which(names(df) == old_name)] <- new_name
  return(df)
}

GetFileType <- function(fname) {
  #check everything after the period
  ftypestr <- sub(pattern = ".*\\.", replacement = "", x = fname)

  #check it against the list of file names to extensions
  ftypemaster <- c("xlsx" = "excel", "xlsm" = "excel", "txt" = "csv", "csv" = "csv")
  type <- ftypemaster[ftypestr]
  return(type)
}

CheckForFileNotExist <- function(full_path) {
  doesNotExist <- !file.exists(full_path)
  return(doesNotExist)
}

CheckForDirNotExist <- function(fpath) {
  doesNotExist <- !dir.exists(fpath)
  return(doesNotExist)
}



WriteToFile <- function(df, fname, fpath, delim, addAsNewSht, sheet = 1) {
  require(openxlsx)
  require(readr)
  require(stringr)

  #determine the type of file to be written
  ftype <- GetFileType(fname)

  #create the total file path for existence checks
  full_path <- str_c(fpath, fname)

  if (CheckForDirNotExist(fpath)) {
    dir.create(fpath)
  }
  ftype <- GetFileType(fname)
  if (CheckForFileNotExist(full_path)) {
    if (!ftype == "excel") {
      file.create(full_path)
    }
  }




  #add logic to handle various file types here
  if (ftype == "csv") {
    if (missing(delim)) {delim <- ", "}
    write_delim(df, path = full_path, append = FALSE, delim = delim)
  } else if (ftype == "excel") {
    #
    # placeholder for a headerstyle and table styles
    #
    if(missing(addAsNewSht)) {addAsNewSht <- T}
    write.xlsx(df, file = full_path,
               creator = Sys.getenv("USERNAME"),
               startCol = 1, startRow = 2,
               colNames = TRUE, keepNA = TRUE,
               firstActiveRow = 3, colWidths = "auto",
               overwrite = TRUE)
  }
}
