convert_to_numeric <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    } else {
      warning(paste("Column", col, "not found in the dataframe."))
    }
  }
  return(df)
}