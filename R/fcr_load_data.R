#' Read environmental and pollutant variables
#'
#' Reads Excel files with environmnetal proporties and substance coefficents.
#'
#' Both input files must have the same path. The pollutant excel files must be
#' named "'pollutantName'_sheet.xlsx". Furthermore, both excel tables must have
#' the same number of columns and column names.
#'
#' @param path Path of both Excel files
#' @param pollutantName Name of Pollutant as in filename
#'
#' @return List of one row data frames.
#' The length equals the number of non NA input variables. Data frames include
#' information about distribution type, distribution parameters, site specific
#' character and a unique variable ID
#'
#' @export
#' @importFrom readxl read_excel
#'
read_fcr_input <- function(
  path, pollutantName
){

  substance <- readxl::read_excel(
    path = file.path(path, paste0(pollutantName, "_sheet.xlsx")),
    sheet = "input", col_names = TRUE, na = "NA")

  envi <- readxl::read_excel(
    path = file.path(path, "environment_sheet.xlsx"),
    sheet = "input", col_names = TRUE, na = "NA")

  df_in <- rbind(substance, envi)[,c("parameter", "value_1", "value_2", "shift",
                                     "distribution", "site_specific")]
  df_in$site_specific <- as.logical(df_in$site_specific)
  # the unique ID is needed to set seeds for site specific variables later on
  df_in$pID <- 1:nrow(df_in)
  del_rows <- which(is.na(df_in$value_1))
  if(length(del_rows) > 0){
    df_in <- df_in[-del_rows,]
  }
  split(df_in, df_in$parameter)
}
