#' Read environmental and pollutant variables
#'
#' Reads Excel files with environmnetal proporties and substance coefficents.
#'
#' Both input files must have the same path. The pollutant excel files name must
#' be "'pollutantName'_sheet.xlsx".
#'
#' @param path Path of both Excel files
#' @param pollutantName Name of Pollutant as in filename
#'
#' @return List of 2 tables for environment and pollutant characterstics
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

  list("Sub" = substance, "Env" = envi)
}
