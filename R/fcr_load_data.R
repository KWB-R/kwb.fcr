#' Read environmental and pollutant variables
#'
#' Reads Excel files with environmnetal proporties and substance coefficents.
#'
#' Both input files must have the same path. The pollutant excel files must be
#' named "'pollutantName'_sheet.xlsx". Furthermore, both excel tables must have
#' the same number of columns and column names.
#'
#' @param input_path Path of the folder "pollutants" and "sites" that contain
#' the input data excel sheets
#' @param pollutantName Name of Pollutant as in filename of the pollutant
#' (prefix before "_sheet.xslx)
#' @param siteName Name of the site as in filename of the environment sheet
#' (prefix before "_sheet.xslx)
#' @param fertilizerName Name of the fertilizer as in filename of the fertilizer
#' sheet (prefix before "_sheet.xslx). If fertilizerName = "none", fertilizer
#' application and pollutant concentration will be set to 0.
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
  input_path, pollutantName, siteName, fertilizerName = "none"
){

  # Load substance and site information
  substance_path <- file.path(input_path, "pollutants")
  if(!any(dir(substance_path) == paste0(pollutantName, "_sheet.xlsx"))){
    stop(paste0("No pollutant called '", pollutantName, "'"))
  }
  site_path <- file.path(input_path, "sites")
  if(!any(dir(site_path) == paste0(siteName, "_sheet.xlsx"))){
    stop(paste0("No site called '", siteName, "'"))
  }

  substance <- readxl::read_excel(
    path = file.path(substance_path, paste0(pollutantName, "_sheet.xlsx")),
    sheet = "input", col_names = TRUE, na = "NA")
  info <- readxl::read_excel(
    path = file.path(substance_path, paste0(pollutantName, "_sheet.xlsx")),
    sheet = "additional_infos", col_names = TRUE, na = "NA")

  envi <- readxl::read_excel(
    path = file.path(site_path, paste0(siteName, "_sheet.xlsx")),
    sheet = "input", col_names = TRUE, na = "NA")

  # load fertilizer information
  if(fertilizerName == "none"){
    fert <- data.frame(
      "parameter" = c("fert_app", "c_fert"),
      "description" = rep("", 2),
      "unit" = c("kg / (ha*a)", "mg/kg"),
      "value_1" = rep(0,2), "value_2" = rep(0,2), "shift" = rep(0,2),
      "distribution" = rep("none", 2),
      "site_specific" = rep(TRUE, 2),
      "references" = rep("", 2), comments = rep("", 2))
  } else {
    fertilizer_path <- file.path(input_path, "fertilizers")
    if(!any(dir(fertilizer_path) == paste0(fertilizerName, "_sheet.xlsx"))){
      stop(paste0("No fertilizer called '", fertilizerName, "'"))
    }
    fert <- readxl::read_excel(
      path = file.path(fertilizer_path, paste0(fertilizerName, "_sheet.xlsx")),
      sheet = "input", col_names = TRUE, na = "NA")

    polRow <- grep(fert$parameter, pattern = paste0(pollutantName, "_"))
    if(length(polRow) == 0L){
      stop(paste0("No pollutant called '", pollutantName,
                  "' definied in fertilizer '", fertilizerName, "'"))
    }
    fert$parameter[polRow] <- "c_fert"
    fert <- fert[fert$parameter %in% c("fert_app", "c_fert"),]
  }

  df_in <- do.call(rbind, list(substance, envi, fert))
  df_in <- df_in[,c("parameter", "value_1", "value_2", "shift",
                                     "distribution", "site_specific")]
  df_in$site_specific <- as.logical(df_in$site_specific)
  # the unique ID is needed to set seeds for site specific variables later on
  df_in$pID <- 1:nrow(df_in)
  del_rows <- which(is.na(df_in$value_1))
  if(length(del_rows) > 0){
    df_in <- df_in[-del_rows,]
  }
  list("dat" = split(df_in, df_in$parameter),
       "info" = info)
}

#' Read Additional pollutant information
#'
#' Reads the sheet "additional_infos" from the pollutant Excel file. The
#' pollutant excel files must be named "'pollutantName'_sheet.xlsx".
#'
#' @param path Path of both Excel files
#' @param pollutantName Name of Pollutant as in filename
#'
#' @return
#' A table containing the addiational information.
#'
#' @export
#' @importFrom readxl read_excel
#'
additional_substanc_info <- function(
  path, pollutantName
){
  readxl::read_excel(
    path = file.path(path, paste0(pollutantName, "_sheet.xlsx")),
    sheet = "additional_infos", col_names = TRUE, na = "NA")
}
