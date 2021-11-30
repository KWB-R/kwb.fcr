# Pakete Laden
library(readxl)
library(dplyr)

# Funktionen laden und Tabellen erstellen
folder <- "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/R-Skript/"
source(file = paste0(folder, "TGD_Boden_Excel_Sheets.R"))

FertID <- data.frame(
  "ID" = c(1:10), 
  "Name" = c("Entw. Klärschlamm", "Struvite aus Schlamm", 
             "Struvite aus Schlammwasser", "Schlammasche", "AshDec", 
             "Mineraldünger techn. P-Säure", "Rohphosphat", "Mineraldünger", 
             "Decadmierter Minerald.", "Nur Deposition"), 
  "Eng" = c("dewatered sewage sludge", "struvite from sludge", 
            "struvite form sludge water", "sludge ash", "AshDec - treated",
            "mineral fertilizer (technical P-acid)", "phosphate rock", 
            "mineral fertilizers", "mineral fertilizers (decadmiation)", 
            "deposition only"),
  "Color" = c(hsv(h = c(seq(0.5514184,1, 0.1), seq(0.0514184,0.55, 0.1))[1:9],
                s = c(1,1,0.8), 
                v = c(0.7372549,1)),
                hsv(0,0,0)),
  "lineType" = c(rep("solid", 9), "dashed"),
  stringsAsFactors = FALSE)

comps <- data.frame("comp" = c("soil", "leach", "human"), 
                    "deutsch" = c("Bodenorganismen", "Grundwasser", "Mensch"),
                    "english" = c("Soil organisms", "groundwater", "human"),
                    stringsAsFactors = FALSE)
##############################################################################
################################ Daten Laden #################################
##############################################################################
Sub <- read_excel(path = "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/R-Skript/Substance_Sheet.xlsx", 
                  sheet = "Input", col_names = TRUE, na = "NA")

Env <- read_excel(path = "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/R-Skript/Environment_Sheet.xlsx", 
                  sheet = "Input", col_names = TRUE, na = "NA")


if(FALSE){
  ##############################################################################
  ################################ Berechnung ##################################
  ##############################################################################
  # fertilizer = 0: only background
  # fertilizer = 1: dewatered sewage sludge
  # Fertilizer = 2: Struvite (sludge)
  # Fertilizer = 3: Struvite (Sluddge water)
  # Fertilizer = 4: Sewage sludge ash
  # Fertilizer = 5: AshDev-treated ash
  # Fertilizer = 6: Mineral fertilizer from technical P-acid
  # fertilizer = 7: Phosphate Rock
  # Fertilizer = 8: Mineral Fertilizer 
  # Fertilizer = 9: Mineral Fertilizer after decadmation
  Substance <- "Cd"
  PPCP <- FALSE
  Fertilizer <- c(2,3,5,7,10)
  start_with_0 <- FALSE
  K_d <- "adapt2" 
  # "adapt1" -> calculation via log(k_d)- value
  # "adapt2" -> calculation via c_soil / log(c_water) 
  # "other" -> either calculation via f_oc and K_oc or entered K_d value
  BCF = "adapt2"
  # "adapt1" -> calculation via log(BCF)- value
  # "adapt2" -> calculation via log(c_plant) / c_soil 
  # "other" -> either calculation via f_oc and K_oc or entered K_d value
  vPNEC_organisms <- FALSE
  
  result <- RunTGD(runs = 100, years = 100, Substance = Substance, K_d = K_d,
                Fertilizer = Fertilizer, BCF = BCF, FertID = FertID, PPCP = PPCP,
                set_initial_concentration = FALSE, vPNEC_organisms = vPNEC_organisms)
  
  if(start_with_0){
  Sub2 <- rewrite_sub(table = Sub, parameter = "c_i", distribution = "none", 
                      substance = Substance,value_1 = 0, value_2 = NA)

  result_no_c0 <- RunTGD(runs = 10000, years = 100, Substance = Substance, K_d = "adapt2",
                   Fertilizer = Fertilizer, BCF = "adapt2", FertID = FertID)
  }
  
  summary(result$leach / result$variableInput$MF)

  rm(list=setdiff(x = ls(), y = c("result", "result_no_c0", "Substance", 
                                  "Fertilizer", "FertID", "Sub", "Env", "comps")))
  save.image(
    paste0(
      "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/R-Skript/results_",
      Substance, Sys.Date(), ".RData"))
}