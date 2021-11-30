source(file = "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/R-Skript/TGD_Boden_Excel_Sheets.R")
load(file = "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/Results/Benzo2018-06-25.RData")


# Farben bestimmen
# Transparente Farben für "orange" und "steelblue"
nurec <- rgb(red = 255, green = 165, blue = 0, alpha = 10, maxColorValue = 255)
b2 <- rgb(red = 70, green = 130, blue = 180, alpha = 10, maxColorValue = 255)
KWB_blue <- rgb(red = 0, green = 130, blue = 188, maxColorValue = 255)

if(FALSE){
  ##############################################################################
  ################################## Plotten ###################################
  ##############################################################################
  # fertilizer = 0: background
  # fertilizer = 1: dewatered sewage sludge
  # Fertilizer = 2: Struvite (sludge)
  # Fertilizer = 3: Struvite (Sluddge water)
  # Fertilizer = 4: Sewage sludge ash
  # Fertilizer = 5: AshDec-treated ash
  # Fertilizer = 6: Mineral fertilizer from technical P-acid
  # fertilizer = 7: Phosphate Rock
  # Fertilizer = 8: Mineral Fertilizer 
  # Fertilizer = 9: Mineral Fertilizer after decadmation
  # Fertilizer = 10: Fertilizer with 0 pollutant --> deposition only
  
  Substance <- "Cd"
  Fertilizer_high <- c(1)
  Fertilizer <- c(2,3,4,5,6,7,8,9)
  german <- F
  plotPath <- "Y:/AUFTRAEGE/UFO-Phorwärts/Data-Work packages/Risikomodell/Results/Plots/"
  result <- result_ni
  var_par <- variableInput(Sub = Sub, Env = Env, Substance = Substance)
  
  # Einträge und Austräge --------------
  dev.new(noRStudioGD = TRUE, width = 8, height = 5)
  SoilInputOutput(Sub = Sub,
                  Substance = Substance, 
                  Output = F, 
                  df = result$variableInput, 
                  FertID = FertID,
                  Fertilizer = Fertilizer,
                  german = german, 
                  mean_app = 60, 
                  sd_app = 10)
  savePlot(filename = paste0(
    plotPath, 
    Substance, 
    ifelse(german, "_Eintrag_Austrag", "_input_output")), 
    type = "wmf")
  dev.off()
  
  dev.new(noRStudioGD = TRUE, width = 6, height = 5)
  SoilInputOutput(Sub = Sub,
                  Substance = Substance, 
                  Output = FALSE, 
                  df = result[[which(names(result) == comps$comp[1])]], 
                  FertID = FertID,
                  Fertilizer = Fertilizer,
                  german = german, 
                  mean_app = 60, 
                  sd_app = 10)
  
  savePlot(filename = paste0(
    plotPath, 
    Substance, ifelse(german, "_Eintrag", "_input")), 
    type = "wmf")
  dev.off()
  
  # Dünger Spezifisch
  for(i in 1:3){
    # Alle Berechneten Dünger
    dev.new(noRStudioGD = TRUE, width = 8, height = 6)
    soilQuants <- CumSum100(
      df = result[[which(names(result) == comps$comp[i])]], 
      Fertilizer = Fertilizer, 
      xmin = 0.001, 
      xmax = 10, 
      xlab = paste0(ifelse(german, "Risikoquotient - ", "Risk quotient - "),
                    ifelse(german ,comps$deutsch[i], comps$english[i])), 
      ylab = ifelse(german, 
                    "Anteil der Überschreitung des Risikoquotienten [%]",
                    "Share of exceeding risk quotient [%]"),
      german = german)
    
    savePlot(filename = paste0(
      plotPath, 
      Substance, 
      "_RQ_", ifelse(german, comps$deutsch[i], comps$english[i])), 
      type = "wmf")
    dev.off()
    
    write.table(x = soilQuants, file = paste0(
      plotPath, 
      Substance, 
      "_quantsTable_",
      ifelse(german, comps$deutsch[i], comps$english[i]), ".csv"), 
      sep = ";", dec = ".")
    
    # Dünger, die die Ausgangsbelastung erhöhen
    dev.new(noRStudioGD = TRUE, width = 8, height = 6)
    soilQuants <- CumSum100(
      df = result[[which(names(result) == comps$comp[i])]], 
      Fertilizer = Fertilizer_high, 
      xmin = 0.001, 
      xmax = 100, 
      xlab = paste0(ifelse(german, "Risikoquotient - ", "Risk quotient - "),
                    ifelse(german ,comps$deutsch[i], comps$english[i])), 
      ylab = ifelse(german, 
                    "Anteil der Überschreitung des Risikoquotienten [%]",
                    "Share of exceeding risk quotient [%]"),
      german = german)
    savePlot(filename = paste0(
      plotPath, 
      Substance, 
      "_RQ_light_", ifelse(german, comps$deutsch[i], comps$english[i])), 
      type = "wmf")
    dev.off()
    
    # Korrelationen ----------------------
    dev.new(noRStudioGD = TRUE, width = 8, height = 5)
    spear <- plotSpearman(input = result, 
                          Fertilizer = Fertilizer, 
                          compartment = comps$comp[i], 
                          plot_t0 = T,
                          ylab = ifelse(german, 
                                        "Rangkorrelationskoeffizient", 
                                        "rank correlation coefficient"),
                          varPar = var_par,
                          FertID = FertID,
                          german = german)
    savePlot(filename = paste0(
      plotPath, 
      Substance, 
      "_cor_",
      ifelse(german, comps$deutsch[i], comps$english[i])), 
      type = "wmf")
    
    dev.off()
    
    write.table(x = spear, file = paste0(
      plotPath, 
      Substance, 
      "_corTable_",
      ifelse(german, comps$deutsch[i], comps$english[i]), ".csv"), 
      sep = ";", dec = ".")
  }
}


