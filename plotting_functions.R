source("./fake_bands_legend_code.R")

field_plot_fun <- function(scode, yrcurrent, yrstart, yrprev){
  
  wbplot_DO <- 
    plotWaterBands(site = scode, year_current = yrcurrent, 
                   years_historic = yrstart:yrprev, 
                   parameter = "DO_mgL") 
  
  wbplot_pH <- 
    plotWaterBands(site = scode, year_current = yrcurrent, 
                   years_historic = yrstart:yrprev, 
                   parameter = "pH")
  
  wbplot_sc <- 
    plotWaterBands(site = scode, year_current = yrcurrent, 
                   years_historic = yrstart:yrprev, 
                   parameter = "SpCond_uScm")
  
  wbplot_temp <- 
    plotWaterBands(site = scode, year_current = yrcurrent, 
                   years_historic = yrstart:yrprev, 
                   parameter = "Temp_F")
  
  wbn_plot <- 
    grid.arrange(wbplot_DO, wbplot_pH, wbplot_temp, wbplot_sc, legg, 
                 layout_matrix = rbind(c(1, 2),
                                       c(3, 4),
                                       c(5, 5)),
                 heights = c(0.5, 0.5,  0.25),
                 nrow = 3, ncol = 2)
  return(wbn_plot)
}

lab_plot_fun <- function(scode, yrcurrent, yrstart, yrprev){
  wbplot_TN <- tryCatch(plotWaterBands(site = scode, year_current = yrcurrent, 
                                       years_historic = yrstart:yrprev, parameter = "TN_mgL"), 
                        error = function(e){NULL})
  
  wbplot_TP <- tryCatch(plotWaterBands(site = scode, year_current = yrcurrent, 
                                       years_historic = yrstart:yrprev, 
                                       parameter = "TP_ugL"),
                        error = function(e){NULL})
  
  wbplot_ANC <- tryCatch(plotWaterBands(site = scode, year_current = yrcurrent, 
                                        years_historic = yrstart:yrprev, 
                                        parameter = "ANC_ueqL"),
                         error = function(e){NULL})
  
  wbplot_ChlA <- tryCatch(plotWaterBands(site = scode, year_current = yrcurrent, 
                                         years_historic = yrstart:yrprev, 
                                         parameter = "ChlA_ugL"), 
                          error = function(e){NULL})
  
  wbp_plot <- 
    if(is.null(wbplot_ANC) & is.null(wbplot_ChlA) & is.null(wbplot_TN) & is.null(wbplot_TP)){
      paste0("No data for site = ", sitecode, " and year_current = ", yr_current, ".")
    } else if(!is.null(wbplot_ANC) & !is.null(wbplot_ChlA)){
      grid.arrange(wbplot_TN, wbplot_TP, wbplot_ANC, wbplot_ChlA, legg,
                   layout_matrix = rbind(c(1, 2),
                                         c(3, 4),
                                         c(5, 5)),
                   heights = c(0.5, 0.5,  0.25),
                   nrow = 3, ncol = 2)
    } else if(!is.null(wbplot_ANC) & !is.null(wbplot_TN) & 
              is.null(wbplot_TP) & is.null(wbplot_ChlA)){ # for ACKEBO 2024
      grid.arrange(wbplot_ANC, wbplot_TN, legg,
                   layout_matrix = rbind(c(1, 2),
                                         c(3, 3)),
                   heights = c(0.9, 0.25),
                   nrow = 2, ncol = 2)
    } else if(!is.null(wbplot_ANC)){
      grid.arrange(wbplot_TN, wbplot_TP, wbplot_ANC, legg,
                   layout_matrix = rbind(c(1, 2),
                                         c(3, NA),
                                         c(4, 4)),
                   heights = c(0.5, 0.5,  0.25),
                   nrow = 3, ncol = 2)
    } else if(!is.null(wbplot_ChlA)){
      grid.arrange(wbplot_TN, wbplot_TP, wbplot_ChlA, legg,
                   layout_matrix = rbind(c(1, 2),
                                         c(3, NA),
                                         c(4, 4)),
                   heights = c(0.5, 0.5,  0.25),
                   nrow = 3, ncol = 2)
    } else {
      grid.arrange(wbplot_TN, wbplot_TP, legg,
                   layout_matrix = rbind(c(1, 2),
                                         c(3, 3)),
                   heights = c(0.9, 0.25),
                   nrow = 2, ncol = 2)
    }
  
  return(wbp_plot)
}