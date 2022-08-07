library(readxl)
library(DT)
library(metafor)
library(dplyr)
library(ggplot2)

regtest(yi, vi, data = odonata_data1)

par(mfrow = c(4, 3))
par(mar = c(1.3, 1, 1.2, 1.2))


add_value <- seq(0.1, 1, 0.1)

for(i in 1:length(add_value)){
  odonata_data1 <- read_excel("/Users/82109/Desktop/BEC/2차 논문/Data extraction/data_extraction 7.xlsx")
  
  odonata_data1$new_measurement_c <- odonata_data1$mean_c + add_value[i]
  odonata_data1$new_measurement_p <- odonata_data1$mean_p + add_value[i]
  
  lnRR <- escalc(measure = "ROM", n1i = odonata_data1$n_p, n2i = odonata_data1$n_c, m1i = odonata_data1$new_measurement_p, m2i = odonata_data1$new_measurement_c, sd1i = odonata_data1$sd_p, sd2i = odonata_data1$sd_c)
  
  odonata_data1 <- bind_cols(odonata_data1, lnRR)
  
  odonata_results_with_Literature <-
    rma.mv(yi, vi, random = ~ 1 | Literature/Study, slab = paste(Literature, Year, sep = ""), data =  odonata_data1)

funnel(odonata_results_with_Literature, yaxt = "n", xaxt = "n", ylab = "", xlab = "", pch = 1)
title(xlab = add_value[i], line = 0.2, cex.lab = 1.3)          
  
}



