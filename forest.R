library(readxl)
library(DT)
library(metafor)
library(dplyr)
library(ggplot2)
library(dmetar)

odonata_data1 <- read_excel("/Users/82109/Desktop/BEC/2차 논문/Data extraction/data_extraction 7.xlsx")

  odonata_data1$new_measurement_c <- odonata_data1$mean_c + 0.1
  odonata_data1$new_measurement_p <- odonata_data1$mean_p + 0.1
  
  lnRR <- escalc(measure = "ROM", n1i = odonata_data1$n_p, n2i = odonata_data1$n_c, m1i = odonata_data1$new_measurement_p, m2i = odonata_data1$new_measurement_c, sd1i = odonata_data1$sd_p, sd2i = odonata_data1$sd_c)
  
  odonata_data1 <- bind_cols(odonata_data1, lnRR)
  
  odonata_results_with_Literature <-
    rma.mv(yi, vi, random = ~ 1 | Literature/Study, slab = paste(Literature, Year, sep = ""), data =  odonata_data1)

odonata_results_with_Literature  

png("forest.png", res = 350, width = 20, height = 25, units = "cm")
odonata_results_with_Literature_forest <- forest(odonata_results_with_Literature, annotate = TRUE, slab = odonata_results_with_Literature$slab, xlab = "LRR", header = FALSE, mlab = "", col = "orange", cex = .75, pch = 20, cex.lab = 1, width = 7, psize = 1.1, alim = c(-7.5, 5), at = c(-7.5, -5, 0, 5), xlim = c (-12, 9))
text(-12, 57, "Paper & Year", pos = 4, font = 2)                        
text(5.1, 57, "ln(RR) [95% CI]", pos = 4, font = 2) 
text(-12, -1, "Summary", pos= 4, font =2)
dev.off()





