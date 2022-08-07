library(readxl)
library(DT)
library(metafor)
library(dplyr)
library(ggplot2)

odonata_data1 <- read_excel("/Users/82109/Desktop/BEC/2차 논문/Data extraction/data_extraction 7.xlsx")

zval_vector <- c()
pval_vector <- c()
se_vector <- c()
estimate_vector <- c()
CImax_vector <- c()
CImin_vector <- c()
add_value <- seq(0.1, 1, 0.1)

for(i in 1:length(add_value)){
  odonata_data1 <- read_excel("/Users/82109/Desktop/BEC/2차 논문/Data extraction/data_extraction 7.xlsx")
  
  odonata_data1$new_measurement_c <- odonata_data1$mean_c + add_value[i]
  odonata_data1$new_measurement_p <- odonata_data1$mean_p + add_value[i]
  
    lnRR <- escalc(measure = "ROM", n1i = odonata_data1$n_p, n2i = odonata_data1$n_c, m1i = odonata_data1$new_measurement_p, m2i = odonata_data1$new_measurement_c, sd1i = odonata_data1$sd_p, sd2i = odonata_data1$sd_c)
  
  odonata_data1 <- bind_cols(odonata_data1, lnRR)
  
  odonata_results_with_Literature <-
    rma.mv(yi, vi, random = ~ 1 | Literature/Study, slab = paste(Literature, Year, sep = ""), data =  odonata_data1)
  
  estimate_vector[i] <- odonata_results_with_Literature$b[1]
  CImax_vector[i] <- odonata_results_with_Literature$ci.ub[1]
  CImin_vector[i] <- odonata_results_with_Literature$ci.lb[1]
  zval_vector[i] <- odonata_results_with_Literature$zval[1]
  se_vector[i] <- odonata_results_with_Literature$se[1]
  pval_vector[i] <- odonata_results_with_Literature$pval[1]
 
RR_vector <- predict(odonata_results_with_Literature, transf = exp, digits = 2) 
print(RR_vector)

I2 <- var.comp(odonata_results_with_Literature)
summary(I2)

}

print(estimate_vector)
print(se_vector)
print(RR_vector)
print(zval_vector)
print(pval_vector)
print(CImax_vector)

sensetivity_data <- data.frame(add_value, estimate_vector, CImax_vector, CImin_vector)
png("results.png", res = 350, width = 25, height = 20, units = "cm")
ggplot(sensetivity_data, aes(x = add_value, y = estimate_vector)) + 
  geom_point() +
  geom_errorbar(aes(ymin = CImin_vector, ymax = CImax_vector), width=0.05) + 
  scale_x_continuous(breaks=seq(0, 1, 0.1)) +
  labs(y = "Estimate (overall LRR)", x = "Added value to mean") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major = element_line(colour = "grey90"), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1, colour = "grey"), axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 15)), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20))
dev.off()










