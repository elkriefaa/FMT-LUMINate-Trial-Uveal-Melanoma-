library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)


######################
### WATERFALL PLOT ###
######################


#All uveal melanoma patients

recist <- read_excel("Best_response_waterfall.xlsx", 
                     sheet = "recist")
# Choose subgroup

recist <- recist[recist$tumor_type == "Uveal melanoma",]

recist <- recist %>%
  arrange(desc(best_percentage_change)) %>%
  mutate(response_percent = best_percentage_change * 100) %>%
  select(response_percent, best_response, id_patient, tumor_type)

colors <- case_when(
  recist$best_response == "PR" ~ "#657fdb",
  recist$best_response == "PD" ~ "#f06f59",
  recist$best_response == "Early termination/death" ~ "#f5cdc6"
)

test <- barplot(recist$response_percent, 
                col = colors, 
                border = "white", 
                space = 0.1, 
                ylim = c(-100, 100),  
                ylab = "Change from baseline (%)",
                axes = FALSE)

axis(2, at = seq(-100, 100, by = 20), las = 2)
abline(h = -30, col = "black", lty = "dashed", lwd = 1)
abline(h = 20, col = "black", lty = "dashed", lwd = 1)
abline(h = 0, col = "black", lty = "solid", lwd = 1)


# Add asterisks for "Early termination/death"
early_termination <- which(recist$best_response == "Early termination/death")
text(test[early_termination], recist$response_percent[early_termination] + 5, 
     labels = "*", col = "black", cex = 1)
# Adding a footnote
mtext("* Disease progression due to clinical progression or death", side = 1, line = 3, cex = 0.7, font = 3, adj = 0)

legend("topright",
       legend = c("Partial Response (PR)",
                  "Progressive Disease (PD)",
                  "Early termination/death"),
       fill = c("#657fdb","#f06f59","#f5cdc6"),
       bty = "n",
       inset = c(0.08, -0.08),
       xpd = TRUE)
