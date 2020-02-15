# LOAD PACKAGES
install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,
       gridExtra, survival,survminer,plyr) 

#START HERE
#import fungal stats
fung_csv <- import("fung.csv")
df <- data.frame(fung_csv)

for (i in colnames(df[,3:33])){   
  df[[i]] <- factor(df[[i]],levels = c("0","1"),labels=c("No","Yes"))  
}

for (i in colnames(df[,35:37])){   
  df[[i]] <- factor(df[[i]],levels = c("0","1"),labels=c("No","Yes"))  
}

sink(file="output.txt")
#Summary tables
df_sum <- df[, -grep(".f$", colnames(df))]
explanatory = c(colnames(df_sum[,3:20]))
dependent = 'exp.asper'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE)

#Regression Table dysphagia
explanatory = c("surg","vcz","ampb", "lampb","itz","fluco")
dependent = 'exp.asper'
finalfit(df,dependent, explanatory, metrics=TRUE, na_to_missing = TRUE)

##Survival Plot
#convert variables to proper format for surv fitting
df$td.asperg <- replace_na(df$td.asperg, 240)
df$td.asperg <- ifelse(df$td.asperg == 999,NA,df$td.asperg)

df$e <- ifelse(df$exp.asper == "Yes","1","0")
df$e <- as.numeric(df$e)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Survival plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
require("survival")
vczf <- survfit( Surv(td.asperg, e) ~ vcz,
                 data = df )
itzf <- survfit( Surv(td.asperg, e) ~ itz,
                 data = df )
surgf <- survfit( Surv(td.asperg, e) ~ surg,
                 data = df )

require("survminer")
splots <- list()

splots[[1]] <- ggsurvplot(vczf, data = df, risk.table = FALSE, 
                          xlab = element_blank(), ylab = "",
                          xlim = c(0,240), ylim=c(0.6,1), 
                          pval.coord = c(1,.65),pval = TRUE, pval.size = 2,
                          conf.int=TRUE, font.x=c(12,"bold"), font.y=c(12,"bold"),
                          legend.title = "VCZ",legend.labs = c("No","Yes"), legend = c("right"),
                          ggtheme = theme_bw(), break.time.by = 30, palette=c("#808080", "#2E9FDF"))

splots[[2]] <- ggsurvplot(itzf, data = df, risk.table = FALSE, 
                          xlab = element_blank(), ylab = "Survival probability",
                          xlim = c(0,240), ylim=c(0.6,1), 
                          pval.coord = c(1,.65), pval = TRUE,pval.size = 2,
                          conf.int=TRUE, font.x=c(12,"bold"), font.y=c(12,"bold"),
                          legend.title = "ITZ",legend.labs = c("No","Yes"), legend = c("right"),
                          ggtheme = theme_bw(), break.time.by = 30, palette=c("#808080", "#2E9FDF"))

splots[[3]] <- ggsurvplot(surgf, data = df, risk.table = FALSE,
                          xlab = "Time (days)", ylab = "",
                          xlim = c(0,240), ylim=c(0.6,1), 
                          pval.coord = c(1,.65), pval = TRUE,pval.size = 2,
                          conf.int=TRUE, font.x=c(12,"bold"), font.y=c(12,"bold"),
                          legend.title = "Surgery",legend.labs = c("No","Yes"), legend = c("right"),
                          ggtheme = theme_bw(), break.time.by = 30, palette=c("#808080", "#2E9FDF"))

# Arrange multiple ggsurvplots and print the output
tiff("surv.tiff", units="in", width=5, height=5, res=300)
arrange_ggsurvplots(splots,
                    ncol = 1, nrow = 3)
dev.off()

#clean up
rm(list = ls())
cat("\014")  
dev.off()
