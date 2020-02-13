# LOAD PACKAGES
install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,survival,survminer,plyr) 

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
df$td.asperg <- ifelse(df$td.asperg == 999,NA,df$td.asperg)

df$e <- ifelse(df$exp.asper == "Yes","1","0")
df$e <- as.numeric(df$e)

#Survival fitting
surv_object <- Surv(time=df$td.asperg, event = df$e)
fit1 <- survfit(surv_object ~ df$surg, data = df)
summary(fit1)
#survival plot
tiff("surv.tiff", units="in", width=5, height=5, res=300)
ggsurvplot(fit1, data = df, pval = TRUE,
           xlab = "Time (days)", ylab = "Survival probability",
           conf.int=TRUE, font.x=c(12,"bold"), font.y=c(12,"bold"), risk.table = TRUE,
           ggtheme = theme_bw(), break.time.by = 50, palette=c("#808080", "#2E9FDF"))

#clean up
rm(list = ls())
cat("\014")  
dev.off()
