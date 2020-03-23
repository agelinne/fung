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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Expired Aspergilosis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Summary tables
df_sum <- df[, -grep(".f$", colnames(df))]
explanatory = c(colnames(df_sum[,2:20]))
dependent = 'exp.asper'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE, column = FALSE)

#Regression Table expired
explanatory = c("surg","vcz","ampb", "lampb","itz","fluco","num.agent")
dependent = 'exp.asper'
finalfit(df,dependent, explanatory, metrics=TRUE, na_to_missing = TRUE, column = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Expired other
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Summary Tables
explanatory = c(colnames(df_sum[,3:20]))
dependent = 'exp.other'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Recovery
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Summary Tables
explanatory = c(colnames(df_sum[,3:20]))
dependent = 'recov'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE)

#Regression Table recovery
explanatory = c("surg","vcz","ampb", "lampb","itz","num.agent")
dependent = 'recov'
finalfit(df,dependent, explanatory, metrics=TRUE, na_to_missing = TRUE)

##Survival Plot
#convert variables to proper format for surv fitting
df$t <- df$td.asperg
df$t <- ifelse(df$t== 999,NA,df$t)
df$td.asperg <- replace_na(df$td.asperg, 240)
df$td.asperg <- ifelse(df$td.asperg == 999,NA,df$td.asperg)

df$e <- ifelse(df$exp.asper == "Yes","1","0")
df$e <- as.numeric(df$e)

df$eo <- ifelse(df$exp.other == "Yes","1","0")
df$eo <- as.numeric(df$eo)

df <- mutate(df, rx = case_when(
  vcz == "Yes"  ~ 2,
  itz == "Yes" ~ 1,
  TRUE ~ 0
))

df$rx <- factor(df$rx, levels=c("0","1","2"), labels = c("Neither","ITZ","VCZ"))

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
tiff("surv.tiff", units="in", width=3.7, height=5, res=300)
arrange_ggsurvplots(splots,
                    ncol = 1, nrow = 3)
dev.off()

#Time Removal Plot
tiff("time.tiff", units="in", width=4, height=3, res=400)
mu <- ddply(subset(df,e=="1"), "surg", summarize, grp.mean=mean(t,na.rm=TRUE))
ggplot(subset(df,e=="1"), aes(x=t, fill = surg)) + 
  geom_density(aes(color=surg),alpha=.5) + 
  geom_vline(data=mu, aes(xintercept=grp.mean, color=surg),
             linetype="dashed") +
  labs(y="Aspergillosis Expiration (density)",x="Time (days)", fill = "Surgery", colour = "Surgery") +
  scale_color_manual(values=c("#808080", "#2E9FDF")) + 
  scale_fill_manual(values=c("#808080", "#2E9FDF")) + theme_bw() +
  theme(axis.title.x=element_text(face="bold"), 
        axis.title.y=element_text(face="bold"),
        legend.title = element_text(face="bold"))
dev.off()

#Facet wraped survival plots
require("survival")
fit <- survfit(Surv(td.asperg, e) ~ vcz,
                 data = df )

# Visualize: plot survival curves facet by rx and surg
#++++++++++++++++++++++++++++++++++++
df$surg <- factor(df$surg, levels = c("No","Yes"), 
                  labels = c("No Surgery","Surgery"))

tiff("vcz.tiff", units="in", width=5, height=3, res=300)
ggsurvplot(fit, df, facet.by = c("surg"),
           conf.int = TRUE,
           xlim = c(0,240), ylim=c(0.5,1),
           pval.coord = c(0,.52), pval = TRUE,pval.size = .5,
           font.x=c(12,"bold"), font.y=c(12,"bold"),
           ggtheme = theme_bw(), break.time.by = 30,
           palette=c("#808080", "#2E9FDF"), legend.title = "VCZ",
           short.panel.labs = TRUE,panel.labs.font = list(12,"bold"))
dev.off()

#clean up
rm(list = ls())
cat("\014")  
dev.off()
