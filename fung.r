# LOAD PACKAGES
install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,survival,survminer,plyr) 

#START HERE
#import fungal stats
dbs_csv <- import("fung.csv")
df <- data.frame(dbs_csv)

df <- df %>% mutate_if(is.numeric,as.factor)
df$Age <- as.numeric(as.character(df$Age))
df$num.agent <- as.numeric(as.character(df$num.agent))

#Summary tables
explanatory = subset(df, select = surg:num.agent)
dependent = 'exp.asper'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE)
