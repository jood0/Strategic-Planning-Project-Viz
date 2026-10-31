#are certain majors more commonly put together for double majors? 
#Can we predict what your second major will be if we know your first major?

# Libraries
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(stringr) 


# Load Data
majors <-read_csv("Double Majors all colleges.xlsx - Data.csv")

# DATA CLEANING -----------------------------------------------------------
names(majors)[names(majors) == "Student ID"] <- "ID"

majors <- majors %>%
  mutate(Major = sub(",", "", Major)) #remove the comma from the name of a major

# nonMajors<-majors[str_detect(majors$Major, "visitor|Undeclared|Extension"), ] 
# majors <- majors[!(majors$Major %in% nonMajors$Major), ]

# basket form of data ------------------------------------------------------
maData <- ddply(majors,c("ID"),
                         function(df1)paste(df1$Major,
                                            collapse = ","))

maData<-maData[str_detect(maData$V1, ","), ]  #remove student who don't have a second major 
maData<-maData[str_detect(maData$V1, "L... -"), ] # remove student who don't have a major in L&S

write.csv(maData,"major_basket.csv", quote = FALSE, row.names = FALSE)

tr1 <- read.transactions('major_basket.csv', format = 'basket', sep=',')
#sep tell how items are separated, using ','


# plot Frequency ------------------------------------------------------------------
# library(RColorBrewer)
# itemFrequencyPlot(tr1,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
# 
# itemFrequencyPlot(tr1,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")


# Generating Rules -------------------------------------------------------

#mine the rules using the APRIORI algorithm
association.rules <- apriori(tr1, parameter = list(supp=0.001, conf=0.5)) 

inspect(association.rules)

# Graph-Based Visualizations----------------------------------------------
theRules <- head(association.rules, n = 10, by = "confidence")
plot(theRules, method = "graph",  engine = "htmlwidget")
