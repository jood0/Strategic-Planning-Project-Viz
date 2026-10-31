# install.packages(c(
#   "igraph",
#   "ggraph",
#   "tidygraph",
#   "readxl",
#   "stringr",
#   "data.table",
#   "edgebundleR",
#   "tidyr",
#   "dplyr",
#   "qgraph"
#   )
# )
#install.packages(c("readxl"))

# load libraries
library("igraph")
library("ggraph")
library("tidygraph")
library("readxl")
library("stringr")
library("data.table")
library("edgebundleR")
library("tidyr")
library("dplyr")
library("qgraph")
library("networkD3")
library("visNetwork")
library("ggplot2")
library("viridis")
library("hrbrthemes")

# set working directory
setwd("2024_ls_strategic_plan")

# read in .csv file
gradCenterData <- read.csv("data/GradCentersScraped.csv")

# get just center and column
gradCenterToGraph <- data.frame(gradCenterData$Center, gradCenterData$College)
colnames(gradCenterToGraph) = c("center", "college")

#create matrix
mm <- model.matrix(~ college - 1, data = gradCenterToGraph)
gradCenterMatrix <- data.frame(mm)
gradCenterMatrix$college = gradCenterToGraph$center

#change colnames
colnames(gradCenterMatrix) = c("Biological Sciences", 
                               "Agriculture and Environmental Science",
                               "Education",
                               "Engineering",
                               "Letters and Science",
                               "Law",
                               "Management",
                               "Medicine",
                               "Center")

# Specify the desired order of columns
new_order <- c("Center", 
               "Biological Sciences", 
               "Agriculture and Environmental Science",
               "Education",
               "Engineering",
               "Letters and Science",
               "Law",
               "Management",
               "Medicine")

# Reorder the dataframe
gradCenterMatrix <- gradCenterMatrix[, new_order]

# collapse
# Aggregate the dataframe by the "Center" column
gradCenterMatrixCollapsed <- aggregate(. ~ Center, data = gradCenterMatrix, FUN = sum)

#melt them... idk what this does but it WORKS!!!
gradCenterMatrixCollapsed_melted <- melt(gradCenterMatrixCollapsed, id.vars = "Center")

#stacked bar ggplot for NON-L&S colleges
ggplot(gradCenterMatrixCollapsed_melted, aes(fill=variable, y=Center, x=value)) +
  geom_bar(position="fill", stat="identity") +
  labs(title = "Faculty Appointments in Graduate Centers, by College",
       y = "Center",
       x = "College of Faculty Appointment",
       fill = "College") +
  theme_ipsum()