# GOAL: Create visualizations of the College of Letters & Sciences' undergraduate major data

# REFRENCES:
# https://ladal.edu.au/net.html#Network_Visualization

# Set Up ------------------------------------------------------------------

# FILES TO HAVE - MAKE "data" subfolder, this project uses 
# Major_Categories5.csv and Double Majors all colleges.xlsx

# install packages

# install.packages(c(
#   "igraph",
#   "ggraph",
#  "tidygraph",
#   "readxl",
#  "stringr",
#   "data.table",
#   "edgebundleR",
#   "ggplot2",
#   "reshape2",
#   "viridis", 
#   "hrbrthemes"
#   "googlesheets4"
#   "writexl",
#   "ggstance"
#   )
# )
#install.packages(c("readxl", "hrbrthemes"))
#install.packages(c("viridis", "hrbrthemes"))

#load libraries
library("igraph")
library("ggraph")
library("tidygraph")
library("readxl")
library("stringr")
library("data.table")
library("edgebundleR")
library("reshape2")
library("ggplot2")
library("viridis")
library("hrbrthemes")
library("dplyr")
library("writexl")
library("ggstance")

# set working directory
setwd("2024_ls_strategic_plan")
# read the data
majors<-read_excel(
  path = "./data/Double Majors all colleges.xlsx",
  sheet = 3)


# Functions ---------------------------------------------------------------

delete_rows <- function(df_cleaned, pair_forward, pair_backwards, unique_ab, start_with){
  
  i=start_with
  #for (i in 1:length(unique_ab)){
  print(paste("index: ", i))
  # print("starting loop with unique_ab: ")
  # print(unique_ab)
  #print(length(unique_ab))
  # print("pair list: ")
  # print(unique_ab)
  print(unique_ab[i]) 
  
  text_to_compare <- unique_ab[i]
  
  # if the index is greater than the length of unique_ab, we're done
  if (!is.na(text_to_compare)){ #i <= length(unique_ab) && length(which(is.na(unique_ab)))=0
    matches <- which(df_cleaned[,pair_backwards] == text_to_compare)
    
    print(paste("index of matches:", matches))
    #print(paste("length of matches:", length(matches)))
    
    
    if (length(matches)>0){ 
      #pipe it back through the function
      #print("found matches")
      
      ba <- df_cleaned[matches[1], pair_forward]
      print(paste("ba:",ba))
      unique_ab_new <- unique_ab[-which(unique_ab == ba)]
      
      # print(unique_ab)
      print(length(unique_ab_new))
      
      #print(paste("removing", ba))
      
      df_cleaned <- df_cleaned[-matches, ] 
      
      #print(df_cleaned)
      
      df_cleaned <- delete_rows(df_cleaned = df_cleaned, pair_forward = dim(df_cleaned)[2]-1, pair_backwards = dim(df_cleaned)[2], unique_ab = unique_ab_new, start_with=i+1)
      
    }else{
      #if there are no matches, continue in the loop
      # print("no matches found")
      # print("inside no matches loop")
      # print(unique_ab)
      #print(unique_ab)
      
      unique_ab_new <- unique_ab
      
      df_cleaned <- df_cleaned 
      
      #print(df_cleaned)
      
      df_cleaned <- delete_rows(df_cleaned = df_cleaned, pair_forward = dim(df_cleaned)[2]-1, pair_backwards = dim(df_cleaned)[2], unique_ab = unique_ab_new, start_with=i+1)
    }
  }else{
    #break
  }
  #print(df_cleaned)
  #} #end of for loop
  
  
  
  return(df_cleaned)
}


rm_unordered_pairs <- function(df, column_1, column_2){
  #col_1 & col_2 should be the column number
  
  
  # set up the needed structure
  df$ab <- paste(df[,column_1], df[,column_2], sep=" ")
  df$ba <- paste(df[,column_2], df[,column_1], sep=" ")
  
  unique_ab <- unique(df$ab)
  
  # print(unique_ab)
  # print(df)
  
  
  
  data_cleaned <- delete_rows(df_cleaned=df, pair_forward=(ncol(df)-1), pair_backwards =(ncol(df)), unique_ab = unique_ab, start_with=1)
  
  return(data_cleaned)
  
}

# separate major column in to major_code and major_name
split.majors<-as.data.frame(
  matrix(
    data = unlist(strsplit(majors$Major, " - ")),
    ncol = 2,
    byrow = TRUE
  ),
)
names(split.majors)<-c("major_code", "major_name")

majors<-cbind(majors, split.majors)

# which IDs have L$S majors?
ls_ids <- majors$`Student ID`[which(majors$College == "L")]

# subset the majors data to only IDs that have L&S majors
majors_ls <- majors[which(majors$`Student ID` %in% ls_ids),]
# remove visitors from other campuses
majors_ls <- majors_ls[which(str_detect(majors_ls$Major, "visitor", negate=TRUE)),]
#remove undeclared majors
majors_ls <- majors_ls[which(str_detect(majors_ls$Major, "Undeclared", negate=TRUE)),]

majors_cross <- crossprod(table(majors_ls[c(1,2)])) #use major, which includes the college

majors_cross <- as.data.frame(majors_cross)
diag(majors_cross) <- 0

#clean up row names, and make data workable
doubleMajors_total = data.frame(rowSums(majors_cross))
rownames <- row.names(doubleMajors_total)
doubleMajors_total = data.frame(rownames, rowSums(majors_cross))
colnames(doubleMajors_total) = c("major", "count")
row.names(doubleMajors_total) <- 1:nrow(doubleMajors_total)

#remove all entries that don't begin with L
doubleMajors_total = doubleMajors_total[grep("^L", doubleMajors_total$major),]

#get the amount of total people in each major
majors_total = data.frame(majors_ls$Major)
colnames(majors_total) = c("major")
majors_total$major = factor(majors_total$major)

#factor the list, and categorize to make the sums
majors_total <- as.data.frame(table(majors_total$major))
colnames(majors_total) = c("major", "count")

#remove all entries that don't begin with L
majors_total = majors_total[grep("^L", majors_total$major),]

percent_double_majoring = data.frame(majors_total$major, majors_total$count, doubleMajors_total$count)
colnames(percent_double_majoring) = c("major", "studentCount", "doubleMajorCount")
percent_double_majoring$percentDoubleMajoring <- percent_double_majoring$doubleMajorCount / percent_double_majoring$studentCount
colnames(percent_double_majoring) = c("major", "studentCount", "doubleMajorCount", "percentDoubleMajoring")

#read in the major categories
major_categories = read.csv("data/Major_Categories5.csv")

#remove dead majors that arent in the major categories
percent_double_majoring <- percent_double_majoring[percent_double_majoring$major %in% major_categories$major, ]
percent_double_majoring$group = major_categories$group

#get graphable data frame without binning first
percent_double_majoring_graphable = data.frame(percent_double_majoring$major, percent_double_majoring$group, percent_double_majoring$percentDoubleMajoring)
colnames(percent_double_majoring_graphable) = c("major", "group" , "percentDoubleMajoring")

#remove the code for the major
percent_double_majoring_graphable <- percent_double_majoring_graphable %>%
  mutate(major = str_replace(major, "^[^-]* - ", ""))

#bin the groups
binned_percent_double_majoring = data.frame(percent_double_majoring$group, percent_double_majoring$percentDoubleMajoring)
colnames(binned_percent_double_majoring) = c("group", "percentDoubleMajoring")

#bin by group
binned_percent_double_majoring <- binned_percent_double_majoring %>%
  group_by(group) %>%
  summarise_all(.funs = mean, na.rm = TRUE) #averages up the percent double majoring in each bin

#load in major / minor units
majorUnits = read.csv("data/programs_units.csv")

# remove minors
majorUnits <- majorUnits %>%
  filter(!grepl("Minor", Program))

# add BA / BS column
majorUnits <- majorUnits %>%
  mutate(DegreeType = case_when(
    grepl("Bachelor of Arts", Program) ~ "BA",
    grepl("Bachelor of Science", Program) ~ "BS",
    TRUE ~ NA_character_
  ))

# remove "Bachelor of Arts" or "Bachelor of Science" from the title
majorUnits <- majorUnits %>%
  mutate(Program = str_replace_all(Program, ", Bachelor of Arts", "")) %>%
  mutate(Program = str_replace_all(Program, ", Bachelor of Science", ""))

# remove all "Individual" majors or "Integrated" majors
majorUnits <- majorUnits %>%
  filter(!grepl("Individual", Program) & !grepl("Integrated", Program))

# filter matching majors
majorUnits <- majorUnits %>%
  mutate(Match = if_else(Program %in% percent_double_majoring_graphable$major, 1, 0))

# export both dataframes as a csv

# Save majorUnits dataframe to majorUnits.csv
#write.csv(majorUnits, "majorUnits.csv", row.names = FALSE)

# Save percent_double_majoring_graphable dataframe to percent_double_majoring_graphable.csv
#write.csv(percent_double_majoring_graphable, "percent_double_majoring_graphable.csv", row.names = FALSE)

# read in double major percents with units
percent_double_majoring_graphable = read.csv("data/DoubleMajorPercentUnits.csv")

# Function to split the values and extract the second part
split_units <- function(value) {
  parts <- unlist(strsplit(value, ', '))
  if (length(parts) > 1) {
    return(parts[2])
  } else {
    return(NA)
  }
}

# Apply the function to create the new column
percent_double_majoring_graphable$units2 <- sapply(percent_double_majoring_graphable$units, split_units)
percent_double_majoring_graphable$units <- sapply(strsplit(as.character(percent_double_majoring_graphable$units), ","), function(x) trimws(x[1]))

#function to take the average of the ranges

average_numbers <- function(df, column_name) {
  for (i in 1:nrow(df)) {
    value <- df[i, column_name]
    parts <- strsplit(as.character(value), "-")[[1]]
    
    if (length(parts) == 2) {
      average <- mean(as.numeric(parts))
      df[i, column_name] <- average
    }
  }
  return(df)
}

# Apply the function
percent_double_majoring_graphable <- average_numbers(percent_double_majoring_graphable, "units")
percent_double_majoring_graphable <- average_numbers(percent_double_majoring_graphable, "units2")

percent_double_majoring_graphable$units = as.numeric(percent_double_majoring_graphable$units)
percent_double_majoring_graphable$units2 = as.numeric(percent_double_majoring_graphable$units2)

#add a new column, unitsAvg, which takes the AVG of BS and BA units
percent_double_majoring_graphable$unitsAvg <- rowMeans(percent_double_majoring_graphable[, c("units", "units2")], na.rm = TRUE)


#-----------------------------------------------
#GRAPH

#NON-BINNED GRAPH
# Convert percentDoubleMajoring to percentages
percent_double_majoring_graphable$percentDoubleMajoring <- percent_double_majoring_graphable$percentDoubleMajoring * 100

# Sort the dataframe by percentDoubleMajoring in ascending order
percent_double_majoring_graphable <- percent_double_majoring_graphable[order(percent_double_majoring_graphable$percentDoubleMajoring), ]

ggplot(percent_double_majoring_graphable, aes(x = percentDoubleMajoring, y = factor(major, levels = percent_double_majoring_graphable$major))) +
  geom_bar(stat = "identity", aes(fill = percentDoubleMajoring), position = "dodge", width = 0.8) +  # Set position to "dodge"
  labs(title = "Percentage of Students in each Major with a Second Major", x = "Percent of Students with 2 Majors", y = "") +
  scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_fill_gradient(low = "#022851", high = "#FFBF00") +  # Set gradient color from blue to yellow by x-axis value
  theme_ipsum() +
  theme(text = element_text(size = 10)) +  # Set text size to 10
  theme(plot.margin = margin(20, 50, 20, 50, "pt")) +  # Adjust the size of the plot area
  guides(fill = FALSE)  # Remove the fill color guide

ggplot(percent_double_majoring_graphable, aes(x = percentDoubleMajoring, y = factor(major, levels = percent_double_majoring_graphable$major))) +
  geom_bar(stat = "identity", aes(fill = group), position = "dodge", width = 0.8) +  # Set position to "dodge"
  labs(title = "Percentage of Students in each Major with a Second Major", x = "Percent of Students with 2 Majors", y = "") +
  scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), name = "Group") +  # Set custom colors for each group and name the legend
  theme_ipsum() +
  theme(text = element_text(size = 10)) +  # Set text size to 10
  theme(plot.margin = margin(20, 50, 20, 50, "pt")) # Adjust the size of the plot area

ggplot(percent_double_majoring_graphable, aes(x = percentDoubleMajoring, y = factor(major, levels = percent_double_majoring_graphable$major))) +
  geom_bar(stat = "identity", aes(fill = unitsAvg), position = "dodge", width = 0.8) +  # Color bars by the "units" column
  labs(title = "Percentage of Students in each Major with a Second Major, Colored by Units", x = "Percent of Students with 2 Majors", y = "") +
  scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_fill_viridis_c(name = "Average Major Unit Count") +  # Use Viridis color scale for "units" values
  theme_ipsum() +
  theme(text = element_text(size = 10)) +  # Set text size to 10
  theme(plot.margin = margin(20, 50, 20, 50, "pt")) # Adjust the size of the plot area
