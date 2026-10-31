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
#   "ggstance",
#   "tidyr"
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
library("tidyr")

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
#split.majors<-as.data.frame(
#  matrix(
#    data = unlist(strsplit(majors$Major, " - ")),
#    ncol = 2,
#    byrow = TRUE
#  ),
#)
#names(split.majors)<-c("major_code", "major_name")

#majors<-cbind(majors, split.majors)

#remove extra cols
majors_simplified <- data.frame(majors$Major, majors$College)
colnames(majors_simplified) = c("major", "college")
majors_simplified <- majors_simplified %>%
  filter(grepl("L", college))

# remove visitors from other campuses
majors_simplified <- majors_simplified[which(str_detect(majors_simplified$major, "visitor", negate=TRUE)),]
#remove undeclared majors
majors_simplified <- majors_simplified[which(str_detect(majors_simplified$major, "Undeclared", negate=TRUE)),]

#collapse
majors_collapsed <- majors_simplified %>%
  group_by(major) %>%
  summarize(         # Keep the first code (or you can modify this as needed)
    count = n()                 # Add a new column with the count of matching major names
  )

#read in the major categories
major_categories = read.csv("data/Major_Categories5.csv")

#remove dead majors that arent in the major categories
majors_collapsed <- majors_collapsed[majors_collapsed$major %in% major_categories$major, ]

# Function to split the Major column into code and major name
split_major <- function(major) {
  # Extract the code and major name using regex
  code <- str_extract(major, "^[A-Z]{4}")
  major_name <- str_extract(major, "(?<= - ).*")
  
  # Return a data frame with both columns
  data.frame(code = code, major_name = major_name)
}

# Apply the function to split the Major column
majors_collapsed <- majors_collapsed %>%
  rowwise() %>%
  do(data.frame(split_major(.$major), count = .$count)) %>%
  ungroup() %>%
  select(code, major = major_name, count)

#remove the code for the major
#majors_collapsed <- majors_collapsed %>%
#  mutate(major = str_replace(major, "^[^-]* - ", ""))

majors_final = majors_collapsed

total_majors <- sum(majors_final$count)
majors_final$count <- majors_final$count / total_majors

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#get minors

# cleaning and selecting data
minors = read.csv("./data/Campus Minors.xlsx - 22-23 Data.csv")

minors <- minors[ -c(4:8) ]

minors <- minors[1:(nrow(minors) - 2), ]

minors <- subset(minors, !(X22.23.Total == 0)) #remove minors with 0 count both years

# Function to transform the data
transform_minor_name <- function(minor) {
  # Extract the major name and code using regex
  major <- str_extract(minor, "^[^(]+")
  code <- str_extract(minor, "[A-Z]{4}")
  
  # Combine the code and major name in the desired format
  transformed <- paste0(code, " - ", major)
  
  # Trim any extra whitespace
  str_trim(transformed)
}

minors <- minors %>%
  mutate(Minor = sapply(Minor, transform_minor_name))

#trim to only L&S data
minors <- minors %>%
  filter(grepl("L", College))

#reorder and simplify
minors_simplified <- data.frame(minors$Code, unname(minors$Minor), minors$X22.23.Total)
colnames(minors_simplified) = c("code", "minor", "count")

minors_simplified <- minors_simplified %>%
  mutate(minor = str_replace(minor, "^[^-]* - ", ""))

minors_final = minors_simplified

# Calculate total counts in majors and minors
total_minors <- sum(minors_final$count)
minors_final$count <- minors_final$count / total_minors

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#COMBINE MAJORS AND MINORS

# Merging the dataframes on the 'code' column, keeping only matching rows
majors_and_minors <- inner_join(majors_final, minors_final, by = "code")


# Selecting and renaming the columns as needed
majors_and_minors <- majors_and_minors %>%
  select(major, code, major_count = count.x, minor_count = count.y)

#rename colunmns
colnames(majors_and_minors) = c("field", "code", "major_proportion", "minor_proportion")

#MAKE COUNTS INTO PROPORTIONS

majors_and_minors_proportions <- data.frame(majors_and_minors$field, majors_and_minors$major_proportion, majors_and_minors$minor_proportion)
colnames(majors_and_minors_proportions) = "field"

# Calculate total counts in majors and minors
#total_majors <- sum(majors_and_minors$major_count)
#total_minors <- sum(majors_and_minors$minor_count)

# Calculate proportions
#majors_and_minors_proportions$major_proportion <- majors_and_minors$major_count / total_majors
#majors_and_minors_proportions$minor_proportion <- majors_and_minors$minor_count / total_minors

# rename colnames to fix order
colnames(majors_and_minors_proportions) <- c("field", "mjr_proportion", "minor_proportion")
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

#READ IN UNITS DATA

# read in double major percents with units
double_major_units = read.csv("data/DoubleMajorPercentUnits.csv")

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
double_major_units$units2 <- sapply(double_major_units$units, split_units)
double_major_units$units <- sapply(strsplit(as.character(double_major_units$units), ","), function(x) trimws(x[1]))

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
double_major_units <- average_numbers(double_major_units, "units")
double_major_units <- average_numbers(double_major_units, "units2")

double_major_units$units = as.numeric(double_major_units$units)
double_major_units$units2 = as.numeric(double_major_units$units2)

#add a new column, unitsAvg, which takes the AVG of BS and BA units
double_major_units$unitsAvg <- rowMeans(double_major_units[, c("units", "units2")], na.rm = TRUE)


#remove unnecisary data
major_units = data.frame(double_major_units$major, double_major_units$unitsAvg)
colnames(major_units) = c("field", "units")

majors_and_minors_with_units <- left_join(majors_and_minors_proportions, major_units, by = "field")
colnames(majors_and_minors_with_units) = c("field", "2. Major Proportion", "1. Minor Proportion", "units")
#--------------------------------------------------------------------------------------------


#GRAPH MAJORS AND MINORS
# Sort majors_and_minors dataframe by major_count

#majors_and_minors_proportions <- majors_and_minors_proportions %>%
#  arrange(desc(major_proportion))

#due to restrictions, names are ugly, will fix for photoshop

data_long <- majors_and_minors_with_units %>%
  pivot_longer(cols = c("2. Major Proportion", "1. Minor Proportion"),
               names_to = "variable",
               values_to = "proportion")

ggplot(data_long, aes(x = proportion, y = reorder(field, proportion), fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(x = "Proportion", y = "Field", title = "Proportion L&S Majors and Minors in Each Field") +
  scale_fill_manual(values = c("2. Major Proportion" = "#6A0DAD", "1. Minor Proportion" = "#7FFF00")) +
  theme_minimal()



#ggplot(data_long, aes(x = proportion, y = reorder(field, proportion), fill = units)) +
#  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
#  labs(x = "Proportion", y = "Field", title = "Proportion L&S Majors and Minors in Each Field") +
#  scale_fill_viridis_c(name = "Unit Count", option = "C") +  # Use viridis color scale for unit counts
#  theme_minimal() +
#  theme(text = element_text(size = 10)) +  # Set text size to 10
#  facet_wrap(~ variable, scales = "free_x")  # Separate panels for each variable


#ggplot(data_long, aes(x = proportion, y = reorder(field, proportion), fill = variable)) +
#  geom_bar(stat = "identity", position = "dodge", color = "black") +
#  labs(x = "Proportion", y = "Field", title = "Proportion of L&S Majors / Minors") +
#  scale_fill_manual(values = c("Major" = "#8A2BE2", "Minor" = "#9ACD32")) +
#  theme_minimal()

#load in major / minor units
#majorUnits = read.csv("data/programs_units.csv")
