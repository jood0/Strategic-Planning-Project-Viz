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
#   "writexl"
#   )
# )
#install.packages(c("readxl", "hrbrthemes"))
#install.packages(c("viridis", "hrbrthemes"))

# load libraries
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


# Data Formatting ---------------------------------------------------------

# table format should be:
# major 1 | major 2 | frequency (count)



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

# which IDs have 2 majors? (we need to deal with 3 later)
# id.count<-as.data.frame(table(majors$`Student ID`), stringsAsFactors = FALSE)
#   
# double.major.ids<-id.count$Var1[which(id.count$Freq==2)]

# which IDs have L$S majors?
ls_ids <- majors$`Student ID`[which(majors$College == "L")]

# subset the majors data to only IDs that have L&S majors
majors_ls <- majors[which(majors$`Student ID` %in% ls_ids),]
# remove visitors from other campuses
majors_ls <- majors_ls[which(str_detect(majors_ls$Major, "visitor", negate=TRUE)),]
#remove undeclared majors
majors_ls <- majors_ls[which(str_detect(majors_ls$Major, "Undeclared", negate=TRUE)),]


#how many times are each major paired?
#majors_cross <- crossprod(table(majors_ls[c(1,5)])) #use major_name
majors_cross <- crossprod(table(majors_ls[c(1,2)])) #use major, which includes the college

majors_cross <- as.data.frame(majors_cross)
diag(majors_cross) <- 0


# frequency of major occurrence
major_freq <- majors_cross %>%                  # start with the majors_cross data
  dplyr::mutate(Major = rownames(.),       # make a column called major_name and populate it with the rownames
                occurrences = rowSums(.)) %>%   # make a column called occurrences and populate it with the sum of all the number in that particular row
  dplyr::select(Major, occurrences)        # just keep the columns called major_name and occurrences in the final output
major_freq <- major_freq[which(major_freq$occurrences>0),]


# edges between majors

# this code seems to duplicate edges - a-b AND b-a
major_edges <- majors_cross %>%                 # start with the majors_cross data
  dplyr::mutate(from = rownames(.)) %>%         # make a new column called from and populate it with the row names
  tidyr::gather(to, Frequency, rownames(.)) %>% # change the shape of the dataset, make a column called to and Frequency and loop through the rownames list to make all the possible combinations (we're making the cross product table into long format)
  dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency)) # change the value in the column called Frequency to NA if the value is 0, and if not, leave it alone


# wide <- cbind(row.names(majors_cross), majors_cross)
# names(wide)[1]<-"from"
# long <- melt(setDT(wide), id.vars=c("from"), variable.name="to", value.name="frequency") #why is this the same dimensions as major_edges?  ---> because the matches are duplicated below the diagonals of the crossproduct... see https://stackoverflow.com/questions/71453077/remove-duplicate-combinations-from-cross-join-result-in-r for help?
# long_used <- long[which(long$frequency>0),]


major_edges_used <- major_edges[which(major_edges$Frequency>0),]

major_edges_used<- rm_unordered_pairs(df = major_edges_used, column_1=1, column_2=2)

major_edges_categorized <- major_edges_used

#converts the "to" column from a major, instead simplifies it to its respective college's letter
for (i in seq_along(major_edges_categorized$to)) {
  edge <- major_edges_categorized$to[i]
  
  if (substring(edge, 1, 1) == "L") {
    major_edges_categorized$to[i] <- "L"
  } else if (substring(edge, 1, 1) == "A") {
    major_edges_categorized$to[i] <- "A"
  } else if (substring(edge, 1, 1) == "B") {
    major_edges_categorized$to[i] <- "B"
  } else if (substring(edge, 1, 1) == "E") {
    major_edges_categorized$to[i] <- "E"
  }
}

#factor from
major_edges_categorized$from = factor(major_edges_categorized$from)

# Get the unique factors in the factored column
majorFrom <- levels(major_edges_categorized$from)

#create our new data frame to track each major's college double major associations
majorCollegeRelations = data.frame(majorFrom)

#initialize the other 4 columns as 0 for now
majorCollegeRelations$LettersAndScience <- 0
majorCollegeRelations$Agriculture <- 0
majorCollegeRelations$Biology <- 0
majorCollegeRelations$Engineering <- 0

#create a chart for how much each major is paired with each other college
#go over each major in the leveled factor, then look for all times where the major is present in major_edges_categorized, and add the amount into the proper major based on the Frequency
for (i in seq_along(majorCollegeRelations$majorFrom))
{
  for (j in seq_along(major_edges_categorized$to))
  {
    major_to <- major_edges_categorized$to[j]
    
    if (major_edges_categorized$from[j] == majorCollegeRelations$majorFrom[i])
    {
      if (major_to == "L")
      {
        majorCollegeRelations$LettersAndScience[i] = majorCollegeRelations$LettersAndScience[i] + major_edges_categorized$Frequency[j]
      }
      if (major_to == "A")
      {
        majorCollegeRelations$Agriculture[i] = majorCollegeRelations$Agriculture[i] + major_edges_categorized$Frequency[j]
      }
      if (major_to == "B")
      {
        majorCollegeRelations$Biology[i] = majorCollegeRelations$Biology[i] + major_edges_categorized$Frequency[j]
      }
      if (major_to == "E")
      {
        majorCollegeRelations$Engineering[i] = majorCollegeRelations$Engineering[i] + major_edges_categorized$Frequency[j]
      }
    }
  }
}

#rename "majorFrom" to major
names(majorCollegeRelations)[names(majorCollegeRelations) == "majorFrom"] <- "major"

majorCollegeRelations = majorCollegeRelations[-1, ]

#read the major categories csv in, and use to sort the majors into bins
majorCategories = read.csv("data/Major_Categories5.csv")
majorCategories <- data.frame(majorCategories$group)
colnames(majorCategories) = "group"
majorCategories$group = factor(majorCategories$group)
#get rid of the major variable so we can squash the data frame by the major bin
groupedCollegeRelations <- data.frame(majorCategories$group, 
                                      majorCollegeRelations$LettersAndScience,
                                      majorCollegeRelations$Biology,
                                      majorCollegeRelations$Agriculture,
                                      majorCollegeRelations$Engineering)

#squash the group
groupedCollegeRelations <- groupedCollegeRelations %>%
  group_by(majorCategories.group) %>%
  summarise_all(.funs = sum, na.rm = TRUE) #sums up the people double majoring in each bin

#rename columns to less spaghetti
colnames(groupedCollegeRelations) = c("group", "L&S", "Biology", "AES", "Engineering")
#names(groupedCollegeRelations)[names(groupe)]
#names(groupedCollegeRelations)[names(groupedCollegeRelations) == "majorCollegeRelations.LettersAndScience"] <- "L&S"
#names(groupedCollegeRelations)[names(groupedCollegeRelations) == "majorCollegeRelations.Biology"] <- "Biology"
#names(groupedCollegeRelations)[names(groupedCollegeRelations) == "majorCollegeRelations.Agriculture"] <- "Agriculture"
#names(groupedCollegeRelations)[names(groupedCollegeRelations) == "majorCollegeRelations.Engineering"] <- "Engineering"

#FOR ALTERNATIVE GRAPH - remove the L&S college to only see relations with other colleges
groupedCollegeRelationsNoLettersAndSciences <- groupedCollegeRelations[c(-2)]

#melt them... idk what this does but it WORKS!!!
groupedCollegeRelations_melted <- melt(groupedCollegeRelations, id.vars = "group")
groupedCollegeRelationsNoLettersAndSciences_melted <- melt(groupedCollegeRelationsNoLettersAndSciences, id.vars = "group")

#stacked bar ggplot for NON-L&S colleges
ggplot(groupedCollegeRelationsNoLettersAndSciences_melted, aes(fill=variable, y=group, x=value)) +
  geom_bar(position="fill", stat="identity") +
  labs(title = "Students in Each L&S Major Group Double Majoring in Other Colleges",
  y = "Group",
  x = "Portion of Double Majors",
  fill = "College of Second Major") +
  theme_ipsum() +
  scale_fill_manual(values=c("#022851","#B3C1D1", "#6FCFEB",  "#FFBF00"))


#stacked bar ggplot for ALL COLLEGES (Inluding L&S)
ggplot(groupedCollegeRelations_melted, aes(fill=variable, y=group, x=value)) +
  geom_bar(position="fill", stat="identity") +
  labs(title = "Students in Each L&S Major Group Double Majoring in Each College",
       y = "Group",
       x = "Portion of Double Majors",
       fill = "College of Second Major") +
  theme_ipsum() +
  scale_fill_manual(values=c("#FFBF00", "#022851","#B3C1D1", "#6FCFEB"))

library("writexl")
write_xlsx(groupedCollegeRelations, "majorCollegeRelations.xlsx")


#IGNORE THIS SCRATCH STUFF... KEEPING IN CASE OF FUTURE REFERENCE
#----------------------------------------------------------------
# Melt the data frame to long format
#majorCollegeRelations_melted <- melt(majorCollegeRelations, id.vars = "major")

#ggplot(majorCollegeRelations_melted, aes(x = major, y = value, fill = variable)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(title = "Student Counts by Major and College",
#       x = "Major Names",
#       y = "Number of Students") +
#  theme_minimal()

# Stacked + percent
#ggplot(majorCollegeRelations_melted, aes(fill=variable, y=value, x=major)) + 
#  geom_bar(position="fill", stat="identity") +
#  labs(title = "Student Counts by Major and College",
#      x = "Major Names",
#      y = "Number of Students") +
#  theme_ipsum()
