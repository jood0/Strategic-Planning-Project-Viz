#custom function by Leon to reformat names


#dependencies here

#install.packages("humaniformat")
#install.packages("dplyr")

library(humaniformat)
library(dplyr)

#USAGE
#takes a list of names, and then a string "from"
#inputs are "Last, First M"

formatNames = function(names, originalFormat="First Last")
{
  returnNames = names
  
  #convert "Last, First" to "First M Last"
  if (originalFormat == "Last, First")
  {
    # Splitting the Name column
    nameParts <- strsplit(returnNames, ",")
    
    # Reformatting names
    nameParts <- sapply(nameParts, function(x) {
      if (length(x) == 2) {
        # Handle "Last, First" format
        name <- x[2]
        paste(name, x[1], sep = " ")  # Rearrange to "First Last"
      } else if (length(x) == 3) {
        # Handle "Last, First MI" format
        paste(x[2], x[3], x[1], sep = " ")  # Rearrange to "First MI Last"
      } else {
        # Handle other cases based on your data
        NA  # or some default value
      }
    })
    
    returnNames = nameParts
  }
  
  #now, the format should be equal to First M Last
  
  #format using humaniformat
  parsedNames = parse_names(returnNames)
  returnNames = paste(parsedNames$first_name, parsedNames$last_name)
  
  #make names all lower case
  returnNames = toupper(returnNames)
  #return final value
  return(returnNames)
}

#EXAMPLES BELOW

# set working directory
setwd("2024_ls_strategic_plan")

#THIS DATA IS IN THE FORMAT FIRST, LAST
# read in dataframe
gradCenterData <- read.csv("data/GradCentersScrapedRough.csv")
gradCenterData$Name = formatNames(gradCenterData$Name)

#THIS DATA IS IN THE FORMAT LAST,FIRST
# read in dataframe
facultyData <- read.csv("data/faculty_department.csv")
facultyData$Employee.Name = formatNames(facultyData$Employee.Name, "Last, First")
