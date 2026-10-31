# GOAL: Create visualizations of the College of Letters & Sciences' undergraduate major data

# REFRENCES:
# https://ladal.edu.au/net.html#Network_Visualization


# Set Up ------------------------------------------------------------------

# install packages

# install.packages(c(
#   "igraph",
#   "ggraph",
#   "tidygraph",
#   "readxl",
#   "stringr",
#   "data.table",
#   "edgebundleR"
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

# set working directory
setwd("data")

# read the data
majors<-read_excel(
  path = "Double Majors all colleges.xlsx",
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

# Visualization -----------------------------------------------------------

# make the graph object
ig <- igraph::graph_from_data_frame(d=major_edges_used, vertices=major_freq, directed = FALSE)

# add labels to the nodes
tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)


# plot the network
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10") +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)




ggraph(tg, layout = 'stress') +
  geom_edge_link() +
  geom_node_point()

# edgebundleR Visualization -----------------------------------------------

# REFERENCE: https://gist.github.com/timelyportfolio/3a052a4bcc85275ce469
# REFERENCE: https://garthtarr.github.io/edgebundleR/articles/edgebundleR.html
# id = student id
# type = 
# set = College
# loc = major_name



eb_vertices <- merge(major_freq, majors_ls, by="Major")
eb_vertices <- eb_vertices[,-3]
eb_vertices <- eb_vertices[!duplicated(eb_vertices),]

g_edges <- major_edges[which(major_edges$Frequency>0),1:3]

g <- igraph::graph_from_data_frame(d=g_edges, vertices=eb_vertices, directed = FALSE)


#g <- graph_from_data_frame(rel, directed=F, vertices=eb_majors)
#g <- graph_from_data_frame(major_edges[which(major_edges$Frequency>0),1:2], directed=F, vertices=eb_majors)
clr <- as.factor(V(g)$College)
#levels(clr) <- c("salmon", "wheat", "lightskyblue", "plum")
levels(clr) <- c("#022851","#B3C1D1", "#6FCFEB",  "#FFBF00")
V(g)$color <- as.character(clr)

V(g)$size<-degree(g)*20

#E(g)$size<-degree(g)*20 #edge bundle can't handle changing the edge size yet

# edgebundle(x, tension = 0.5, cutoff = 0.1, width = NULL, fontsize = 14,padding = 100, nodesize = c(5, 20), directed = FALSE)
edgebundle(g, fontsize = 10,padding = 200)




col_g <- tidygraph::as_tbl_graph(g) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)

v.size <- V(col_g)$occurrences

E(tg)$weight <- E(tg)$Frequency

# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
col_g %>%
  ggraph(layout = "fr") +
  geom_node_point(size=log(v.size)*1, 
                  aes(color=College)) +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = E(tg)$weight,
                    alpha = E(tg)$weight)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)
