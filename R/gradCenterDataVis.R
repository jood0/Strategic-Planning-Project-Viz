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

# set working directory
setwd("2024_ls_strategic_plan")

# read in .csv file
gradCenterData <- read.csv("data/GradCentersScraped.csv")

# filter to only L&S faculty
gradCenterDataLSOnly <- gradCenterData[gradCenterData$College == "L&S",]

# remove extra columns, leave only center and department
gradCenterDataLSOnlyTrimmed <- data.frame(gradCenterDataLSOnly$Center, gradCenterDataLSOnly$Department)
colnames(gradCenterDataLSOnlyTrimmed) = c("Center", "Department")

# turn people into connections (people with 2 depts become 2 separate connections)
gradCenterDataIndividualConnection <- gradCenterDataLSOnlyTrimmed %>%
  separate_rows(Department, sep = ",\\s*")

# turn both columns into factors
gradCenterDataIndividualConnection$Center = factor(gradCenterDataIndividualConnection$Center)
gradCenterDataIndividualConnection$Department = factor(gradCenterDataIndividualConnection$Department)

# name columns to make vectors
colnames(gradCenterDataIndividualConnection) = c("From", "To")

#summarize dataframe
gradCenterDataEdges <- gradCenterDataIndividualConnection %>%
  group_by(From, To) %>%
  summarize(Count = n())

#get vertices
gradCenterDataVerticesFrom <- gradCenterDataIndividualConnection %>%
  group_by(From) %>%
  summarise(OccurrenceCount = n())

gradCenterDataVerticesTo <- gradCenterDataIndividualConnection %>%
  group_by(To) %>%
  summarise(OccurrenceCount = n())

#rename them to have the same number of columns
colnames(gradCenterDataVerticesFrom) = c("Name", "Count")
colnames(gradCenterDataVerticesTo) = c("Name", "Count")

#add extra "isCenter" column
gradCenterDataVerticesFrom <- gradCenterDataVerticesFrom %>%
  mutate(isCenter = TRUE)

gradCenterDataVerticesTo <- gradCenterDataVerticesTo %>%
  mutate(isCenter = FALSE)

#merge vertices
gradCenterDataVertices <- merge(gradCenterDataVerticesFrom, gradCenterDataVerticesTo, all = TRUE)

# Step 4: Graph Network
#-----------------------------------------------------------------------------

g <- igraph::graph_from_data_frame(d=gradCenterDataEdges, vertices=gradCenterDataVertices, directed = FALSE)

# Get the vertices data
vertices_data <- as.data.frame(V(g)$isCenter)

# Get 'isCenter' attribute values
is_center <- V(g)$isCenter

# Create a layout matrix to position nodes in two columns
layout <- matrix(0, nrow = vcount(g), ncol = 2)
layout[is_center, 1] <- 1  # Position 'isCenter' nodes in the first column
layout[!is_center, 1] <- 2  # Position non-'isCenter' nodes in the second column

# Create a new vector containing values 0 to 13
nodePosY1 <- (0:12) * 2

# Create another vector containing values 0 to 15
nodePosY2 <- (0:22) * 2 * (13/23)

# Concatenate the two vectors together
nodePosY <- c(nodePosY1, nodePosY2)

layout[, 2] <- nodePosY

# Get degree centrality for nodes
node_degree <- degree(g)

# Scale node size based on degree centrality
vertex_size <- node_degree * 50  # Adjust scaling factor as needed

# Identify unique center nodes
unique_centers <- unique(gradCenterDataVertices$Name[gradCenterDataVertices$isCenter])

# Generate unique colors for each center node
center_colors <- rainbow(length(unique_centers))  # Generate a set of colors

# Create a color mapping for center nodes
center_color_mapping <- setNames(center_colors, unique_centers)

# Create a vector of colors based on 'isCenter' attribute
node_colors <- ifelse(gradCenterDataVertices$isCenter, 
                      center_color_mapping[gradCenterDataVertices$Name], 
                      "#d3d3d3")  # Default color for non-center nodes

# put colors back into the vertices dataframe
gradCenterDataVertices$Color = node_colors

# Get the 'from' vertex indices for each edge
edge_from <- ends(g, E(g))[, 1]  # Get the 'from' vertex index for each edge
  
edge_color_matching_indices <- match(edge_from, gradCenterDataVertices$Name)

edge_colors <- gradCenterDataVertices$Color[edge_color_matching_indices]

# Plot the graph with custom layout and scaled attributes
plot(g, layout = layout, 
     vertex.color = node_colors,
     vertex.size = ifelse(gradCenterDataVertices$isCenter == TRUE, 15, 8),  # Scale node size based on degree
     vertex.label = V(g)$name,
     vertex.frame.width = 0,
     vertex.label.color = "black",
     edge.color = edge_colors)


g <- igraph::graph_from_data_frame(d=gradCenterDataEdges, vertices=gradCenterDataVertices, directed = FALSE)

#g <- graph_from_data_frame(rel, directed=F, vertices=eb_majors)
#g <- graph_from_data_frame(major_edges[which(major_edges$Frequency>0),1:2], directed=F, vertices=eb_majors)
clr <- as.factor(V(g)$isCenter)
#levels(clr) <- c("salmon", "wheat", "lightskyblue", "plum")
levels(clr) <- c("#6FCFEB",  "#FFBF00")
V(g)$color <- as.character(clr)
V(g)$size<-degree(g)*20

#E(g)$size<-degree(g)*20 #edge bundle can't handle changing the edge size yet

# edgebundle(x, tension = 0.5, cutoff = 0.1, width = NULL, fontsize = 14,padding = 100, nodesize = c(5, 20), directed = FALSE)
edgebundle(g, fontsize = 10,padding = 200)

col_g <- tidygraph::as_tbl_graph(g) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)

v.size <- V(col_g)$occurrences

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