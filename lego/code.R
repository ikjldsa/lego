install.packages(c("visNetwork", "geomnet", "igraph"))

library(h2o)
library(tidyverse)
library(visNetwork)
library(geomnet)
library(igraph)


path <- 'C:/Users/ben/lego/'
df <- read.csv(paste0(path,'output.csv'))

## Create crossprod from dummy variabkes
dim(df)
x1 <- as.matrix(df[,which(grepl("primary_tag_", names(df)))])
x2 <- as.matrix(df[,which(grepl("secondary_tag_", names(df)))])
x1_cp <-crossprod(x1) 
x2_cp <-crossprod(x2)

## Create mapping table from cross product
table1 <- data.frame()
r <- 0
for (i in 1:(nrow(x1_cp)-1)){
  for (j in (i+1):ncol(x1_cp)){
    if(x1_cp[i,j] > 0) {
      r = r + 1
      table1[r,"from"]  <- rownames(x1_cp)[i]
      table1[r,"to"]    <- colnames(x1_cp)[j]
      table1[r,"count"] <- x1_cp[i,j]
      # print(r)
    }
  }
}

## Create mapping table from dummy variables
table2 <- data.frame()
r <- 0
for (i in 1:(nrow(x2_cp)-1)){
  for (j in (i+1):ncol(x2_cp)){
    if(x2_cp[i,j] > 3) {
      r = r + 1
      table2[r,"from"]  <- rownames(x2_cp)[i]
      table2[r,"to"]    <- colnames(x2_cp)[j]
      table2[r,"count"] <- x2_cp[i,j]
      # print(r)
    }
  }
}


table1$from <- gsub("primary_tag_", "", table1$from)
table1$to <- gsub("primary_tag_", "", table1$to)

##Nodes
nodes <- data.frame(id = unique(table1$from), 
                    label = unique(table1$from))

##Edges
edges <- as.data.frame(table1)
colnames(edges) <- c("from", "to", "width")

##Create graph for Louvain
graph <- graph_from_data_frame(edges, directed = FALSE)

##Louvain Comunity Detection
cluster <- cluster_louvain(graph)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

nodes <- left_join(nodes, cluster_df, by = "label")
colnames(nodes)[3] <- "group"

visNetwork(nodes, edges)



###Table2
table2$from <- gsub("primary_tag_", "", table2$from)
table2$to <- gsub("primary_tag_", "", table2$to)

##Nodes
nodes <- data.frame(id = unique(table2$from), 
                    label = unique(table2$from))

##Edges
edges <- as.data.frame(table2)
colnames(edges) <- c("from", "to", "width")

##Create graph for Louvain
graph <- graph_from_data_frame(edges, directed = FALSE)

##Louvain Comunity Detection
cluster <- cluster_louvain(graph)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

nodes <- left_join(nodes, cluster_df, by = "label")
colnames(nodes)[3] <- "group"

visNetwork(nodes, edges)