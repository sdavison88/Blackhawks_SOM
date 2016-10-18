#Scraping Blackhawks data
####################################################################

install.packages("rvest")
install.packages("hml2")
install.packages("ggplot2")
library(hml2)
library(rvest)
library(ggplot2)

webpage<-html("http://blackhawks.nhl.com/club/gamelog.htm")

blackhawks <- webpage %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()


colnames(blackhawks) <- blackhawks[2,]
blackhawks <- blackhawks[-c(1,2),]

########################################################3

install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)

# Color palette
pretty_palette <- c('#000000', '#EFA900', '#008254', '#253A87', '#FC6500', '#F70017')

data_raw_hawks <- read.csv("C:/Users/monika.klimek/Desktop/Projects/SOM/blackhawks.csv")  
data_raw_hawks <- blackhawks

data_train_hawks <- data_raw_hawks[, c(4,8,9,10,11,12,13,14,15,16,17)]
data_train_hawks$win[data_train_hawks$Dec=="W"]<-3
data_train_hawks$win[data_train_hawks$Dec=="O"]<-2
data_train_hawks$win[data_train_hawks$Dec=="L"]<-1
data_train_hawks <- data_train_hawks[, c(2, 3,4,5,6,7,8,9,10,11,12)]

#  Kohonen method
data_train_matrix_hawks <- data.matrix(data_train_hawks)
data_train_matrix_hawks <- as.matrix(scale(data_train_hawks))
names(data_train_matrix_hawks) <- names(data_train_hawks)
require(kohonen)

som_grid <- somgrid(xdim = 9, ydim=9, topo="hexagonal")
som_model <- som(data_train_matrix_hawks, 
                             rlen=100, 
                             alpha=c(0.05,0.01),
                             grid=som_grid,
                             n.hood = "circular",
                             keep.data = TRUE)

rm(som_grid, data_train_matrix)


source("C:/Users/monika.klimek/Desktop/Projects/SOM/coolBlueHotRed.R")

plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")


# Plot the heatmap for a variable
var <- 11
plot(som_model, type = "property", property = som_model$codes[,var], main=names(som_model$data)[var], palette.name=coolBlueHotRed)

# Plot the original scale heatmap for a variable from the training set:
var <- 2 #define the variable to plot
var_unscaled <- aggregate(as.numeric(data_train_hawks[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train_hawks)[var], palette.name=coolBlueHotRed)
rm(var_unscaled, var)

source("C:/Users/monika.klimek/Desktop/Projects/SOM/plotHeatMap.R")
plotHeatMap(som_model, data, variable=0)

#Clustering SOM results 

mydata <- som_model$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
colnames(som_model$codes)

# map with different colors for every cluster  	
colnames(som_model$data)[11] <- "Win"
colnames(som_model$data)
colnames(som_model$codes)[11] <- "Wins"


plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster])
add.cluster.boundaries(som_model, som_cluster)

#plot with the codes instead of just colors
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster])
add.cluster.boundaries(som_model, som_cluster)

cluster_details_hawks <- data.frame(Date=blackhawks$Date, cluster=som_cluster[som_model$unit.classif])
table(cluster_details_hawks$cluster) # Check the distribution of cluster members.


# Adding a population comparison cluster for density charts.
data_train_hawks$Date <- cluster_details_hawks$Date
cluster_Dates <- merge(data_train_hawks, cluster_details_hawks, by = "Date")
population_cluster <- cluster_Dates
population_cluster$cluster <- 0
cluster_Dates_all <- rbind(population_cluster,cluster_Dates)
cluster_Dates_all <- data.matrix(cluster_Dates_all)
cluster_Dates_all <- data.frame(cluster_Dates_all)


# -------------------- Create multiplot function to combine ggplots ------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Preparing Density Charts for Cluster Summary
library(ggplot2)

gg_cluster <- function(tgt_clu, tgt_col_num) {  # tgt_clu is a integer, tgt_var is a var name.
  g <- ggplot(subset(cluster_Dates_all, cluster == 0 | cluster == tgt_clu), aes_string(x = colnames(cluster_Dates_all)[tgt_col_num]))
  g1 <- g + geom_density(aes(fill = factor(cluster) , alpha = 0.6))
  g2 <- g1 + theme(legend.position = "right")
  g3 <- g2 + guides(fill=guide_legend(title="Cluster"))
  return(g3)
}
cluster_sum <- function(tgt_clu) {
  g1 <- gg_cluster(tgt_clu, 2)
  g2 <- gg_cluster(tgt_clu, 3)
  g3 <- gg_cluster(tgt_clu, 4)
  g4 <- gg_cluster(tgt_clu, 5)
  g5 <- gg_cluster(tgt_clu, 6)
  g6 <- gg_cluster(tgt_clu, 7)
  g7 <- gg_cluster(tgt_clu, 8)
  g8 <- gg_cluster(tgt_clu, 9)
  g9 <- gg_cluster(tgt_clu, 10)
  g10 <- gg_cluster(tgt_clu, 11)
  g11 <- gg_cluster(tgt_clu, 12)
  multiplot(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, cols=2)
}

attributes_comp <- function(tgt_col_num) {
  g1 <- gg_cluster(1, tgt_col_num)
  g2 <- gg_cluster(2, tgt_col_num)
  g3 <- gg_cluster(3, tgt_col_num)
  g4 <- gg_cluster(4, tgt_col_num)
  g5 <- gg_cluster(5, tgt_col_num)
  g6 <- gg_cluster(6, tgt_col_num)
  multiplot(g1, g2, g3, g4, g5, g6, cols=2)
}
