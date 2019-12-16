##################
###            ###
###   IGRAPH   ###
###            ###
##################
library(igraoh)

###########################
###   CONSTRUCT GRAPH   ###
###########################
graph1 <- graph_from_adjacency_matrix(mm, mode = 'undirected')
graph1.log <- graph_from_adjacency_matrix(as.matrix(mm.log), mode = 'undirected')
#####

################################
###   GRAPH METRICS   ###
################################
#ac <- alpha_centrality(graph1.log)
#ec <- eigen_centrality(graph1)
#ec.log <- eigen_centrality(graph1.log)
#pc <- power_centrality(graph1.log)
#b <- betweenness(graph1)
#be <- betweenness.estimate(graph1)

#a <- assortativity(graph1) # need "types"
#an <- assortativity_nominal(graph1, directed = FALSE) # need "types"
#ad <- assortativity_degree(graph1, directed = FALSE) # = 0.51, 
# 0.51 is positive, meaning similar vertices tend to connect.. hooray

# c <- closeness(graph1)

#all_shortest_paths(graph1)
#all_simple_paths(graph1)
#####


###################
###   CLUSTER   ###
###################

###   maximal weakly connected components   ###
graph1.comps <- components(graph1)
count_components(graph1) # yields 48

###   EDGE BETWEENNESS   ###
#########   
# DEFINE VARIABLES
#filename <- 'cluster_eb'
#suffix <- '.Rdata'
#path1 <- paste('data/save/cluster', '/', sep = '')
#path2 <- 'ISS Test Project/'
# RUN THE ALGO
#tic()
#cluster.eb <- cluster_edge_betweenness(graph1)
#toc.eb <- toc()
# SAVE IT
# PATH 1
#save(cluster.eb, file = paste(path1,filename,'_',start,'_',end,suffix,sep=''))
#save(cluster.eb, file = paste(path1,filename,suffix,sep=''))
# PATH 2
#save(cluster.eb, file = paste(path2,filename,'_',start,'_',end,suffix,sep=''))
#save(cluster.eb, file = paste(path2,filename,suffix,sep=''))
#rm(filename, suffix, path1, path2)
#####
# LOAD IT
load('data/save/cluster/cluster_eb.Rdata')
###   "OPTIMAL"   ###
#####
##   DEFINE VARIABLES
#filename <- 'cluster_opt'
#suffix <- '.Rdata'
#path1 <- paste('data/save/cluster', '/', sep = '')
#path2 <- 'ISS Test Project/'
## RUN THE ALGO
#tic()
#cluster.opt <- cluster_optimal(graph1)
#toc.opt <- toc()
## SAVE IT
# PATH 1
#save(cluster.opt, file = paste(path1,filename,'_',start,'_',end,suffix,sep=''))
#save(cluster.opt, file = paste(path1,filename,suffix,sep=''))
# PATH 2
#save(cluster.opt, file = paste(path2,filename,'_',start,'_',end,suffix,sep=''))
#save(cluster.opt, file = paste(path2,filename,suffix,sep=''))
#rm(filename, suffix, path1, path2)
#####
cluster.info <- cluster_infomap(graph1)
cluster.label <- cluster_label_prop(graph1) # modularity = 0.47! 
# View(communities(cluster.label)) # 182 communities, 
cluster.eigen <- cluster_leading_eigen(graph1) # modularity(cluster.eigen) = 0.32
# cluster.eigen.groups <- groups(cluster.eigen) # 25 groups with more than 1 member
cluster.walk <- cluster_walktrap(graph1) # modularity = 0.38!
cluster.louvain <- cluster_louvain(graph1) # modularity(cluster.louvain) = 0.53!, 

############################
###   CLUSTERING STATS   ###
############################
# SET UP THE FRAME
columns <- c('n Communities','Modularity', 'n com length 1')
algos <- c('eb','eigen','louvain','walk','info','label') # need to add optimal
results.stats <- as.data.frame(matrix(NA, ncol = length(columns), nrow = length(algos)))
rownames(results.stats) <- algos
colnames(results.stats) <- columns
# FILL IT IN
results.stats[,'n Communities'] <- c(length(communities(cluster.eb)),
                                     length(communities(cluster.eigen)),
                                     length(communities(cluster.louvain)),
                                     length(communities(cluster.walk)),
                                     length(communities(cluster.info)),
                                     length(communities(cluster.label)))
results.stats$Modularity <-  c(modularity(cluster.eb),
                               modularity(cluster.eigen),
                               modularity(cluster.louvain),
                               modularity(cluster.walk),
                               modularity(cluster.info),
                               modularity(cluster.label))
#####


##########################
###   FORMAT RESULTS   ###
##########################
###   CONFIGURE X MATRIX   ###
x <- matrix(NA, nrow = length(rownames(Mm)), ncol = 1)
rownames(x) <- rownames(Mm)
x <- as.data.frame(x)
x$V1 <- rownames(Mm)
###   CREATE Y MATRIX   ###
# DEFINE CLUSTER DFS
df1 <- as.data.frame(cbind(cluster.eb$names, as.integer(cluster.eb$membership)))
df2 <- as.data.frame(cbind(cluster.eigen$names, as.integer(cluster.eigen$membership)))
df3 <- as.data.frame(cbind(cluster.louvain$names, as.integer(cluster.louvain$membership)))
df4 <- as.data.frame(cbind(cluster.walk$names, as.integer(cluster.walk$membership)))
df5 <- as.data.frame(cbind(cluster.info$names, as.integer(cluster.info$membership)))
df6 <- as.data.frame(cbind(cluster.label$names, as.integer(cluster.label$membership)))
df7 <- as.data.frame(cbind(rownames(as.data.frame(graph1.comps$membership)), graph1.comps$membership))
# SAVE THEM
save(df1, df2, df3, df4, df5, df6, file = 'bin/data/dfs.Rdata')
# MERGE THEM
y <- as.data.frame(cbind(as.character(df1$V1),df1$V2,df2$V2,df3$V2,df4$V2,df5$V2,df6$V2,df7$V2))
results <- merge(x, y, by = 'V1')
colnames(results) <- c('names',algos,'components')
rm(algos)

#######################################
###   CHECK CLUSTER DISTRIBUTIONS   ###
#######################################
#plot.df <- 
hist(as.numeric(df1$V2), breaks = length(levels(df1$V2)), main = 'Edge Betweenness', xlab = '', ylab = '')
hist(as.numeric(df2$V2), breaks = length(levels(df2$V2)), main = 'Eigenvalue Centrality', xlab = '', ylab = '')
hist(as.numeric(df3$V2), breaks = length(levels(df3$V2)), main = 'Louvain', xlab = '', ylab = '')
hist(as.numeric(df4$V2), breaks = length(levels(df4$V2)), main = 'Walk', xlab = '', ylab = '')
hist(as.numeric(df5$V2), breaks = length(levels(df5$V2)), main = 'Info', xlab = '', ylab = '')
hist(as.numeric(df6$V2), breaks = length(levels(df6$V2)), main = 'Label', xlab = '', ylab = '') #7, 11, __
#####

######################################
###   MOST POPULATED COMMUNITIES   ###
######################################
###   select the top 8 (or 10?) most populated communities,
###   generate df with lists
## need to unlist the communities() results

###   EDGE BETWEENNESS   ###
comms.eb <- as.numeric(levels(as.factor(cluster.eb$membership)))
lengths.eb <- vector()
for(i in seq_along(comms.eb)){
  lengths.eb[i] <- dim(df1[which(df1[,'V2'] == comms.eb[i]),])[1]
}
comm.eb <- communities(cluster.eb)[as.character(head(sort.list(lengths.eb, decreasing = TRUE), n = 20))]
# AS.MATRIX FOR BETTER VISUAL
comm.eb.m <- as.data.frame(matrix(NA, ncol = length(comm.eb), nrow = length(unlist(comm.eb[1]))))
colnames(comm.eb.m) <- unlist(dimnames(comm.eb))
for(j in seq_len(dim(comm.eb))){
  comm.eb.m[j] <- c(unlist(comm.eb[j]),rep(NA, times = dim(comm.eb.m[j])[1]-length(unlist(comm.eb[j]))))
}
rm(j)

###   EIGENVECTOR CENTRALITY   ###
#####
comms.eigen <- as.numeric(levels(as.factor(cluster.eigen$membership)))
lengths.eigen <-  vector()
for(i in seq_along(comms.eigen)){
  lengths.eigen[i] <- dim(df2[which(df2[,'V2'] == comms.eigen[i]),])[1]
}
comm.eigen <- communities(cluster.eigen)[as.character(head(sort.list(lengths.eigen, decreasing = TRUE), n = 9))]
# AS.MATRIX FOR BETTER VISUAL
comm.eigen.m <- as.data.frame(matrix(NA, ncol = length(comm.eigen), nrow = length(unlist(comm.eigen[1]))))
colnames(comm.eigen.m) <- unlist(dimnames(comm.eigen))
for(j in seq_len(dim(comm.eigen))){
  comm.eigen.m[j] <- c(unlist(comm.eigen[j]),rep(NA, times = dim(comm.eigen.m[j])[1]-length(unlist(comm.eigen[j]))))
}
rm(j)
#####
###   INFO   ###
#####
comms.info <- as.numeric(levels(as.factor(cluster.info$membership)))
lengths.info <- vector()
for(i in seq_along(comms.info)){
  lengths.info[i] <- dim(df5[which(df5[,'V2'] == comms.info[i]),])[1]
}
comm.info <- communities(cluster.info)[as.character(head(sort.list(lengths.info, decreasing = TRUE), n = 30))] # __, 15(? 04 14?), 30
# AS.MATRIX FOR BETTER VISUAL
comm.info.m <- as.data.frame(matrix(NA, ncol = length(comm.info), nrow = length(unlist(comm.info[1]))))
colnames(comm.info.m) <- unlist(dimnames(comm.info))
for(j in seq_len(dim(comm.info))){
  comm.info.m[j] <- c(unlist(comm.info[j]),rep(NA, times = dim(comm.info.m[j])[1]-length(unlist(comm.info[j]))))
}
rm(j)
#####
###   LABEL   ###
#####
comms.label <- as.numeric(levels(as.factor(cluster.label$membership)))
lengths.label <- vector()
for(i in seq_along(comms.label)){
  lengths.label[i] <- dim(df6[which(df6[,'V2'] == comms.label[i]),])[1]
}
comm.label <- communities(cluster.label)[as.character(head(sort.list(lengths.label, decreasing = TRUE), n = 7))] #7, 11, __
# AS.MATRIX FOR BETTER VISUAL
comm.label.m <- as.data.frame(matrix(NA, ncol = length(comm.label), nrow = length(unlist(comm.label[1]))))
colnames(comm.label.m) <- unlist(dimnames(comm.label))
for(j in seq_len(dim(comm.label))){
  comm.label.m[j] <- c(unlist(comm.label[j]),rep(NA, times = dim(comm.label.m[j])[1]-length(unlist(comm.label[j]))))
}
rm(j)
#####
###   LOUVAIN   ###
#####
comms.louvain <- as.numeric(levels(as.factor(cluster.louvain$membership)))
lengths.louvain <- vector()
for(i in seq_along(comms.louvain)){
  lengths.louvain[i] <- dim(df3[which(df3[,'V2'] == comms.louvain[i]),])[1]
}
comm.louvain <- communities(cluster.louvain)[as.character(head(sort.list(lengths.louvain, decreasing = TRUE), n = 14))]
# AS.MATRIX FOR BETTER VISUAL
comm.louvain.m <- as.data.frame(matrix(NA, ncol = length(comm.louvain), nrow = length(unlist(comm.louvain[1]))))
colnames(comm.louvain.m) <- unlist(dimnames(comm.louvain))
for(j in seq_len(dim(comm.louvain))){
  comm.louvain.m[j] <- c(unlist(comm.louvain[j]),rep(NA, times = dim(comm.louvain.m[j])[1]-length(unlist(comm.louvain[j]))))
}
rm(j)
#####
###   WALK   ###
#####
comms.walk <- as.numeric(levels(as.factor(cluster.walk$membership)))
lengths.walk <- vector()
for(i in seq_along(comms.walk)){
  lengths.walk[i] <- dim(df4[which(df4[,'V2'] == comms.walk[i]),])[1]
}
comm.walk <- communities(cluster.walk)[as.character(head(sort.list(lengths.walk, decreasing = TRUE), n = 16))]
# AS.MATRIX FOR BETTER VISUAL
comm.walk.m <- as.data.frame(matrix(NA, ncol = length(comm.walk), nrow = length(unlist(comm.walk[1]))))
colnames(comm.walk.m) <- unlist(dimnames(comm.walk))
for(j in seq_len(dim(comm.walk))){
  comm.walk.m[j] <- c(unlist(comm.walk[j]),rep(NA, times = dim(comm.walk.m[j])[1]-length(unlist(comm.walk[j]))))
}
rm(j)
#####

###   SAVE TO XLSX   ###
write.xlsx2(comm.eb.m, 'bin/clusters.xlsx', row.names = FALSE, sheetName = 'eb')
write.xlsx2(comm.eigen.m, 'bin/clusters.xlsx', append = TRUE, row.names = FALSE, sheetName = 'eigen')
write.xlsx2(comm.info.m, 'bin/clusters.xlsx', append = TRUE, row.names = FALSE, sheetName = 'info')
write.xlsx2(comm.label.m, 'bin/clusters.xlsx', append = TRUE, row.names = FALSE,  sheetName = 'label')
write.xlsx2(comm.louvain.m, 'bin/clusters.xlsx', append = TRUE, row.names = FALSE, sheetName = 'louvain')
write.xlsx2(comm.walk.m, 'bin/clusters.xlsx', append = TRUE, row.names = FALSE, sheetName = 'walk')
save(comm.eb,comm.eigen,comm.info,comm.label,comm.louvain,comm.walk,file='bin/data/comms.Rdata')

###   SAVE TO RDATA   ###
save(comm.eb.m, file = 'bin/data/comm.Rdata')

###   VISUALIZE   ###
barplot(lengths(comm.eb), main = 'Edge Betweenness, top 20')
barplot(lengths(comm.eigen), main = 'Eigenvalue Centrality, top 9')
barplot(lengths(comm.info), main = 'Info, top 30')
barplot(lengths(comm.label), main = 'Label, top 7')
barplot(lengths(comm.louvain), main = 'Louvain, top 14')
barplot(lengths(comm.walk), main = 'Walk, top 16')

###   CLEAN IT UP   ###
rm(comms.eb,comms.eigen,comms.info,comms.label,comms.louvain,comms.walk)
rm(lengths.eb,lengths.eigen,lengths.info,lengths.label,lengths.louvain,lengths.walk)
rm(df1,df2,df3,df4,df5,df6,df7)





