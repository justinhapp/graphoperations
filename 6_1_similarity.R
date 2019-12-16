###   SIMILARITY MATRIX   ###
df <- df[which(as.character(df$Description) != ''),]
items <- levels(as.factor(df$Description))
#items[1] <- 'none'

transactions <- levels(as.factor(df$Transaction))
#transactions <- order(unique(as.numeric(df$Transaction)))

M <- matrix(0, nrow = length(items), ncol = length(items))
rownames(M) <- as.character(items)
colnames(M) <- as.character(items)

###   BUILD THAT LOOP   ###
tic()
for(t in seq_along(as.integer(transactions))){
  temp <- df[which(df$Transaction == transactions[t]),]
  if(dim(na.omit(temp))[1] <= 1)next
  else if(dim(na.omit(temp))[1] == 2){
    temp <- na.omit(temp)
    pair <- as.matrix(as.character(temp$Description))
    #pair <- na.omit(pair)
    for(i in 1:dim(pair)[2]){
      M[(as.character(pair[1,i])),(as.character(pair[2,i]))] <- (M[(pair[1,i]),(pair[2,i])]) + 1
      M[(as.character(pair[2,i])),(as.character(pair[1,i]))] <- (M[(pair[2,i]),(pair[1,i])]) + 1
    }
  } 
  #pairs <- unique(as.data.frame(t(combn(tranny$Description, m=2))))
  else if(dim(na.omit(temp))[1] >= 3){
    temp <- na.omit(temp)
    pair <- as.matrix(combn(as.character(temp$Description), m=2))
    #pair <- na.omit(pair)
    for(i in 1:dim(pair)[2]){
      M[(as.character(pair[1,i])),(as.character(pair[2,i]))] <- (M[(pair[1,i]),(pair[2,i])]) + 1
      M[(as.character(pair[2,i])),(as.character(pair[1,i]))] <- (M[(pair[2,i]),(pair[1,i])]) + 1
    }
  }
  ###   THREE WAYS TO NORMALIZE
  ## LEAVE IT
  #pairs <- pairs
  ## Unique
  #pairs <- unique(pairs)
  ## ln
  # ln()
  ### Add similarity
  #for(i in 1:dim(pair)[1]){
  # M[(as.character(pair[1,i])),(as.character(pair[2,i]))] <- (M[(pair[1,i]),(pair[2,i])]) + 1
  #M[(as.character(pair[2,i])),(as.character(pair[1,i]))] <- (M[(pair[2,i]),(pair[1,i])]) + 1
  #}
}
toc() # takes an hour :/
###   apply fill in

#fillin <- function(list){
  temp <- df[which(df$Transaction == list),]
  if(dim(na.omit(temp))[1] <= 1)next
  else if(dim(na.omit(temp))[1] == 2){
    temp <- na.omit(temp)
    pair <- as.matrix(as.character(temp$Description))
    #pair <- na.omit(pair)
    for(i in 1:dim(pair)[2]){
      M[(as.character(pair[1,i])),(as.character(pair[2,i]))] <- (M[(pair[1,i]),(pair[2,i])]) + 1
      M[(as.character(pair[2,i])),(as.character(pair[1,i]))] <- (M[(pair[2,i]),(pair[1,i])]) + 1
    }
  } 
  #pairs <- unique(as.data.frame(t(combn(tranny$Description, m=2))))
  else if(dim(na.omit(temp))[1] >= 3){
    temp <- na.omit(temp)
    pair <- as.matrix(combn(as.character(temp$Description), m=2))
    #pair <- na.omit(pair)
    for(i in 1:dim(pair)[2]){
      M[(as.character(pair[1,i])),(as.character(pair[2,i]))] <- (M[(pair[1,i]),(pair[2,i])]) + 1
      M[(as.character(pair[2,i])),(as.character(pair[1,i]))] <- (M[(pair[2,i]),(pair[1,i])]) + 1
    }
  }
}
#lapply(transactions,fillin)

M <- as.data.frame(M)
rm(i,t,temp,pair)

#######################
###   EDIT MATRIX   ###
#######################
### Make sparse graph minimal (but still sparse) graph
Mm <- M[, colSums(M) != 0] 
Mm <- Mm[rowSums(M) != 0,]
###   REMOVE PLASTIC BAG
Mm <- Mm[,!(colnames(Mm) %in% 'PLASTIC BAG')]
Mm <- Mm[!(row.names(Mm) %in% 'PLASTIC BAG'),]
###   REMOVE SHIPPING FEE
#Mm <- Mm[,!(colnames(Mm) %in% 'SHIPPPING FEE')]
#Mm <- Mm[!(row.names(Mm) %in% 'SHIPPING FEE'),]


#####################
###   SAVE DATA   ###
#####################
###   DEFINE VARIABLES   ###
filename <- 'similarity'
suffix <- '.Rdata'
path1 <- paste('data/save/', filename, '/', sep = '')
path2 <- 'Test/'
path3 <- 'bin/'
###   PATH 1   ###
## FILE 1
save(M, file = paste(path1,filename,'_',start,'_',end,suffix,sep=''))
save(M, file = paste(path1,filename,suffix,sep=''))
## FILE 2
save(Mm, file = paste(path1,filename,'_',start,'_',end,suffix,sep=''))
save(Mm, file = paste(path1,filename,suffix,sep=''))
###   PATH 2   ###
## FILE 1
save(M, file = paste(path2,filename,'_',start,'_',end,suffix,sep=''))
save(M, file = paste(path2,filename,suffix,sep=''))
## FILE 2
save(Mm, file = paste(path2,filename,'_',start,'_',end,suffix,sep=''))
save(Mm, file = paste(path2,filename,suffix,sep=''))
###   PATH 3   ###
## FILE 1
save(M, file = paste(path3,filename,'_',start,'_',end,suffix,sep=''))
save(M, file = paste(path3,filename,suffix,sep=''))
## FILE 2
save(Mm, mm, file = paste(path3,filename,'_',start,'_',end,suffix,sep=''))
save(Mm, mm, file = paste(path3,filename,suffix,sep=''))
## FILE 3
save(mm, file = paste(path3,filename,'_',start,'_',end,suffix,sep=''))
save(mm, file = paste(path3,filename,suffix,sep=''))
###   CLEAN-UP   ###
rm(filename,suffix,path1,path2,path3)

