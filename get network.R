#rm(list=ls())
library(dplyr)
library(tidyr)
library(DescTools)
library(nnet)

#library(dplyr) #basic data management & %>%
#library(MASS) #for negative bin regression
#library(stargazer) #for combining output
#library(ggplot2) #for graphing
#library(pscl) #for zero inflated models & predprob
#library(reshape2) #convert wide to tall
#library(summarytools) #freq

#additions: see which first author is boss
#binary variable for important papers
#another x variable for every author combination denoting total number of papers coauthored by them #this is the fckin dep var!!!

################# NETWORK PLOT ##################

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), 
             unique(data$A4), unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
data_sub <- select(data, MA, A1, A2, A3, A4, A5, A6, A7)

x <- c(1:length(authors))
n <- length(x)
m <- 2

perm <- CombSet(x, m, repl=FALSE, ord=FALSE) #gets unique combination sets

estimate=matrix(NA,nrow(perm),1)

for(i in 1:nrow(perm)){
  
  aut1 <- perm[i, 1] #gets the first element of first set
  aut2 <- perm[i, 2] #gets the second element of first set
  a <- which(data_sub == authors[aut1], arr.ind = TRUE) #list of paper authored by aut1
  b <- which(data_sub == authors[aut2], arr.ind = TRUE) #list of paper authored by aut2
  papers_tog <- length(intersect(a[,1], b[,1])) #number of unique papers coauthored by aut1 and aut2
  estimate[i]=papers_tog
  
}

perm <- as.data.frame(perm)
perm$papers_tog <- estimate
perm$papers_tog <- ifelse(perm$papers_tog > 0, 1, 0)
perm <- subset(perm, papers_tog==1)

a <- table(lapply(perm[-3], factor, levels = sort(unique(unlist(perm[-3])))))
a[lower.tri(a)] <- t(a)[lower.tri(a)]
a <- matrix(a, ncol = ncol(a), dimnames = dimnames(a))

network <- igraph::graph_from_adjacency_matrix(a, mode='undirected', diag=F)
#plot(network, layout=igraph::layout.sphere, main="sphere")
#plot(network, layout=igraph::layout.random, main="random", vertex.size=10)
plot(network, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=7.5) 
#title("Original network")

dist <- distances(network)
dist <- colMeans(dist)
hist(dist)
mean(dist)

transitivity(network, type="local")
transitivity(network, type="localaverage")

graph.density(network,loop=FALSE)

################# AFTER ENTRY ##################

list_est <- list()

start <- length(authors)+1
end <- start+round(mean(new_added))

for (i in 1:length(authors)){
  
  num_to_add=sample(c(1:end), replace=FALSE, size=aut_entry$X4[i]) #num_to_add=sample(c(start:end), replace=FALSE, size=aut_entry$X4[i])
  dataframe <- data.frame(V1=c(rep(i,times=aut_entry$X4[i])), V2=num_to_add)
  list_est[[i]] <- dataframe
  
}

list_est <- do.call(rbind.data.frame, list_est)
list_est$papers_tog <- 1

perm2 <- rbind(perm, list_est)

#perm2 <- list_est

a2 <- table(lapply(perm2[-3], factor, levels = sort(unique(unlist(perm2[-3])))))
a2[lower.tri(a2)] <- t(a2)[lower.tri(a2)]
a2 <- matrix(a2, ncol = ncol(a2), dimnames = dimnames(a2))

network2 <- igraph::graph_from_adjacency_matrix(a2, mode='undirected', diag=F)
#plot(network2, layout=igraph::layout.sphere, main="sphere")
#plot(network2, layout=igraph::layout.random, main="random", vertex.size=10)
plot(network2, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=7.5)

dist2 <- distances(network2)
dist2 <- colMeans(dist2)
hist(dist2)
mean(dist2)

transitivity(network2, type="local")
transitivity(network2, type="localaverage")

graph.density(network2,loop=FALSE)

################# AFTER EXIT ##################

perm_after_exit <- perm2[!(perm2$V1 %in% id_that_exit),]
perm_after_exit <- perm_after_exit[!(perm_after_exit$V2 %in% id_that_exit),]
perm_after_exit$papers_tog <- NULL
perm_after_exit <- perm_after_exit %>%
  rename(aut1 = V1, aut2=V2)

a3 <- table(lapply(perm_after_exit, factor, levels = sort(unique(unlist(perm_after_exit)))))
a3[lower.tri(a3)] <- t(a3)[lower.tri(a3)]
a3 <- matrix(a3, ncol = ncol(a3), dimnames = dimnames(a3))
network3 <- igraph::graph_from_adjacency_matrix(a3, mode='undirected', diag=F)
plot(network3, layout=igraph::layout.random, main="random", vertex.size=10)
plot(network3, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=7.5, vertex.color="#0000FF25",
     edge.arrow.size = 0.09)
#igraph::tkplot(network3)

dist3 <- distances(network3)
dist3 <- colMeans(dist3)
hist(dist3)
mean(dist3)

transitivity(network3, type="local")
transitivity(network3, type="localaverage")

graph.density(network3,loop=FALSE)

################# FEATURE INCLUSION ##################

#as_data_frame(network2, what = "edges")

#how many new publications: mean(output)
#how many new authors: mean(new_added)

#in original dataset
#get mean number of coauthors, pairs, and papers by author
#get mean 

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
data_sub <- select(data, MA, A1, A2, A3, A4, A5, A6, A7)

perm_only_new <- perm_after_exit #[105:nrow(perm_after_exit), ]
#aut_id <- unique(perm_only_new$aut1) #changed on 06/22/2022 9:49pm
aut_id <- unique(c(perm_only_new$aut1, perm_only_new$aut2))

how_many <- matrix(NA, ncol=5, nrow=length(aut_id))

for (i in 1:length(aut_id)){ 
  
  sub1 <- subset(df, autname1==authors[i])
  sub2 <- subset(df, autname2==authors[i])
  sub3 <- rbind(sub1, sub2)
  how_many[i, 1]=as.numeric(i)
  how_many[i, 2]=authors[i]
  how_many[i, 3]=as.numeric(nrow(sub3)) #number of connections for every author
  
  how_many[i, 4] <- nrow(which(data_sub == authors[i], arr.ind = TRUE)) #number of papers published by aut1 (i)
  
  a <- which(data_sub == authors[i], arr.ind = TRUE) #list of paper IDs authored by aut1
  a1 <- data_sub[a[,1],]
  a1 <- unlist(a1)
  a1 <- unique(a1)
  a1 <- a1[a1 != authors[i]] #all coauthor names of aut1
  
  how_many[i, 5] <- length(a1) #number of coauthors of aut1
  
}

how_many <- data.frame(how_many)
how_many$X1 <- as.numeric(how_many$X1)
how_many$X3 <- as.numeric(how_many$X3) #avg number of connections per author
how_many$X4 <- as.numeric(how_many$X4) #avg number of publications per author
how_many$X5 <- as.numeric(how_many$X5) #avg number of coauthors per author

how_many$share_connection <- how_many$X3/sum(how_many$X3)
how_many$share_pub <- how_many$X4/sum(how_many$X4)
how_many$share_coauthor <- how_many$X5/sum(how_many$X5)

#how_many$new_connection <- share_connection
how_many$new_pub <- round(how_many$share_pub*(mean(output)))
how_many$new_coauthor <- round(how_many$share_coauthor*(mean(new_added)))

new_df <- merge(left, right, all.x=TRUE)

g <- perm_only_new %>% group_by(aut1) %>% summarise(aut1=n(aut2))

perm_only_new$paper_id <- NA

for (i in c(unique(perm_only_new$aut1))){
  
  sub <- subset(perm_only_new, aut1==i)
  range <- unique(sub$aut2)
  perm_only_new$paper_id[i] <- sample(c(how_many$new_pub[i]), replace=TRUE, nrow(sub)) #WAS FALSE
  
}



#take original data
#get number of publications and get avg number of coauthors by paper

how_many2 <- matrix(NA, ncol=6, nrow=length(aut_id))

for (i in 1:length(aut_id)){
  
  sub1 <- subset(df, aut1==aut_id[i])
  sub2 <- subset(df, aut2==aut_id[i])
  sub <- rbind(sub1, sub2)
  how_many2[i,1] <- length(unique(sub$paper)) #avg number of papers in original dataset
  g <- sub %>% group_by(paper) %>% summarise(no_coauthor=n())
  how_many2[i,2] <- mean(g$no_coauthor) #avg number of authors per paper in original dataset
  
}

#now take avg coauthor per paper lost

only_exit <- subset(exit_model, breaking==1)

for (i in 1:length(aut_id)){
  
  sub1 <- subset(only_exit, aut1==aut_id[i])
  sub2 <- subset(only_exit, aut2==aut_id[i])
  sub <- rbind(sub1, sub2)
  how_many2[i,3] <- length(unique(sub$paper)) #avg number of papers that exit
  g <- sub %>% group_by(paper) %>% summarise(no_coauthor=n())
  how_many2[i,4] <- mean(g$no_coauthor) #avg number of authors per paper that exit
  
}

#now take avg coauthor per paper gained

only_entry <- subset(entry_model, adding==1)

for (i in 1:length(aut_id)){
  
  sub1 <- subset(only_entry, aut1==aut_id[i])
  sub2 <- subset(only_entry, aut2==aut_id[i])
  sub <- rbind(sub1, sub2)
  how_many2[i,5] <- length(unique(sub$paper)) #avg number of papers that enter
  g <- sub %>% group_by(paper) %>% summarise(no_coauthor=n())
  how_many2[i,6] <- mean(g$no_coauthor) #avg number of authors per paper that enter
  
}

how_many2[is.nan(how_many2)] <- 0

how_many2 <- data.frame(how_many2)

how_many2$avg_paper <- how_many2$X1-how_many2$X3+how_many2$X5 #total number of papers after change
how_many2$avg_author <- round(how_many2$X2-how_many2$X4+how_many2$X6) #avg number of coauthors by paper
how_many2$no_new_papers <- round(how_many2$avg_paper/sum(how_many2$avg_paper)*round(mean(output))) #avg allocation of new papers


#g <- perm_only_new %>% group_by(aut1) %>% summarise(aut1=n())

papers_by_aut <- list()

for (i in 1:length(unique(perm_only_new$aut1))){
  
  #sub <- subset(perm_only_new, aut1==unique(perm_only_new$aut1)[i]) #changed on 06/22/2022 9:49pm
  
  sub <- rbind(subset(perm_only_new, aut1==unique(perm_only_new$aut1)[i]), subset(perm_only_new, aut1==unique(perm_only_new$aut2)[i]))
  
  sub$paper_id <- sample(1:round(mean(output)), size=nrow(sub), replace=TRUE)
  papers_by_aut[[i]] <- sub
  
}

main_df <- do.call("rbind", papers_by_aut) #allocates new papers

#g <- main_df %>% group_by(paper_id) %>% summarise(number=n())

final_df <- list()

for (i in 1:length(unique(main_df$paper_id))) {
  
  sub <- subset(main_df, paper_id==(unique(main_df$paper_id))[i]) #sub <- subset(main_df, paper_id==i)
  coauthors_by_paper <- unique(unlist(sub[,1:2],use.names = FALSE))
  #coauthors_by_paper <- unlist(lapply(coauthors_by_paper, function(x) paste("A", x, sep="")))
  t(coauthors_by_paper) %>% data.frame() -> df1
  final_df[[i]] <- df1
  
}

final_df <- do.call("bind_rows", final_df)

final_df <- final_df[!is.na(final_df$X1),]

final_df <- final_df[,1:8]

colnames(final_df) <- c("MA","A1", "A2", "A3", "A4", "A5", "A6", "A7")

final_df[] <- lapply(final_df, function(x) paste("A", x, sep=""))

final_df[final_df == "ANA" ] <- NA

final_df$Index <- 1:nrow(final_df)
final_df$Year <- sample(c(2006:2020), replace=TRUE, size=nrow(final_df))
final_df$Fund <- sample(c(100000, 200000, 300000, 400000, 500000, 600000, 700000), size=nrow(final_df), replace=TRUE)
final_df$Paper <- paste("P", final_df$Index)
final_df$Citations <- sample(c(0:50), replace=TRUE, size=nrow(final_df))

final_df[is.na(final_df)] <- "" 

write.csv(final_df, file="funding_new.csv")  







data <- read.csv("funding_org.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), 
             unique(data$A4), unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
data_sub <- select(data, MA, A1, A2, A3, A4, A5, A6, A7)

x <- c(1:length(authors))
n <- length(x)
m <- 2

perm <- CombSet(x, m, repl=FALSE, ord=FALSE) #gets unique combination sets

estimate=matrix(NA,nrow(perm),1)

for(i in 1:nrow(perm)){
  
  aut1 <- perm[i, 1] #gets the first element of first set
  aut2 <- perm[i, 2] #gets the second element of first set
  a <- which(data_sub == authors[aut1], arr.ind = TRUE) #list of paper authored by aut1
  b <- which(data_sub == authors[aut2], arr.ind = TRUE) #list of paper authored by aut2
  papers_tog <- length(intersect(a[,1], b[,1])) #number of unique papers coauthored by aut1 and aut2
  estimate[i]=papers_tog
  
}

perm <- as.data.frame(perm)
perm$papers_tog <- estimate
perm$papers_tog <- ifelse(perm$papers_tog > 0, 1, 0)
perm <- subset(perm, papers_tog==1)

a <- table(lapply(perm[-3], factor, levels = sort(unique(unlist(perm[-3])))))
a[lower.tri(a)] <- t(a)[lower.tri(a)]
a <- matrix(a, ncol = ncol(a), dimnames = dimnames(a))

network <- graph_from_adjacency_matrix(a, mode='undirected', diag=F)
plot(network, layout=layout.sphere, main="sphere")
plot(network, layout=layout.random, main="random")






n=20                          # Size of matrix
mat=matrix(0,ncol=n,nrow=n)   # Create a n x n matrix with zeros
mat[5:14,10]=1                # Add 10 live cells
temp_mat=mat                  # Create a temporary matrix
image(t(apply(mat, 2, rev)),col=c("grey50","seagreen1"),yaxt="n",xaxt="n") # Plot image
grid(nx=n,ny=n,col="grey70",lty=1)
mat1=mat[c(2:n,1),];mat2=mat[c(n,1:n-1),]; mat3=mat[,c(2:n,1)]; mat4=mat[,c(n,1:n-1)]; mat5=mat[c(2:n,1),c(2:n,1)]; mat6=mat[c(2:n,1),c(n,1:n-1)]
mat7=mat[c(n,1:n-1),c(n,1:n-1)];mat8=mat[c(n,1:n-1),c(2:n,1)];
for (k in 1:200){      # Repeat 200 times
  numb_alive=mat1+mat2+mat3+mat4+mat5+mat6+mat7+mat8
  temp_mat[mat==1 & numb_alive<2]=0
  temp_mat[mat==1 & numb_alive>3]=0
  temp_mat[mat==1 & (numb_alive==2 | numb_alive==3)]=1
  temp_mat[mat==0 & numb_alive==3]=1
  mat=temp_mat # Update matrix
  mat1=mat[c(2:n,1),];mat2=mat[c(n,1:n-1),];mat3=mat[,c(2:n,1)];mat4=mat[,c(n,1:n-1)]
  mat5=mat[c(2:n,1),c(2:n,1)];mat6=mat[c(2:n,1),c(n,1:n-1)];mat7=mat[c(n,1:n-1),c(2:n,1)]
  mat8=mat[c(n,1:n-1),c(n,1:n-1)]
  image(t(apply(mat, 2, rev)),col=c("grey50","seagreen1"),add=TRUE)   # Plot image
  grid(nx=n,ny=n,col="grey70",lty=1)
  Sys.sleep(0.5) # To see changes on the screen we need to pause the loop
}







