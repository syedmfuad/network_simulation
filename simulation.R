library(igraph)

write.csv(prod_model, file="prod_model.csv")
write.csv(entry_model, file="entry_model.csv")
write.csv(exit_model, file="exit_model.csv")

prod_model <- read.csv("prod_model.csv")
entry_model <- read.csv("entry_model.csv")
exit_model <- read.csv("exit_model.csv")

#poisson model

new_exit_model <- exit_model %>% select(concat, breaking)
new_exit_model <- subset(new_exit_model, breaking==1)

new_prod_model <- prod_model[!(prod_model$concat %in% new_exit_model$concat),]

#new_prod_model %>% arrange(desc(depvar)) %>%  slice(1:10) -> new_model1

new_prod_model %>% arrange(desc(x3)) %>%  slice(1:10) -> new_model1

new_model1$x2 <- new_model1$x2-1
new_model1$x7 <- new_model1$x2^2

#new_prod_model %>% arrange(desc(depvar)) %>%  slice(11:nrow(new_prod_model)) -> new_model2

new_prod_model %>% arrange(desc(x3)) %>%  slice(11:nrow(new_prod_model)) -> new_model2

new_model2$x2 <- 0
new_model2$x7 <- new_model2$x2^2

new_prod_model <- rbind(new_model1, new_model2)

prod_for_pred <- new_prod_model %>% select(x2, x3, x4, x5, x6, x7, x8)

pred_p <- predict(m2, newdata = prod_for_pred, type = "response")
output <- sum(pred_p)/(516/85)
#output <- sum(pred_p)/(sum(round(pred_p))/output)

#maybe use predicted values as depvar



output_plot <- matrix(ncol=length(pred_p), nrow=1000)

for(i in 1:length(pred_p)){
  set.seed(i)
  output_plot[,i] = rpois(1000, lambda=pred_p[i])
}

output_plot <- rowSums(output_plot)/(sum((pred_p))/output)
hist(output_plot, main="Distribution of publications", xlab="Publications") #number of publications
mean(output_plot)



#mlogit model and entry model

new_exit_model <- exit_model %>% select(concat, breaking)
new_exit_model <- subset(new_exit_model, breaking==1) #connections that break

new_entry_model <- entry_model[!(entry_model$concat %in% new_exit_model$concat),] #connections that dont break

new_model1 <- new_entry_model[(new_entry_model$concat %in% new_model1$concat),]

new_model1$x2 <- new_model1$x2-1
new_model1$x7 <- new_model1$x2^2

new_model2 <- new_entry_model[(new_entry_model$concat %in% new_model2$concat),]

new_model2$x2 <- 0
new_model2$x7 <- new_model2$x2^2

new_entry_model <- rbind(new_model1, new_model2)

entry_for_pred <- new_entry_model %>% select(x2, x3, x4, x5, x6, x7, x8)

pred_ml <- predict(mod1, newdata = entry_for_pred, type="prob")

new_added <- matrix(NA,1000,1)

pred_ml <- as.data.frame(pred_ml)

for (i in 1:1000){
  pred_ml$random <- runif(nrow(pred_ml), 0, 1)
  pred_ml$entry_pred <- ifelse(pred_ml$`3` > pred_ml$random, 1, 0)
  new_added[i] <- length(which(pred_ml$entry_pred == 1))/(nrow(pred_ml)/length(authors))
}

hist(new_added, main="Distribution of new authors (nodes)", xlab="New Authors")
mean(new_added, na.rm=TRUE)

summary(pred_ml$entry_pred)




#logit model and exit model

new_exit_model <- subset(exit_model, breaking==0)

new_prod_model_ext <- subset(new_prod_model, x2 != 0)

new_model1 <- new_exit_model[!(new_exit_model$concat %in% new_prod_model_ext$concat),]

new_model1$x2 <- new_model1$x2-1
new_model1$x7 <- new_model1$x2^2

new_model2 <- new_exit_model[(new_exit_model$concat %in% new_prod_model_ext$concat),]

new_model2$x2 <- 0
new_model2$x7 <- new_model2$x2^2

new_exit_model <- rbind(new_model1, new_model2)

exit_for_pred <- new_exit_model %>% select(x2, x3, x4, x5, x6, x7, x8)

pred_l <- predict(mod2, newdata = exit_for_pred, type="response")

new_exit <- matrix(NA,1000,1)

pred_l <- as.data.frame(pred_l)

for (i in 1:1000){
  pred_l$random <- runif(nrow(pred_l), 0, 3)
  pred_l$exit_pred <- ifelse(pred_l$pred_l > pred_l$random, 1, 0)
  new_exit[i] <- length(which(pred_l$exit_pred == 1))/(nrow(pred_l)/length(authors))
}

hist(new_exit, main="Distribution of exiting authors (nodes)", xlab="Exiting Authors")
mean(new_exit, na.rm=TRUE)

summary(pred_l$exit_pred)
summary(exit_model$breaking)


#plot of agents
#agents should be replaced by 72 in first iteration

agents <- 72+new_added-new_exit
agents_mean <- mean(agents)

hist(agents, main="Distribution of authors", xlab="Authors")

125.5024
171.1649


new_prod_model$depvar <- pred_p

prod_model <- new_prod_model




new_entry_model$entry_pred <- pred_ml$entry_pred

entry_model <- new_entry_model




new_exit_model$breaking <- pred_l$exit_pred

exit_model <- new_exit_model




#plot

#who gets new authors

entry_model <- subset(entry_model, entry_pred==1)

authors <- c(unique(entry_model$autname1), unique(entry_model$autname2))
authors <- unique(authors)

aut_entry <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){ #number of connections for every author
  
  sub <- subset(entry_model, entry_pred==1) #number of new connections
  sub1 <- subset(sub, autname1==authors[i]) #was entry_model
  sub2 <- subset(sub, autname2==authors[i]) #was entry_model
  sub3 <- rbind(sub1, sub2)
  aut_entry[i, 1]=as.numeric(i) #author id
  aut_entry[i, 2]=authors[i] #author name
  aut_entry[i, 3]=as.numeric(nrow(sub3))
  
}

aut_entry <- data.frame(aut_entry)
aut_entry$X1 <- as.numeric(aut_entry$X1)
aut_entry$X3 <- as.numeric(aut_entry$X3)

#so number of new authors is agents_mean-length(authors)

aut_entry$X4 <- round((aut_entry$X3/sum(aut_entry$X3))*(agents_mean-length(authors)))
sum(aut_entry$X4)

#id names in aut_entry do not match with prod_plot

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

aut_entry <- left_join(aut_entry, name_id, by = c('X2' = 'Name'))


list_est <- list()

for(i in 1:nrow(aut_entry)){
  
  x_var=matrix(NA,aut_entry$X4[i],2)
  x_var[,1] <- aut_entry$ID[i]
  x_var[,2] <- sample(c((length(authors)+1):sum(aut_entry$X4)), size=nrow(x_var))
  list_est[[i]] <- x_var
  
}

list_est <- do.call(rbind.data.frame, list_est)

list_est$papers_tog <- 1

colnames(list_est)[1] <- "aut1"
colnames(list_est)[2] <- "aut2"



prod_plot <- subset(prod_model, depvar>0.5)

prod_plot <- select(prod_plot, aut1, aut2)
prod_plot$papers_tog <- 1

#adding list_est with prod_plot

prod_plot <- rbind(prod_plot, list_est)

a <- table(lapply(prod_plot[-3], factor, levels = sort(unique(unlist(prod_plot[-3])))))
a[lower.tri(a)] <- t(a)[lower.tri(a)]
a <- matrix(a, ncol = ncol(a), dimnames = dimnames(a))

network <- igraph::graph_from_adjacency_matrix(a, mode='undirected', diag=F)
plot(network, layout=igraph::layout.fruchterman.reingold, main="Smart Small World Network", 
     vertex.size=7.5, vertex.color="#0000FF25", edge.arrow.size = 0.09)

dist <- distances(network)
dist <- colMeans(dist)
hist(dist, main="Distribution of path length", xlab="Path length")
mean(dist)

transitivity(network, type="local")
transitivity(network, type="localaverage")

graph.density(network,loop=FALSE)








