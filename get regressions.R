##################### REGRESSIONS ######################

authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))

authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors

x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

##################### POISSON ####################

m2 <- glm(depvar ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, family = poisson, data = prod_model) 
summary(m2, exp = T)
LL <- (2*7-1297.70)/2

#random things with model

hist(exp((predict(m2))))

output <- matrix(ncol=14, nrow=nrow(prod_model))

for(i in 1:nrow(prod_model)){
  
  output[i,1] = dpois(0, lambda=exp(predict(m2))[i])
  output[i,2] = dpois(1, lambda=exp(predict(m2))[i])
  output[i,3] = dpois(2, lambda=exp(predict(m2))[i])
  output[i,4] = dpois(3, lambda=exp(predict(m2))[i])
  output[i,5] = dpois(4, lambda=exp(predict(m2))[i])
  output[i,6] = dpois(5, lambda=exp(predict(m2))[i])
  output[i,7] = dpois(6, lambda=exp(predict(m2))[i])
  output[i,8] = dpois(7, lambda=exp(predict(m2))[i])
  output[i,9] = dpois(8, lambda=exp(predict(m2))[i])
  output[i,10] = dpois(9, lambda=exp(predict(m2))[i])
  output[i,11] = dpois(10, lambda=exp(predict(m2))[i])
  output[i,12] = dpois(11, lambda=exp(predict(m2))[i])
  output[i,13] = dpois(12, lambda=exp(predict(m2))[i])
  output[i,14] = dpois(13, lambda=exp(predict(m2))[i])
  
}

output <- as.data.frame(output) #data frame with probabilities for each outcome
output$total <- rowSums(output[,c(1:14)])
summary(output$total)

#total number of papers (distribution)

data <- read.csv("funding_new.csv")

output <- matrix(ncol=nrow(prod_model), nrow=1000)

for(i in 1:nrow(prod_model)){
  set.seed(i)
  output[,i] = rpois(1000, lambda=exp(predict(m2))[i])
}

output <- rowSums(output)/(nrow(df)/nrow(data))
hist(output, main="Distribution of new publications", xlab="New Publications") #number of publications

#random things with poisson (graphs)

freq(prod_model$depvar) #91.86% had no publications
mean(prod_model$depvar) #average number of articles is 0.20

mean(prod_model$depvar)^0 * exp(-mean(prod_model$depvar)) / factorial(0) #predicted prob of 0 articles is 0.82

fr <- table(prod_model$depvar) %>% data.frame
names(fr) <- c('articles', 'freq')
fr$articles <- as.numeric(as.character(fr$articles)) #convert factor to numeric
ggplot(fr, aes(x = articles, y = freq)) +
  geom_col() + theme_bw() + lims(y = c(0, 2400)) + geom_line() + labs(x = "Number of articles", y = "Frequency") +
  geom_text(aes(x = articles, y = freq, label = freq, vjust = -1)) +
  theme(axis.title.y = element_text(angle = 0))

##################### MLOGIT ####################

entry_model$entry <- as.factor(entry_model$entry)

entry_model$entry = relevel(entry_model$entry, ref = 3)

mod1 <- multinom(entry ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                 MaxNWts =10000000, maxit=100, data=entry_model)
summary(mod1)
multi1.rrr = exp(coef(mod1))
pred <- as.data.frame(round(fitted(mod1), 2), 10)

#total number of nodes added (distribution)

mod1 <- multinom(entry ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                 MaxNWts =10000000, maxit=100, data=entry_model)
summary(mod1)
multi1.rrr = exp(coef(mod1))
pred <- as.data.frame(round(fitted(mod1), 2), 10)

new_added <- matrix(NA,1000,1)

for (i in 1:1000){
  pred$random <- runif(nrow(pred), 0, 1)
  pred$entry_pred <- ifelse(pred$`3` > pred$random, 1, 0)
  #new_added[i] <- round(length(which(pred$entry == 1))/(nrow(pred)/72))
  new_added[i] <- length(which(pred$entry_pred == 1))/(nrow(pred)/length(authors))
}

hist(new_added, main="Distribution of new authors (nodes)", xlab="New Authors")
mean(new_added)

#which authors get new authors

entry_model <- cbind(entry_model, pred)

unif <- matrix(NA,nrow(pred),1)

for (i in 1:nrow(pred)){
  random <- runif(1000, 0, 1)
  unif[i] <- mean(random)
}

entry_model$unif <- unif

########## extra ##########

unif <- matrix(NA,nrow(pred),1000)

for (i in 1:nrow(pred)){
  #random <- runif(1000, 0, 1)
  unif[i,] <- runif(1000, 0, 1)
}

unif <- data.frame(unif)
unif$pred <- entry_model$`3`

for (i in 1:ncol(unif)){
  #random <- runif(1000, 0, 1)
  unif[,i] <- ifelse(unif$pred > unif[,i], 1, 0)
}

entry_model$unif <- rowMeans(unif[,1:1000])

########## extra ##########

entry_model$adding <- ifelse(entry_model$`3` > entry_model$unif, 1, 0)
#entry_model$diff <- entry_model$`3`-entry_model$unif

summary(entry_model$adding)

aut_entry <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){ #number of connections for every author
  
  sub <- subset(entry_model, adding==1)
  sub1 <- subset(sub, autname1==authors[i]) #was entry_model
  sub2 <- subset(sub, autname2==authors[i]) #was entry_model
  sub3 <- rbind(sub1, sub2)
  aut_entry[i, 1]=as.numeric(i)
  aut_entry[i, 2]=authors[i]
  aut_entry[i, 3]=as.numeric(nrow(sub3))
  
}

aut_entry <- data.frame(aut_entry)
aut_entry$X1 <- as.numeric(aut_entry$X1)
aut_entry$X3 <- as.numeric(aut_entry$X3)
aut_entry$X4 <- round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub))/2, 0) #round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub)), 0) 

##################### LOGIT ####################

mod2 <- glm(exit ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, family=binomial("logit"), data=exit_model)
summary(mod2)
pred <- as.data.frame(round(fitted(mod2), 2), 10)

exit_model$pred <- pred$`round(fitted(mod2), 2)`

estimate=matrix(NA,nrow(name_id),1)

for (i in 1:nrow(name_id)) {
  
  sub1 <- exit_model[exit_model$aut1 == i, ]
  sub2 <- exit_model[exit_model$aut2 == i, ]
  sub <- rbind(sub1, sub2)
  estimate[i]=mean(sub$pred)
  
}

estimate <- as.data.frame(estimate) #dataframe of average probability of exit from a pair with this author



estimate$ID <- c(1:length(authors))

exit_model2 <- left_join(exit_model, estimate, by = c('aut1' = 'ID'))
exit_model3 <- left_join(exit_model, estimate, by = c('aut2' = 'ID'))

exit_model$prob1 <- exit_model2$V1
exit_model$prob2 <- exit_model3$V1

exit_model$single_aut1 <- ifelse(exit_model$prob1>exit_model$prob2, 1, 2)

unif <- matrix(NA,nrow(pred),1)

for (i in 1:nrow(pred)){
  random <- runif(1000, 0, 1)
  unif[i] <- mean(random)
}

exit_model$unif <- unif



########## extra ##########

unif <- matrix(NA,nrow(pred),1000)

for (i in 1:nrow(pred)){
  #random <- runif(1000, 0, 1)
  unif[i,] <- runif(1000, 0, 1)
}

unif <- data.frame(unif)
unif$pred <- exit_model$pred

for (i in 1:ncol(unif)){
  #random <- runif(1000, 0, 1)
  unif[,i] <- ifelse(unif$pred > unif[,i], 1, 0)
}

exit_model$unif <- rowMeans(unif[,1:1000])

########## extra ##########



#exit_model$unif <- pred$random <- runif(nrow(pred), 0, 1)
exit_model$breaking <- ifelse(exit_model$pred > exit_model$unif, 1, 0)
exit_model$who_exits <- exit_model$single_aut1*exit_model$breaking

new_exit <- matrix(NA,1000,1)

for (i in 1:1000){
  pred$random <- runif(nrow(pred), 0, 1)
  pred$breaking <- ifelse(pred$`round(fitted(mod2), 2)` > pred$random, 1, 0)
  new_exit[i] <- (length(which(pred$breaking == 1))/(nrow(pred)/length(authors)))
}

hist(new_exit, main="Distribution of exiting authors (nodes)", xlab="Exiting Authors")


#we have to permanently remove some authors from dataframe
#to do this, we first use the who_exits variable
#according to this variable, we get the average probability of exit
#get every author's probability of exit every time one of one of it's connections break

exit_model$who_exits_id <- ifelse(exit_model$who_exits==2, exit_model$aut2, ifelse(exit_model$who_exits==1, exit_model$aut1, 0))

estimate=matrix(NA,nrow(name_id),1)

for (i in 1:nrow(name_id)) {
  
  exit_only <- subset(exit_model, who_exits>0)
  
  sub1 <- exit_only[exit_only$aut1 == i, ]
  sub2 <- exit_only[exit_only$aut2 == i, ]
  sub <- rbind(sub1, sub2)
  estimate[i] <- length(which(sub$who_exits_id == i))/nrow(sub)
  
}

estimate[is.nan(estimate)] <- 0

name_id$prob_exit <- estimate

authors_after_exit <- subset(name_id, prob_exit != 1)
authors_after_exit <- authors_after_exit$Name

authors_that_exit <- subset(name_id, prob_exit >= 1)
id_that_exit <- authors_that_exit$ID
authors_that_exit <- authors_that_exit$Name


model_after_exit1 <- exit_model[!exit_model$autname1 %in% authors_that_exit, ]
model_after_exit <- model_after_exit1[!model_after_exit1$autname2 %in% authors_that_exit, ]


