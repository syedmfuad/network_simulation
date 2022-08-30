##################### REGRESSIONS ######################

authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))

authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
authors<-authors[!is.na(authors)]

x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

##################### POISSON ####################

m2 <- glm(depvar ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, family = poisson, data = prod_model)
#m2 <- zeroinfl(depvar ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 | 1, data = prod_model)
#m2 <- MASS::glm.nb(depvar ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, data = prod_model, control=glm.control(maxit=100))
summary(m2, exp = T)
LL <- (2*7-1297.70)/2


                Estimate Std. Error z value Pr(>|z|)    
  (Intercept)  5.1313667  0.7490553   6.850 7.36e-12 ***
  x2          -0.2118467  0.0729126  -2.905  0.00367 ** 
  x3          -0.9431546  0.0447257 -21.088  < 2e-16 ***
  x4          -0.1285006  0.0113565 -11.315  < 2e-16 ***
  x5           0.9015653  0.0563061  16.012  < 2e-16 ***
  x6          -0.1698154  0.0545998  -3.110  0.00187 ** 
  x7           0.0134969  0.0049405   2.732  0.00630 ** 
  x8           0.0146145  0.0006847  21.346  < 2e-16 ***
                  
                

#total number of papers (distribution)

data <- read.csv("funding_new.csv")

output <- matrix(ncol=nrow(prod_model), nrow=1000)

for(i in 1:nrow(prod_model)){
  set.seed(i)
  output[,i] = rpois(1000, lambda=exp(predict(m2))[i])
}

#df_test <- data.frame(original=prod_model$depvar, sim=output[1,])

output <- rowSums(output)/(nrow(df)/nrow(data))
hist(output, main="Distribution of new publications", xlab="New Publications") #number of publications
mean(output)

#y <- predict(m2, type = 'response')

##################### MLOGIT ####################

entry_model$entry <- as.factor(entry_model$entry)

entry_model$entry = relevel(entry_model$entry, ref = 3)

#3-brings in additional author who continues to publish
#2-brings in additional authors who only publishes once
#1-does not bring additional author

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
mean(new_added, na.rm=TRUE)

#which authors get new authors

entry_model <- cbind(entry_model, pred)

unif <- matrix(NA,nrow(pred),1)

for (i in 1:nrow(pred)){
  random <- runif(1000, 0, 1)
  unif[i] <- mean(random)
}

entry_model$unif <- unif

#entry_model$adding <- ifelse(entry_model$`3` > entry_model$unif, 1, 0) #-> this was original line
entry_model$adding <- entry_model$entry_pred # -> this is new line

summary(entry_model$adding)

aut_entry <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){ #number of connections for every author
  
  sub <- subset(entry_model, adding==1) #number of new connections
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
aut_entry$X4 <- round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub))/2, 0) #round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub)), 0) 

##################### LOGIT ####################

mod2 <- glm(exit ~ x2 + x3 + x4 + x5 + x6 + x7 + x8, family=binomial("logit"), data=exit_model)
summary(mod2)
#pred <- as.data.frame(round(fitted(mod2), 2), 10) #-> old
#exit_model$pred <- pred$`round(fitted(mod2), 2)` #-> old
pred = data.frame(probs = (predict(mod2, type="response")))

exit_model$pred <- pred$probs

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

#exit_model$unif <- pred$random <- runif(nrow(pred), 0, 1)
exit_model$breaking <- ifelse(exit_model$pred > exit_model$unif, 1, 0)
exit_model$who_exits <- exit_model$single_aut1*exit_model$breaking

new_exit <- matrix(NA,1000,1)

for (i in 1:1000){
  pred$random <- runif(nrow(pred), 0, 1)
  #pred$breaking <- ifelse(pred$`round(fitted(mod2), 2)` > pred$random, 1, 0) #-> old
  pred$breaking <- ifelse(pred$probs > pred$random, 1, 0)
  new_exit[i] <- (length(which(pred$breaking == 1))/(nrow(pred)/length(authors)))
}

hist(new_exit, main="Distribution of exiting authors (nodes)", xlab="Exiting Authors")
mean(new_exit)

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










#authors <- c(unique(model_after_exit$autname1), unique(model_after_exit$autname2))

#authors <- unique(authors)
#length(authors)

#extra added 08/20/2022

#first for every author, get number of new connections and exits

aut_exit <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){ #number of connections for every author
  
  sub <- subset(exit_model, breaking==1) #number of new connections
  sub1 <- subset(sub, autname1==authors[i])
  sub2 <- subset(sub, autname2==authors[i])
  sub3 <- rbind(sub1, sub2)
  aut_exit[i, 1]=as.numeric(i) #author id
  aut_exit[i, 2]=authors[i] #author name
  aut_exit[i, 3]=as.numeric(nrow(sub3))
  
}

aut_exit <- data.frame(aut_exit)
aut_exit$X1 <- as.numeric(aut_exit$X1)
aut_exit$X3 <- as.numeric(aut_exit$X3)
aut_exit$X4 <- round(((aut_exit$X3/sum(aut_exit$X3))*nrow(sub))/2, 0) #round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub)), 0)



aut_original <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){ #number of connections for every author
  
  sub <- subset(entry_model, depvar>0) #number of new connections
  sub1 <- subset(sub, autname1==authors[i])
  sub2 <- subset(sub, autname2==authors[i])
  sub3 <- rbind(sub1, sub2)
  aut_original[i, 1]=as.numeric(i) #author id
  aut_original[i, 2]=authors[i] #author name
  aut_original[i, 3]=as.numeric(nrow(sub3))
  
}

aut_original <- data.frame(aut_original)
aut_original$X1 <- as.numeric(aut_original$X1)
aut_original$X3 <- as.numeric(aut_original$X3)
aut_original$X4 <- round(((aut_original$X3/sum(aut_original$X3))*nrow(sub))/2, 0)


 
aut_list <- matrix(NA, ncol=3, nrow=length(authors))

data_sub <- select(data, MA, A1, A2, A3, A4, A5, A6, A7)

for(i in 1:length(authors)){
  
  a <- which(data_sub == authors[i], arr.ind = TRUE) #list of paper authored by aut1
  
  aut_list[i, 1]=as.numeric(i) #author id
  aut_list[i, 2]=authors[i] #author name
  aut_list[i, 3]=nrow(a)
  
}

aut_list <- data.frame(aut_list)
aut_list$X1 <- as.numeric(aut_list$X1)
aut_list$X3 <- as.numeric(aut_list$X3)


aut_final <- cbind(aut_original, aut_list[,3], aut_entry[,4], aut_exit[,4])

aut_final$new_papers_prop <- aut_final[,4]+aut_final[,6]-aut_final[,7]
aut_final$new_autlist3 <- (aut_final[,5]/aut_final[,4])*aut_final[,8]
aut_final2 <- aut_final[!(aut_final$X1 %in% id_that_exit),]
output <- (nrow(data)/sum(aut_final[,5], na.rm=TRUE))*sum(aut_final2[,9], na.rm=TRUE)






