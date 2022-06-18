rm(list=ls())
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

data <- read.csv("funding_new.csv")

authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))

authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors

data_sub <- select(data, MA, A1, A2, A3, A4, A5, A6, A7)

x <- c(1:length(authors))
n <- length(x)
m <- 2

perm <- CombSet(x, m, repl=FALSE, ord=FALSE) #gets unique combination sets (permutation/combination)

x_var=matrix(NA,nrow(perm),7) #empty matrix for x-variables

for(i in 1:nrow(perm)){
  
  aut1 <- perm[i, 1] #gets the first element of first combination set
  aut2 <- perm[i, 2] #gets the second element of first combination set
  
  #total papers where they are coauthors together
  
  a <- which(data_sub == authors[aut1], arr.ind = TRUE) #list of paper IDs authored by aut1
  b <- which(data_sub == authors[aut2], arr.ind = TRUE) #list of paper IDs authored by aut2
  
  papers_tog <- length(intersect(a[,1], b[,1])) #total number of unique papers coauthored by aut1 and aut2
  
  papers_total <- length(unique(c(a[,1], b[,1]))) #total number of unique papers authored by aut1 and aut2 (may not be coauthored)
  
  #funding
  
  ab <- unique(c(a[,1],b[,1])) #list of unique paper IDs authored by aut1 and aut2
  
  fund_sub <- select(data, Fund)
  fund_sub <- unique(fund_sub[ab,])
  
  fund_sub <- as.numeric(fund_sub)
  total_fund <- sum(fund_sub, na.rm=TRUE)/length(ab) #average funds ($) of all projects with aut1 and aut2
  
  #first step relationships
  
  a1 <- data_sub[a[,1],]
  a1 <- unlist(a1)
  a1 <- unique(a1)
  a1 <- a1[a1 != authors[aut1]] #all coauthor names of aut1
  
  b1 <- data_sub[b[,1],]
  b1 <- unlist(b1)
  b1 <- unique(b1)
  b1 <- b1[b1 != authors[aut2]] #all coauthor names of aut2
  
  total_f_d <- c(a1, b1)
  total_f_d <- unique(total_f_d)
  total_f_d <- length(total_f_d) #total number of unique coauthors of aut1 and aut2
  
  #second step relationships
  
  list_a<-list() #all second step coauthors (coauthors of coauthors) of aut1
  
  for (j in 1:length(a1)){ #repeating the above steps (that get the coauthors) for all coauthors of aut1
    
    a2 <- which(data_sub == a1[j], arr.ind = TRUE)
    
    a2_1 <- data_sub[a2[,1],]
    a2_1 <- unlist(a2_1)
    a2_1 <- unique(a2_1)
    a2_1 <- a2_1[a2_1 != a1[j]]
    
    datvec1 = a2_1[!(a2_1 %in% a1)]
    datvec1 = datvec1[!(datvec1 %in% authors[aut1])]
    
    list_a[[j]] <- datvec1 #every element in datavec1 list is a vector of coauthors of coauthors of aut1 
    
  }
  
  list_b<-list() #all second step coauthors (coauthors of coauthors) of aut2
  
  for (k in 1:length(b1)){ #repeating the above steps (that get the coauthors) for all coauthors of aut2
    
    b2 <- which(data_sub == b1[k], arr.ind = TRUE)
    
    b2_1 <- data_sub[b2[,1],]
    b2_1 <- unlist(b2_1)
    b2_1 <- unique(b2_1)
    b2_1 <- b2_1[b2_1 != b1[k]]
    
    datvec2 = b2_1[!(b2_1 %in% b1)]
    datvec2 = datvec2[!(datvec2 %in% authors[aut2])]
    
    list_b[[k]] <- datvec2 #every element in datavec2 list is a vector of coauthors of coauthors of aut2
    
  }
  
  datvec1 <- unlist(list_a)
  datvec1 <- unique(datvec1) #all second degree coauthor names of aut1
  
  datvec2 <- unlist(list_b)
  datvec2 <- unique(datvec2) #all second degree coauthor names of aut2
  
  total_s_d <- c(datvec1, datvec2)
  total_s_d <- unique(total_s_d)
  total_s_d <- length(total_s_d) #total number of second degree coauthors of aut1 and aut2
  
  #number of coauthors in papers together
  
  #papers together
  
  a <- which(data_sub == authors[aut1], arr.ind = TRUE) #list of paper IDs authored by aut1
  b <- which(data_sub == authors[aut2], arr.ind = TRUE) #list of paper IDs authored by aut2
  
  ca <- intersect(a[,1], b[,1])
  
  ca <- data_sub[ca,]
  ca <- unlist(ca)
  ca <- unique(ca)
  
  remove <- c(authors[aut1], authors[aut2])
  ca <- ca[!ca %in% remove]
  ca <- length(ca) #total number of coauthors in unique papers together by aut1 and aut2
  
  #new authors
  
  a <- which(data_sub == authors[aut1], arr.ind = TRUE) #list of paper IDs authored by aut1
  b <- which(data_sub == authors[aut2], arr.ind = TRUE) #list of paper IDs authored by aut2
  
  ca1 <- intersect(a[,1], b[,1]) #list of paper IDs coauthored by aut1 and aut2
  
  only_a <- a[,1][!(a[,1] %in% ca1)]
  only_a <- data_sub[only_a,]
  only_a <- unlist(only_a)
  only_a <- unique(only_a)
  only_a <- only_a[only_a != authors[aut1]] #list of new author names introduced by aut1
  
  only_b <- b[,1][!(b[,1] %in% ca1)]
  only_b <- data_sub[only_b,]
  only_b <- unlist(only_b)
  only_b <- unique(only_b)
  only_b <- only_b[only_b != authors[aut2]] #list of new author names introduced by aut2
  
  ca1 <- data_sub[ca1,]
  ca1 <- unlist(ca1)
  ca1 <- unique(ca1)
  ca1 <- ca1[ca1 != authors[aut1]]
  ca1 <- ca1[ca1 != authors[aut2]]
  
  only_a <- ca1[!(ca1 %in% only_a)]
  only_b <- ca1[!(ca1 %in% only_b)]
  
  only_new <- length(unique(c(only_a, only_b))) #total number of new authors introduced by aut1 and aut2
  
  x_var[i,1]=papers_tog
  x_var[i,2]=papers_total
  x_var[i,3]=total_fund
  x_var[i,4]=total_f_d
  x_var[i,5]=total_s_d
  x_var[i,6]=ca
  x_var[i,7]=only_new
  
}

depvar <- as.data.frame(perm) #getting dependent variable #number of papers by each pair

for (i in 1:nrow(depvar)){
  
  j <- depvar[i,1]
  depvar[i,3] <- authors[j]
  
  k <- depvar[i,2]
  depvar[i,4] <- authors[k]
  
}

x_var_df <- as.data.frame(x_var)
prod_model <- cbind(x_var_df, depvar) #merge depvar with x-variables

colnames(prod_model) <- c("depvar", "x1", "x2", "x3", "x4", "x5", "x6", "aut1", "aut2", "autname1", "autname2")

prod_model$concat <- paste(prod_model$autname1, prod_model$autname2, sep=" ")

prod_model$x7 <- (prod_model$x2)^2
prod_model$x8 <- (prod_model$x3)*(prod_model$x4)

rm(list=setdiff(ls(), "prod_model"))

############################# MLOGIT #############################

list_paper<-list()
data <- read.csv("funding_new.csv")

for(i in 1:nrow(data)){
  
  data1 <- data[i,] #getting first paper since this is a paper-wise analysis
  
  authors <- c(unique(data1$MA), unique(data1$A1), unique(data1$A2), unique(data1$A3), unique(data1$A4), 
               unique(data1$A5), unique(data1$A6), unique(data1$A7))
  
  authors <- unique(authors)
  authors <- authors[authors != ""]
  
  data_sub <- select(data1, MA, A1, A2, A3, A4, A5, A6, A7)
  
  x <- c(1:length(authors))
  n <- length(x)
  m <- 2
  
  # the samples
  perm <- CombSet(x, m, repl=FALSE, ord=FALSE) #gets unique combination sets (permutation/combination)
  
  perm1 <- as.data.frame(perm)
  perm1$col <- i
  perm1$year <- data[i,5]
  
  for (j in 1:nrow(perm1)){
    
    k <- perm1[j,1]
    perm1[j,5] <- authors[k]
    
    l <- perm1[j,2]
    perm1[j,6] <- authors[l]
    
  }
  
  list_paper[[i]] <- perm1 #every element in the list is data frame of every unique 2-author combination for every paper
  
}

df <- do.call(rbind.data.frame, list_paper) #data frame of all and every unique 2-author combination for every paper

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit")))

colnames(df) <- c("aut1", "aut2", "paper", "year", "autname1", "autname2")

df$concat <- paste(df$autname1, df$autname2, sep=" ")

mlogit <- merge(prod_model, df, by.x=c("concat"), by.y=c("concat")) #merge df with the Xs and Ys from first part

#checking stuff

authors <- c(unique(mlogit$autname1.x), unique(mlogit$autname2.x))
authors <- unique(authors)
authors <- authors[authors != ""]

a <- which(mlogit["autname1.x"] == authors[1], arr.ind = TRUE)
length(unique(mlogit[,"paper"][a[,1]]))

b <- which(mlogit["autname2.x"] == authors[1], arr.ind = TRUE)
length(unique(mlogit[,"paper"][b[,1]]))

#remove unnecessary columns

mlogit$aut1.x <- NULL
mlogit$aut2.x <- NULL
mlogit$aut1.y <- NULL
mlogit$aut2.y <- NULL
mlogit$autname1.y <- NULL
mlogit$autname2.y <- NULL

#get df of all authors and list of publications

data_full <- read.csv("funding_new.csv")
data_sub <- select(data_full, MA, A1, A2, A3, A4, A5, A6, A7) #list of all authors
data_paper <- select(data_full, Index, Paper, Citations, Year) #list of all papers
authors <- c(unique(data_full$MA), unique(data_full$A1), unique(data_full$A2), unique(data_full$A3), unique(data_full$A4), unique(data_full$A5), 
             unique(data_full$A6), unique(data_full$A7))

authors <- unique(authors)
authors <- authors[authors != ""] #list of all authors

list_authors<-list()

for (i in 1:length(authors)){
  
  a <- which(data_sub == authors[i], arr.ind = TRUE)[,1]
  df1 <- data_paper[data_paper$Index %in% a,] #all papers by author
  df2 <- data.frame(name=authors[i], papers=nrow(df1), first_paper=min(df1$Year, na.rm=TRUE), last_paper=max(df1$Year, na.rm=TRUE)) 
  
  list_authors[[i]] <- df2
  
}

df2 <- do.call(rbind.data.frame, list_authors) #list of all authors, number of papers, first and last paper

#get author-wise number of publications

data_df <- select(data_full, Paper, MA, A1, A2, A3, A4, A5, A6, A7) #list of all authors

data_df[data_df == ""] <- NA 

data_papers <- matrix(NA, nrow = nrow(data_df), ncol = ncol(data_df)-1)

for (i in 2:ncol(data_df)){
  for (j in 1:nrow(data_df)){
    
    rc <- which(df2 == data_df[j,i], arr.ind = TRUE)
    
    data_papers[j,i-1] <- ifelse(dim(rc)[1] > 0, df2[rc[1,1],rc[1,2]+1], NA)
    
  }
}

data_firstpub <- matrix(NA, nrow = nrow(data_df), ncol = ncol(data_df)-1)

for (i in 2:ncol(data_df)){
  for (j in 1:nrow(data_df)){
    
    rc <- which(df2 == data_df[j,i], arr.ind = TRUE)
    
    data_firstpub[j,i-1] <- ifelse(dim(rc)[1] > 0, df2[rc[1,1],rc[1,2]+2], NA) #gets the first publication year for all authors
    
  }
}

data_new <- as.data.frame(cbind(data_full$Year, data_papers, data_firstpub))

data_new <- data_new %>%
  rename(Year = V1, cit1 = V2, cit2 = V3, cit3 = V4, cit4 = V5, cit5 = V6, cit6 = V7, cit7 = V8, cit8 = V9,
         pub1 = V10, pub2 = V11, pub3 = V12, pub4 = V13, pub5 = V14, pub6 = V15, pub7 = V16, pub8 = V17)

#data_new is a data frame with 8 citation and 8 first year of publication columns
#the number is 8 because 8 is the largest number of authors in a paper
#cit1 to cit8 are the citations of

data_new$aut1 <- ifelse(data_new["pub1"] >= data_new["Year"] & data_new["cit1"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub1"] >= data_new["Year"] & data_new["cit1"] == 1, 2, 1)) #new but only 1 publication

data_new$aut2 <- ifelse(data_new["pub2"] >= data_new["Year"] & data_new["cit2"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub2"] >= data_new["Year"] & data_new["cit2"] == 1, 2, 1))

data_new$aut3 <- ifelse(data_new["pub3"] >= data_new["Year"] & data_new["cit3"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub3"] >= data_new["Year"] & data_new["cit3"] == 1, 2, 1))

data_new$aut4 <- ifelse(data_new["pub4"] >= data_new["Year"] & data_new["cit4"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub4"] >= data_new["Year"] & data_new["cit4"] == 1, 2, 1))

data_new$aut5 <- ifelse(data_new["pub5"] >= data_new["Year"] & data_new["cit5"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub5"] >= data_new["Year"] & data_new["cit5"] == 1, 2, 1))

data_new$aut6 <- ifelse(data_new["pub6"] >= data_new["Year"] & data_new["cit6"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub6"] >= data_new["Year"] & data_new["cit6"] == 1, 2, 1))

data_new$aut7 <- ifelse(data_new["pub7"] >= data_new["Year"] & data_new["cit7"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub7"] >= data_new["Year"] & data_new["cit7"] == 1, 2, 1))

data_new$aut8 <- ifelse(data_new["pub8"] >= data_new["Year"] & data_new["cit8"] > 1, 3, #new and more than 1 publications
                        ifelse(data_new["pub8"] >= data_new["Year"] & data_new["cit8"] == 1, 2, 1))

data_new$dec <- pmax(data_new$aut1, data_new$aut2, data_new$aut3, data_new$aut4, data_new$aut5, data_new$aut6, data_new$aut7,
                     data_new$aut8, na.rm=TRUE)                       

entry_model <- data.frame(ID=c(1:85), entry=data_new$dec)
entry_model <- entry_model %>%
  rename(entry = pub1)

entry_model <- merge(mlogit, entry_model, by.x=c("paper"), by.y=c("ID"))

entry_model$x7 <- (entry_model$x2)^2
entry_model$x8 <- (entry_model$x3)*(entry_model$x4)

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model")))

#author IDs dont match with author name; that's what I'm solving here

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

colnames(entry_model)[colnames(entry_model) == 'autname1.x'] <- 'autname1'
colnames(entry_model)[colnames(entry_model) == 'autname2.x'] <- 'autname2'

entry_model2 <- left_join(entry_model, name_id, by = c('autname1' = 'Name'))
entry_model3 <- left_join(entry_model2, name_id, by = c('autname2' = 'Name'))

entry_model$aut1 <- entry_model3$ID.x
entry_model$aut2 <- entry_model3$ID.y
entry_model$concat2 <- NULL

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "exit_model", "name_id")))

#exit model

data_full <- read.csv("funding_new.csv")

df$is_last1 <- NA
df$is_last2 <- NA

for (i in 1:nrow(df)){
  
  aut1 <- df$autname1[i]
  aut2 <- df$autname2[i]
  
  a1 <- which(data_full[,c("MA", "A1", "A2", "A3", "A4", "A5", "A6", "A7")] == aut1, arr.ind = TRUE)
  author_sub1 <- data_full[data_full$Index %in% a1[,1],]
  
  aut1 <- which(author_sub1[,c("Year")] == max(author_sub1$Year, na.rm=TRUE), arr.ind = TRUE) #gets index number of last paper by aut1
  aut1 <- ifelse(length(aut1)>1, aut1[length(aut1)], aut1)
  
  a2 <- which(data_full[,c("MA", "A1", "A2", "A3", "A4", "A5", "A6", "A7")] == aut2, arr.ind = TRUE)
  author_sub2 <- data_full[data_full$Index %in% a2[,1],]
  
  aut2 <- which(author_sub2[,c("Year")] == max(author_sub2$Year, na.rm=TRUE), arr.ind = TRUE) #gets index number of last paper by aut2
  aut2 <- ifelse(length(aut2)>1, aut2[length(aut2)], aut2)
  
  df$is_last1[i] <- ifelse(df$paper[i]==author_sub1$Index[aut1], 1, 0)
  df$is_last2[i] <- ifelse(df$paper[i]==author_sub2$Index[aut2], 1, 0)
  
}

df$exit <- ifelse(df$is_last1==1 | df$is_last2==1, 1, 0)

entry_model$concat2 <- paste(entry_model$paper, entry_model$concat, sep=" ")
df$concat2 <- paste(df$paper, df$concat, sep=" ")

exit_model <- merge(df, entry_model, by.x=c("concat2"), by.y=c("concat2"))
exit_model$concat2 <- NULL
exit_model$concat.y <- NULL
exit_model$paper.y <- NULL
exit_model$year.y <- NULL
exit_model <- exit_model %>%
  rename(paper = paper.x, year=year.x, concat=concat.x)

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "exit_model")))

#author IDs dont match with author name; that's what I'm solving here

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

exit_model$aut1.x <- NULL
exit_model$aut2.x <- NULL
exit_model$autname1.y <- NULL
exit_model$autname2.y <- NULL

colnames(exit_model)[colnames(exit_model) == 'autname1.x'] <- 'autname1'
colnames(exit_model)[colnames(exit_model) == 'autname2.x'] <- 'autname2'
colnames(exit_model)[colnames(exit_model) == 'aut1.y'] <- 'aut1'
colnames(exit_model)[colnames(exit_model) == 'aut2.y'] <- 'aut2'

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "exit_model", "name_id", "authors")))

##################### REGRESSIONS ######################

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

output <- matrix(ncol=nrow(prod_model), nrow=1000)

for(i in 1:nrow(prod_model)){
  set.seed(i)
  output[,i] = rpois(1000, lambda=exp(predict(m2))[i])
}

output <- rowSums(output)/(nrow(df)/nrow(name_id))
hist(output) #number of publications

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
  pred$entry <- ifelse(pred$`3` > pred$random, 1, 0)
  new_added[i] <- round(length(which(pred$entry == 1))/(nrow(pred)/72))
}

hist(new_added)

#which authors get new authors

entry_model <- cbind(entry_model, pred)
entry_model$unif <- runif(nrow(pred), 0, 1)
entry_model$adding <- ifelse(entry_model$`3` > entry_model$unif, 1, 0)
entry_model$diff <- entry_model$`3`-entry_model$unif

summary(entry_model$adding)

aut_entry <- matrix(NA, ncol=3, nrow=length(authors))

for (i in 1:length(authors)){
  
  sub <- subset(entry_model, adding==1)
  sub1 <- subset(entry_model, autname1==authors[i])
  sub2 <- subset(entry_model, autname2==authors[i])
  sub3 <- rbind(sub1, sub2)
  aut_entry[i, 1]=as.numeric(i)
  aut_entry[i, 2]=authors[i]
  aut_entry[i, 3]=as.numeric(nrow(sub3))
  
}

aut_entry <- data.frame(aut_entry)
aut_entry$X1 <- as.numeric(aut_entry$X1)
aut_entry$X3 <- as.numeric(aut_entry$X3)
#aut_entry$X4 <- round((aut_entry$X3/sum(aut_entry$X3))*nrow(sub), 0)
aut_entry$X4 <- round(((aut_entry$X3/sum(aut_entry$X3))*nrow(sub))/2, 0)

##################### LOGIT ####################

exit_model$x7 <- (exit_model$x2)^2
exit_model$x8 <- (exit_model$x3)*(exit_model$x4)

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

estimate <- as.data.frame(estimate)
estimate$ID <- c(1:72)

exit_model2 <- left_join(exit_model, estimate, by = c('aut1' = 'ID'))
exit_model3 <- left_join(exit_model, estimate, by = c('aut2' = 'ID'))

exit_model$prob1 <- exit_model2$V1
exit_model$prob2 <- exit_model3$V1

exit_model$single_aut1 <- ifelse(exit_model$prob1>exit_model$prob2, 1, 2)

exit_model$unif <- pred$random <- runif(nrow(pred), 0, 1)
exit_model$breaking <- ifelse(exit_model$pred > exit_model$unif, 1, 0)
exit_model$who_exits <- exit_model$single_aut1*exit_model$breaking

new_exit <- matrix(NA,1000,1)

for (i in 1:1000){
  pred$random <- runif(nrow(pred), 0, 1)
  pred$breaking <- ifelse(pred$`round(fitted(mod2), 2)` > pred$random, 1, 0)
  new_exit[i] <- round(length(which(pred$breaking == 1))/(nrow(pred)/72))
}

hist(new_exit)

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
plot(network, layout=igraph::layout.sphere, main="sphere")
plot(network, layout=igraph::layout.random, main="random", vertex.size=10)
plot(network, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=10)

#after add but without connection

rest <- c((nrow(a)+1):(nrow(a)+round(mean(new_added))))
network2 <- add.vertices(network, length(rest), attr = list(name = rest))
plot(network2, layout=igraph::layout.sphere, main="sphere")

#after add and connection

list_est <- list()

for (i in 1:length(authors)){
  
  num_to_add=sample(c(73:116), replace=FALSE, size=aut_entry$X4[i])
  df <- data.frame(V1=c(rep(i,times=aut_entry$X4[i])), V2=num_to_add)
  list_est[[i]] <- df
  
}

list_est <- do.call(rbind.data.frame, list_est)
list_est$papers_tog <- 1

perm2 <- rbind(perm, list_est)

a2 <- table(lapply(perm2[-3], factor, levels = sort(unique(unlist(perm2[-3])))))
a2[lower.tri(a2)] <- t(a2)[lower.tri(a2)]
a2 <- matrix(a2, ncol = ncol(a2), dimnames = dimnames(a2))

network2 <- igraph::graph_from_adjacency_matrix(a2, mode='undirected', diag=F)
plot(network2, layout=igraph::layout.sphere, main="sphere")
plot(network2, layout=igraph::layout.random, main="random", vertex.size=10)
plot(network2, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=7.5)

#after exit

after_exit <- subset(exit_model, breaking==0)
unique_id <- paste(after_exit$aut1, after_exit$aut2)
id <- data.frame(unique_id=unique_id)
perm$unique_id <- paste(perm$V1, perm$V2)

perm_after_exit <- intersect(perm$unique_id, id$unique_id)
perm_after_exit <- data.frame(V1=perm_after_exit)
perm_after_exit <- perm_after_exit %>% separate(V1, c('aut1', 'aut2'))
perm_after_exit$aut1 <- as.numeric(perm_after_exit$aut1)
perm_after_exit$aut2 <- as.numeric(perm_after_exit$aut2)

a4 <- table(lapply(perm_after_exit, factor, levels = sort(unique(unlist(perm_after_exit)))))
a4[lower.tri(a4)] <- t(a4)[lower.tri(a4)]
a4 <- matrix(a4, ncol = ncol(a4), dimnames = dimnames(a4))
network4 <- igraph::graph_from_adjacency_matrix(a4, mode='undirected', diag=F)
plot(network4, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=10)

as_data_frame(network2, what = "edges")



after_exit <- subset(exit_model, breaking==1)
unique_id <- paste(after_exit$aut1, after_exit$aut2)
id <- data.frame(unique_id=unique_id)
perm2$unique_id <- paste(perm2$V1, perm2$V2)

perm_after_exit <- perm2[!(perm2$unique_id %in% id$unique_id),]
#perm_after_exit <- intersect(perm2$unique_id, id$unique_id)
perm_after_exit <- data.frame(V1=perm_after_exit)
perm_after_exit %>% rename(V1 = V1.V1, V2 = V1.V2, papers_tog = V1.papers_tog, unique_id = V1.unique_id) -> perm_after_exit
perm_after_exit <- perm_after_exit %>% separate(V1, c('aut1', 'aut2'))
perm_after_exit$aut1 <- as.numeric(perm_after_exit$aut1)
perm_after_exit$aut2 <- as.numeric(perm_after_exit$V2)
perm_after_exit$V2 <- NULL
perm_after_exit$papers_tog <- NULL
perm_after_exit$unique_id <- NULL

a3 <- table(lapply(perm_after_exit, factor, levels = sort(unique(unlist(perm_after_exit)))))
a3[lower.tri(a3)] <- t(a3)[lower.tri(a3)]
a3 <- matrix(a3, ncol = ncol(a3), dimnames = dimnames(a3))
network3 <- igraph::graph_from_adjacency_matrix(a3, mode='undirected', diag=F)
plot(network3, layout=igraph::layout.fruchterman.reingold, main="fruchterman.reingold", vertex.size=5)









a <- which(data_full[,c("MA", "A1", "A2", "A3", "A4", "A5", "A6", "A7")] == "Sinquefield, S. A.", arr.ind = TRUE)
author_sub <- data_full[data_full$Index %in% a,]

aut1 <- which(author_sub[,c("Year")] == max(author_sub$Year, na.rm=TRUE), arr.ind = TRUE)










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







