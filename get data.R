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

#data <- final_df

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

#perm has 2556 (72*71/2) possible combinations, but not all of them are available in our dataset. For example, in perm you
#may have 1 with 72, but we may not have a paper with author 1 and author 72 as coauthor

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

rm(list=setdiff(ls(), c("prod_model", "data")))

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

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "data")))

colnames(df) <- c("aut1", "aut2", "paper", "year", "autname1", "autname2")

df$concat <- paste(df$autname1, df$autname2, sep=" ")

mlogit <- merge(prod_model, df, by.x=c("concat"), by.y=c("concat")) #merge df with the Xs and Ys from first part

#df has 516 rows and after merging with prod_model, we get 407 rows
#compare not_joined with df

not_joined <- anti_join(df,mlogit)
not_joined$concat <- paste(not_joined$autname2, not_joined$autname1)

mlogit2 <- merge(prod_model, not_joined, by.x=c("concat"), by.y=c("concat"))

mlogit <- rbind(mlogit, mlogit2)

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

entry_model <- data.frame(ID=c(1:nrow(data)), entry=data_new$dec)
entry_model <- entry_model %>%
  rename(entry = pub1)

entry_model <- merge(mlogit, entry_model, by.x=c("paper"), by.y=c("ID"))

entry_model$x7 <- (entry_model$x2)^2
entry_model$x8 <- (entry_model$x3)*(entry_model$x4)

rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "data")))

#author IDs dont match with author name; that's what I'm solving here for entry_model

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

#author IDs dont match with author name; that's what I'm solving here for df

#colnames(df)[colnames(df) == 'autname1.x'] <- 'autname1'
#colnames(entry_model)[colnames(entry_model) == 'autname2.x'] <- 'autname2'

df2 <- left_join(df, name_id, by = c('autname1' = 'Name'))
df3 <- left_join(df2, name_id, by = c('autname2' = 'Name'))

df$aut1 <- df3$ID.x
df$aut2 <- df3$ID.y
df$concat2 <- NULL


rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "exit_model", "name_id", "data")))

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
  rename(paper = paper.x, year=year.x, concat=concat.x, autname1=autname1.x, autname2=autname2.x, aut1=aut1.y, aut2=aut2.y)

exit_model$aut1.x <- NULL
exit_model$aut2.x <- NULL
exit_model$autname1.y <- NULL
exit_model$autname2.y <- NULL


#df has 516 rows and after merging with prod_model, we get 407 rows
#compare not_joined with df

not_joined <- anti_join(df,exit_model)
not_joined$concat2 <- paste(not_joined$paper, not_joined$autname2, not_joined$autname1)

exit_model2 <- merge(not_joined, entry_model, by.x=c("concat2"), by.y=c("concat2"))

exit_model2$concat2 <- NULL
exit_model2$concat.y <- NULL
exit_model2$paper.y <- NULL
exit_model2$year.y <- NULL
exit_model2 <- exit_model2 %>%
  rename(paper = paper.x, year=year.x, concat=concat.x, autname1=autname1.x, autname2=autname2.x, aut1=aut1.y, aut2=aut2.y)

exit_model2$aut1.x <- NULL
exit_model2$aut2.x <- NULL
exit_model2$autname1.y <- NULL
exit_model2$autname2.y <- NULL

exit_model <- rbind(exit_model, exit_model2)


#author IDs dont match with author name; that's what I'm solving here for entry_model

data <- read.csv("funding_new.csv")
authors <- c(unique(data$MA), unique(data$A1), unique(data$A2), unique(data$A3), unique(data$A4), 
             unique(data$A5), unique(data$A6), unique(data$A7))
authors <- unique(authors)
authors <- authors[authors != ""] #gets list of unique authors
x <- c(1:length(authors))

name_id <- data.frame(ID=x, Name=authors)

#colnames(exit_model)[colnames(entry_model) == 'autname1.x'] <- 'autname1'
#colnames(entry_model)[colnames(entry_model) == 'autname2.x'] <- 'autname2'

exit_model2 <- left_join(exit_model, name_id, by = c('autname1' = 'Name'))
exit_model3 <- left_join(exit_model2, name_id, by = c('autname2' = 'Name'))

exit_model$aut1 <- exit_model3$ID.x
exit_model$aut2 <- exit_model3$ID.y
exit_model$concat2 <- NULL


rm(list=setdiff(ls(), c("prod_model", "df", "mlogit", "entry_model", "exit_model", "data")))






