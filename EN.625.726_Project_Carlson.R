#EN.625.726 Project
#Cassandra Carlson

#Packages
library(readxl)
library(BART)

#Read in Excel file
data <- read_excel("/Users/cassandracarlson/Documents/2021_FantasyPros_Fantasy_Football_Advanced_Stats_Report_QB.xlsx") #Path will differ
#names(data)

#Variables
qb_rank <- c(data[1])
amt_of_games <- c(data[2])
comp_pass_rates <- c(data[3])
passing_yds <- c(data[4])
sacks <- c(data[5])
dropped <- c(data[6])

#Prepare variables for use
qb_rank = unlist(qb_rank)
amt_of_games = unlist(amt_of_games)
comp_pass_rates = unlist(comp_pass_rates)
passing_yds = unlist(passing_yds)
sacks = unlist(sacks)
dropped = unlist(dropped)

#Put variables into dataframe
df <- data.frame(qb_rank, amt_of_games, comp_pass_rates, passing_yds, sacks, dropped) #Looks at pressure from other players (includes sacks)
#View(df)
#names(df)

y = df$qb_rank
x = df[,2:6]

#Put variables into dataframe
df1 <- data.frame(qb_rank, amt_of_games, comp_pass_rates, passing_yds, dropped) #Doesn't look at pressure from other players (does not include sacks)
#View(df1)
#names(df1)

y1 = df1$qb_rank
x1 = df1[,2:5]

#Set seed
set.seed(2021)

##########################################################################################################
#Looks at pressure from other players (includes sacks)
##########################################################################################################

#Linear Model Fit
linModel = lm(qb_rank~.,df)
print("Here is a summary of the linear model parameters:")
print(summary(linModel))

##########################################################################################################

#BART Model Fit w/ 200 trees in the sum-of-trees model (default)
burn=1000
nd=1000

bartModel = wbart(x, y, nskip=burn, ndpost=nd, printevery=500) #Prints progress every 500th MCMC iteration

##########################################################################################################

#Testing Sigma and Number of Burns 
plot(bartModel$sigma, ylim=c(1.5,10), xlab="MCMC Iteration", ylab="sigma draw", cex=1)
abline(h=summary(linModel)$sigma, col="red", lty=2) #least squares estimates
abline(v = burn, col="green")
title(main="sigma draws, green line at burn in, red line at least squares estimate",cex.main=.8)

##########################################################################################################

#BART Model Fit w/ 20 trees in the sum-of-trees model
burn=1000
nd=1000

bartModel20 = wbart(x, y, nskip=burn, ndpost=nd, ntree=20, printevery=500) #Prints progress every 500th MCMC iteration

##########################################################################################################

#Compare the in-sample fits from the two BART runs
fitmat = cbind(y, bartModel$yhat.train.mean, bartModel20$yhat.train.mean)
colnames(fitmat) = c("y","yhatBART","yhatBART20")
pairs(fitmat)

#Determines which BART model w/ sacks fits best
print(cor(fitmat))

##########################################################################################################

#Variable Selection for BART models

percount = bartModel$varcount/apply(bartModel$varcount, 1, sum)
mvp = apply(percount, 2, mean)

percount20 = bartModel20$varcount/apply(bartModel20$varcount, 1, sum)
mvp20 = apply(percount20, 2, mean)

plot(mvp20, xlab="variable number", ylab="post mean, percent var use", col="blue", type="b")
lines(mvp, type="b", col='red')
legend("topleft", legend=c("bartModel", "bartModel20"), col=c("red", "blue"), lty=c(1, 1))

##########################################################################################################
#Doesn't look at pressure from other players (does not include sacks)
##########################################################################################################

#Linear Model Fit
linModel1 = lm(qb_rank~.,df1)
print("Here is a summary of the linear model parameters:")
print(summary(linModel1))

##########################################################################################################

#BART Model Fit w/ 200 trees in the sum-of-trees model (default)
burn=1000
nd=1000

bartModel_noSacks = wbart(x1, y1, nskip=burn, ndpost=nd, printevery=500) #Prints progress every 500th MCMC iteration

##########################################################################################################

#Testing Sigma and Number of Burns 
plot(bartModel_noSacks$sigma, ylim=c(1.5,10), xlab="MCMC Iteration", ylab="sigma draw", cex=1)
abline(h=summary(linModel1)$sigma, col="red", lty=2) #least squares estimates
abline(v = burn, col="green")
title(main="sigma draws, green line at burn in, red line at least squares estimate",cex.main=.8)

##########################################################################################################

#BART Model Fit w/ 20 trees in the sum-of-trees model
burn=1000
nd=1000

bartModel20_noSacks = wbart(x1, y1, nskip=burn, ndpost=nd, ntree=20, printevery=500) #Prints progress every 500th MCMC iteration

##########################################################################################################

#Compare the in-sample fits from the two BART runs
fitmat1 = cbind(y1, bartModel_noSacks$yhat.train.mean, bartModel20_noSacks$yhat.train.mean)
colnames(fitmat1) = c("y1","yhatBART_noSacks","yhatBART20_noSacks")
pairs(fitmat1)

#Determines which BART model w/o sacks fits best
print(cor(fitmat1))

##########################################################################################################

#Variable Selection for BART models w/o sacks

percount_noSacks = bartModel_noSacks$varcount/apply(bartModel_noSacks$varcount, 1, sum)
mvp_noSacks = apply(percount_noSacks, 2, mean)

percount20_noSacks = bartModel20_noSacks$varcount/apply(bartModel20_noSacks$varcount, 1, sum)
mvp20_noSacks = apply(percount20_noSacks, 2, mean)

plot(mvp20_noSacks, xlab="variable number", ylab="post mean, percent var use", col="blue", type="b")
lines(mvp_noSacks, type="b", col='red')
legend("topleft", legend=c("bartModel_noSacks", "bartModel20_noSacks"), col=c("red", "blue"), lty=c(1, 1))
