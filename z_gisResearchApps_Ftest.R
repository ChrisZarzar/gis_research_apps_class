#This script will be used to run the F-test between the variables for zip codes with and without cases
#I am starting this off the same as HW 7 from statistics class. This runs the
#classic t-test of whether means are equal. I want to read more into the
#difference of a t-test and a z-test because I may also want to do a Z test like
#in HW 8. I could also look at the confidence intervals using the information
#from HW 9.

#Now I will test whether the variance of the hail reports and the wind reports are equal.
#I will need to use the f test for this
#Hail reports in the hail data set are column 3
#Wind reports in the wind dataset are column 5
#Ho: The variance of hail reports in the hail dataset and the wind reports in the wind dataset are equal
#Ha: The variance of hail reports in the hail dataset and the wind reports in the wind dataset are not equal
#I may just bring this all into one script with the F test. 

print("F-test for bird slopes in 2002")
#set these first two variables to changes what datasets will be calculated
noCase.data <- slope.02bnoCase.data
case.data <- slope.02bcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 

#PRINT OUT THE ACTUAL PVALUES CALCULATED
#TURN THIS NEXT LINE INTO A SCRIPTED COMPARISON WITH AN IF STATEMENT 
#Because we are testing at a p= and we get p=, we cannot reject the null hypothesis

print("F-test for bird slopes in 2003")
noCase.data <- slope.03bnoCase.data
case.data <- slope.03bcase.data


var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 


print("F-test for bird aspect in 2002")
noCase.data <- aspect.02bnoCase.data
case.data <- aspect.02bcase.data


var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 


print("f-test for bird aspect in 2003")
noCase.data <- aspect.03bnoCase.data
case.data <- aspect.03bcase.data


var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 


print("F-test for bird elev in 2002")
noCase.data <- elev.02bnoCase.data
case.data <- elev.02bcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 


print("F-test for bird elev in 2003")
noCase.data <- elev.03bnoCase.data
case.data <- elev.03bcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



# print("t-test for bird roadDen in 2002")
# noCase.data <- roadDen.02bnoCase.data
# case.data <- roadDen.02bcase
# 
# mean.noCase <- mean(noCase.data[,8])
# mean.case <- mean(case.data[,8])
# var.noCase <- var(noCase.data[,8])
# var.case <- var(case.data[,8])
# n.noCase <- length(noCase.data[,8])
# n.case <- length(case.data[,8])
# 
# #Now we set up the test
# t.stat <- (mean.noCase-mean.case)/sqrt((var.noCase/n.noCase)+(var.case/n.case))
# t.stat
# pval <- pt(t.stat,(n.noCase+n.case)-1)
# pval
# 
# print("t-test for bird roadDen in 2003")
# noCase.data <- roadDen.03bnoCase.data
# case.data <- roadDen.03bcase
# 
# mean.noCase <- mean(noCase.data[,8])
# mean.case <- mean(case.data[,8])
# var.noCase <- var(noCase.data[,8])
# var.case <- var(case.data[,8])
# n.noCase <- length(noCase.data[,8])
# n.case <- length(case.data[,8])
# 
# #Now we set up the test
# t.stat <- (mean.noCase-mean.case)/sqrt((var.noCase/n.noCase)+(var.case/n.case))
# t.stat
# pval <- pt(t.stat,(n.noCase+n.case)-1)
# pval


print("F-test for human slopes in 2002")
noCase.data <- slope.02hnoCase.data
case.data <- slope.02hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



print("F-test for huamn slopes in 2003")
noCase.data <- slope.03hnoCase.data
case.data <- slope.03hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



print("F-test for human aspect in 2002")
noCase.data <- aspect.02hnoCase.data
case.data <- aspect.02hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



print("F-test for human aspect in 2003")
noCase.data <- aspect.03hnoCase.data
case.data <- aspect.03hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



print("F-test for human elev in 2002")
noCase.data <- elev.02hnoCase.data
case.data <- elev.02hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 


print("F-test for human elev in 2003")
noCase.data <- elev.03hnoCase.data
case.data <- elev.03hcase.data

var.noCase <- var(noCase.data[,8])
var.case <- var(case.data[,8])
n.noCase <- length(noCase.data[,8])
n.case <- length(case.data[,8])

f.stat <- var.noCase/var.case
f.stat
pval <- pf(f.stat,n.noCase,n.case)
pval 



# print("for human roadDen in 2002")
# noCase.data <- roadDen.02hnoCase.data
# case.data <- roadDen.02hcase
# 
# mean.noCase <- mean(noCase.data[,8])
# mean.case <- mean(case.data[,8])
# var.noCase <- var(noCase.data[,8])
# var.case <- var(case.data[,8])
# n.noCase <- length(noCase.data[,8])
# n.case <- length(case.data[,8])
# 
# #Now we set up the test
# t.stat <- (mean.noCase-mean.case)/sqrt((var.noCase/n.noCase)+(var.case/n.case))
# t.stat
# pval <- pt(t.stat,(n.noCase+n.case)-1)
# pval

# print("t-test for human roadDen in 2003")
# noCase.data <- roadDen.03hnoCase.data
# case.data <- roadDen.03hcase
# 
# mean.noCase <- mean(noCase.data[,8])
# mean.case <- mean(case.data[,8])
# var.noCase <- var(noCase.data[,8])
# var.case <- var(case.data[,8])
# n.noCase <- length(noCase.data[,8])
# n.case <- length(case.data[,8])
# 
# #Now we set up the test
# t.stat <- (mean.noCase-mean.case)/sqrt((var.noCase/n.noCase)+(var.case/n.case))
# t.stat
# pval <- pt(t.stat,(n.noCase+n.case)-1)
# pval

#PRINT OUT THE ACTUAL PVALUES CALCULATED
#TURN THIS NEXT LINE INTO A SCRIPTED COMPARISON WITH AN IF STATEMENT 
#Because we are testing at a p= and we get p=, we cannot reject the null hypothesis 




#Because we are testing at a p=0.01 and we get p=0.0096, we can reject the null hypothesis

# 
# #Now I need to determine if these follow a gamma distribution using the chisquared 
# sd.wind <- sd(wind.data[,2])
# 
# alpha.wind <- mean.wind^2/sd.wind^2
# 
# beta.wind <- sd.wind^2/mean.wind
# rate.wind <- 1/beta.wind
# 
# chi.square(wind.data[,2],dist.name="gamma",n.bins=6,dist.params=c(alpha.wind,rate.wind))
# 
# #Now I need to investigate whether the ranking index for the wind data and hail data follow a gamma (or normal) distribution.
# #I will do this by simply plotting the columns and I will save those plots as images in the word document
# hist(wind.data[,2])
# 
# #This does not follow a gamma distribution.
# 
# alpha.hail <- mean.hail^2/sd.hail^2
# 
# beta.hail <- sd.hail^2/mean.hail
# rate.hail <- 1/beta.hail
# 
# chi.square(hail.data[,2],dist.name="gamma",n.bins=6,dist.params=c(alpha.hail,rate.hail))
# hist(hail.data[,1])
# #This does not follow a gamma distribution. 
# 
# #Each of these test were developed with the assumption that the data follows a
# #gamma distribution. It must follow a gamma distribution for these hypotheses
# #test to be relavent. They do not follow a gammma distribtion and the results
# #are, therefore, not valid for a proper hypothesis test. These data will require
# #non parametric methods for the hypothesis testing.
