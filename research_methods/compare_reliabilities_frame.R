rm(list=ls(all=TRUE))
#clear console
cat("\014")

library(psych)
set.seed(5011)


nfc_now  <- read.table(file='need_for_cog_2018.csv',header=T,sep=',',stringsAsFactors = F)
nfc_then <- read.table(file='need_for_cog_2016.csv',header=T,sep=',',stringsAsFactors = F)


#now transpose so that columns are items and rows are subjectgs
nfc_now  <- t(nfc_now)
nfc_then <- t(nfc_then)

#now reverse code the reverse keyed items
reverse_items            <- c(3,4,5,7,8,9,12,16,17)
nfc_now[,reverse_items]  <- -1 * nfc_now[,reverse_items]
nfc_then[,reverse_items] <- -1 * nfc_then[,reverse_items]


#get cronbach's alpha
nfc_now_alpha  <- alpha(nfc_now,warnings=F)
nfc_then_alpha <- alpha(nfc_then,warnings=F)

#print raw cronbach's alpha to screen
print(nfc_now_alpha$total$raw_alpha)
print(nfc_then_alpha$total$raw_alpha)

#compare then and now nfc summed scores of subjects
nfc_now_scores<-rowSums(nfc_now)
nfc_then_scores<-rowSums(nfc_then)

print(t.test(nfc_now_scores,nfc_then_scores,paired=F,var.equal = T))
observed_diff<-mean(nfc_now_scores)-mean(nfc_then_scores)

#permutation test in case t-test is faulty
#created stacked data frame with group labels
now_data<-data.frame(scores=nfc_now_scores,group='now')
then_data<-data.frame(scores=nfc_then_scores,group='then')
permdata<-rbind(now_data,then_data)

replications<-5000
output<-NULL

for(i in 1:replications){
  #create a shuffled labels variable
  permdata$shuffle<-sample(permdata$group,replace=F)
  
  output<-c(output,
            #subtract the mean of one group's scores from the mean of the other group's scores
            mean(subset(permdata,shuffle=='now')$scores) - 
              mean(subset(permdata,shuffle=='then')$scores))
} # end of i loop

hist(output,breaks=50)
abline(v=observed_diff,lwd=3,col='blue')
abline(v=-1*observed_diff,lwd=3,col='blue')

#now calculate proportion of permutation null distribution outcomes falling outside of abs value of observed score
as_or_more_count<- sum(abs(output)>abs(observed_diff))

perm_p<-as_or_more_count/replications
print(paste('permutation_p',perm_p))

###################################
############### Q#1 ###############
###################################

# Using the saved information from the alpha functions, do the classes have overlapping 
# confidence intervals for the raw alpha coefficient?
# The alpha coefficient was .9 (95% CI [.84, .95]) for the previous class and 
# .81 (95% CI [.67, .91]), so the confidence intervals do overlap.

###################################
############### Q#2 ###############
###################################

# How do the classes compare with the test manual for the brief NFC scale?  Are the 
# confidence intervals overlapping the value printed in the test manual?
# The observed alpha coefficient in the test manual was .86. Both the confidence intervals
# of the previous and current classes overlap with this value.

###################################
############### Q#3 ###############
###################################

# Q#3 The t-test gives a different p-value than the permutation test.  What is the difference 
# in p-values? Which p-value do you think is the more accurate and why?  If you wanted to 
# increase the accuracy of the permutation p-value what would you do?

# The p-value for the t-test was .67, wile the p-value of the permutation test was .66. The 
# p-value of the permutation test is likely more accurate. In general, the p-values of 
# permutation tests tend to be more accurate than observed p-values from inferential tests
# because the permutation test is robust to violations of normality in mean differences that
# the t-test is not. We can look at the distirbution of observed differences from the permutation
# to determine if there was a violation. In this case, it does not appear there was one. If you
# if you wanted to increase the accuracy of the p-value, you could increase the number of 
# permutation samples.







