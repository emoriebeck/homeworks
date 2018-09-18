#clear workspace
rm(list=ls(all=TRUE)) 
#clear console
cat("\014")

library(ggplot2)
library(gridExtra)
set.seed(5011)

small_trial_n=2
large_trial_n=18
p_success=.05
chances_per_trial=5
number_of_subjects=1000

#the baseline trials
baseline<-replicate(number_of_subjects,
                    max(rbinom(small_trial_n,chances_per_trial,p_success)))

#the facilitation trials
facilitation<-replicate(number_of_subjects,
                   max(rbinom(large_trial_n,chances_per_trial,p_success)))

#combine into data frame
d<-data.frame(b=baseline,
              f=facilitation)
#inspect the data frame
print(head(d))

#means different?
print(apply(d, 2, mean))

#a logical vector to find subjects with zero as their baseline score
baseline0<-baseline==0

#means different?
print(apply(d[baseline0,], 2, mean))

#t test on entire sample
print(with(d,t.test(b,f,paired=F)))

#t test on sample restricted to subjects with 0 scores on the baseline task
print(with(d, t.test(b[baseline0],f[baseline0],paired=F)))


#visualize scores
p1<- ggplot() +
  geom_histogram(data=d,aes(x=b),alpha=.2,color='red')+
  geom_histogram(data=d,aes(x=f+.2),alpha=.2,color='blue')+
  xlab('b-red and f-blue') + 
  theme_classic()

p2<-ggplot() +
  geom_histogram(data=d[baseline0,],aes(x=b),alpha=.2,color='red')+
  geom_histogram(data=d[baseline0,],aes(x=f+.2),alpha=.2,color='blue')+
  xlab('b-red and f-blue') + 
  theme_classic()

#compare distributions with and without baseline selection
gridExtra::grid.arrange(p1,p2,ncol=2)







###################################
############### Q#1 ###############
###################################

# We know that the t-test shows spurious significance because the probability 
# of success is constant across baseline and facilitation trials.  Is the 
# t-value larger or smaller for the test that only considers subjects with 
# 0 baseline scores?  List the t-values and explain why this occurs.

# The t-value for subjects with only 0 scores (t(592) = -58.63, p < .001) is larger
# than the t-value for all subjects (t(999) = -34.15, p < .001). This is not surprising 
# because when we conduct the test for paired mean differences using only those subjects
# who failed at baseline, we are essentially ensuring that subjects will have a mean 
# difference because there are enough trials (18 * 5) almost all subjects will have at 
# least one block of 5 trials where the mean difference will at least 1 and many will 
# have trials where they have 2 or more successes.

###################################
############### Q#2 ###############
###################################

# Temporarily alter your script to answer the following question.  What happens 
# if we consider the minimum scores in baseline and facilitation trials for all 
# of the subjects (the first t test)?  What would Cardinal et al. be forced to 
# conclude about facilitation?  You should see that the second t.test, the one 
# that only considers subjects with 0 scores on baseline trials, completely fails.  
# What happened…why did the test completely fail?

# The t-value for all subjects (t(999) = 6.45, p < .001) is lower than when using 
# the max. The t-test cannot be conducted for only those individuals with 0 baseline
# scores. A few things are notable here. First, the sign of the t-value has reversed -- 
# people were more successful at baseline than at test. Second, the reason that the 
# t-test cannot be conducted for the individuals with 0 scores at baseline is because 
# there is no variability in their responses during the test trials (there is duirng 
# baseline). This occurs because the probability that people get a 0 on at least one of
# 18 sets of 5 trials approaches 1, while the probability that people get a 0 on at 
# least oen of 2 sets of 5 trials is lower. Thus, some individuals showed facilitation 
# at baseline using the minimum, but no individuals showed faciliation during the test
# trials. The t-value for subjects with only 0 scores (t(592) = -58.63, p < .001) is larger.

###################################
############### Q#3 ###############
###################################

# Temporarily alter your script to answer the following question.  Now instead of 
# maximums or minimums, consider the mean of baseline and facilitation trials.  Does 
# this yield a valid statistical conclusion and what is the result in the first t.test?  
# How about the second t.test…are the means different, and if so, what happened?  
# Do these findings mean the t.test is somehow broken?

# The mean value for all subjects was .23 at baseline and .25 at test, while the mean of
# those who had 0 scores at baseline was (obviously) 0 at baseline and .25 at test. 
# The t-value for all subjects (t(999) = -1.39, p = .16) is smaller than the t-value of
# only those subjects who had a 0 score at baseline (t(612) = -52.57, p < .001), but the
# conclusion of the t-test comparing change across all subjects is correct. On average, 
# there is no average difference between test and facilitation trials. The t-test is
# not broken. The differences in magnitudes of the two t-tests reflects, at best, 
# researcher degrees of freedom, and at worst, p-hacking. By restricting the results to
# only those individuals who scored 0 at baseline, we are capitalizing on chance -- it is
# more likely to have an average score of 0 across 2 sets of 5 trials when the probability
# of success is very low than to have mean score of 0 across 18 sets of 5 trials. Keeping
# only those people who had 0's at baseline esstentially capitalizes on that chance

###################################
############### Q#4 ###############
###################################

# Temporarily alter your script to answer the following question.  The current 
# simulation uses paired t tests to match the design of Cardinal et al.  Do the 
# results of the original simulation (using maxima) change much if instead we use 
# the between groups version of the t test?  Why or why not?  

# The results of an independent groups t-test is just as misleading. Across all subjects 
# the mean of the max across the facilitation trials (M = 1.36) was higher than the mean
# of the max across the baseline trials (M = .43, t(1994.7) = -37.02, p < .001). Across 
# subjects with max baseline scores of 0, the mean of the max across the facilitation 
# trials (M = 1.36) was higher than the meanof the max across the baseline trials (M = .43, 
# t(1994.7) = -37.02, p < .001). In other words, the results change little when we use 
# a between-person t-test rather than a within-person t-test. This occurs because relative
# to the mean differences, the standard deviation of the observed baseline and test trials 
# is small, which will further exaggerate the differences. 


###################################
############### Q#5 ###############
###################################
# At the top of your script you’ll see the function/command set.seed(5011).  
# What does this do and why is this useful when running Monte Carlo simulations?

# This occurs because random numbers don't "behave." We can't expect the same results
# given a certain number of "random" draws. However, if we set the seed, we can reproduce
# results of that pseudo random draw to get the same results. This is useful when doing 
# simulations so that you can check your results (and others) can check your results 
# under different conditions.

# Note however, that random number generation is a dream, not reality. At best, our computers can provide
# pseudo-random numbers. 