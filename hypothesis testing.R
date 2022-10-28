##-----------------A/B Testing-----------------##

It provides a way to check outcomes of competing scenarios and decide which way to proceed.


Calculate the proportion of late shipments in the sample. That is, 
the mean cases where the late column is "Yes"

late_shipments %>%
  summarize(mean = mean(late == "No"))

The proportion of late shipments is 0.067, or 6.7%.

##----------Calculating a z-score--------------##

Since variables have arbitrary ranges and units, we need to standardize them. 
One standardized value of interest in a hypothesis test is called a z-score. 
To calculate it, we need three numbers: the sample statistic (point estimate), 
the hypothesized statistic, and the standard error of the 
statistic (which we estimate from the bootstrap distribution).

##------------Example--------------------##
Hypothesize that the proportion of late shipments is 6%.
Calculate the standard error. That is, the standard deviation of the bootstrap distribution.
Calculate the z-score.

Answer
# Hypothesize that the proportion is 6%
late_prop_hyp <- 0.06

# Calculate the standard error
std_error <- late_shipments_boot_distn %>%
  summarize(sd(late_prop))%>% pull()

# Find z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp)/std_error

# See the results
z_score

##---------------------Example 2--------------------##

Calculate the z-score of late_prop_samp.
Calculate the p-value for the z-score, assuming a right-tailed test.

# Calculate the z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp)/std_error

# Calculate the p-value
p_value <-pnorm(z_score, lower.tail = F)

# See the result
p_value   


##----------------Calculating confidence intervals-------------------##

If you give a single estimate of a sample statistic, you are bound to be wrong 
by some amount. it's a good idea to state a confidence interval. That is, you 
say "we are 95% 'confident' the proportion of late shipments is 
between A and B" (for some value of A and B).

# Calculate 95% confidence interval using quantile method
conf_int_quantile <- late_shipments_boot_distn %>%
  summarize(lower = quantile(prop_late_shipments, 0.025),
            upper = quantile(prop_late_shipments, 0.975))



##-----------------Hypothesis Testing Workflow----------------##

1. Identify the population parameter being hypothesized
2. Specify the null and alternative hypothesis
3. Determine (standardized) test statistic and the corresponding null distribution
4. Conduct hypothesis testing in R
5. Measure evidence against the null hypothesis
6. Make a decision comparing evdidence to significance level
7. Interpret the results in the context of the original problem


The normal distribution is essentially a t-distribution with infinite 
degrees of freedom.As you increase the degrees of freedom, the t-distribution 
PDF and CDF curves get closer to those of a normal distribution.

##------------p-value of the t-distribution----------------##

# Calculate the degrees of freedom
degrees_of_freedom <- (n_no + n_yes )-2

# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom)

# See the result
p_value


Before you start running hypothesis tests, it's a great idea to 
perform some exploratory data analysis. That is, calculating 
summary statistics and visualizing distributions.