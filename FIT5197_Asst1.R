# FIT9157 Assessment 1
# Student: Dong Lei Qian
# ID: 29115566

# All detailed explanataions can be found in  FIT5197_Asst1_s29115566.dox

# Exercise 1a: What proportion, q, of people in the population have condition X?
q = (1 - pnorm(1.959964)) * 2
q
# 0.05

# Exercise 1b: What proportion, p, of patients with condition X will test positive?
p = ppois(4,lambda = 3.0895398)
p
# 0.8

# Exercise 1c: What proportion, r, of patients without condition X will test negative?
r = 0.92831778 ** 3
r
# 0.8

# Exercise 1d: If a patient tests positive, what is the probability that the patient has condition X?
p * q / (p * q + (1-r) * (1-q))
# 0.1739131

# Exercise 1e: If a patient tests positive on a first test and then tests positive again on a second test, what is the
# probability that the patient has condition X?
p ** 2 * q / (p ** 2 * q + (1-r)  ** 2 * (1-q))
# 0.4571429

# Exercise 1f: If a patient tests positive on a first test, then tests positive again on a second test and then tests 
# positive again on a third test, what is the probability that the patient has condition X?
p ** 3 * q / (p ** 3 * q + (1-r) ** 3 * (1-q))
# 0.7710844

# Exercise 1g (bonus) : If a patient tests positive on a first test, then tests positive again on a second test and then
# tests positive again on a third test and tests positive on n consecutive tests, then, as a function of n, what is the
# probability that the patient has condition X?
# See FIT5197_Asst1_s29115566.dox

# Exercise 2a: What is the mean of Att1 (or Att 1 or ee), and what is the s.d. of Att1?
q2data = read.csv('Asst1Qu2.csv')
mean(q2data$Att1)
# 0.515
sd(q2data$Att1)
# 0.5010291

# Exercise 2b: What is the mean of Att2 (or Att 2 or ff), and what is the s.d. of Att2?
mean(q2data$Att2)
# 0.17
sd(q2data$Att2)
# 0.3765754

# Exercise 2c: What is the mean of Att3 (or Att 3 or gg), and what is the s.d. of Att3?
mean(q2data$Att3)
# 0.515
sd(q2data$Att3)
# 0.5010291

# Exercise 2d: What is the number of observations of each of the above attributes?
str(q2data)
# 'data.frame':	200 obs. of  5 variables:

# Exercise 2e: What is the correlation between Att1 and Att2? Does there appear to be a dependence between
# Att1 and Att2 – if so, please elaborate.
cor(q2data$Att1,q2data$Att2)
# -0.0135832

# Exercise 2f: What is the correlation between Att1 and Att3? Does there appear to be a dependence between
# Att1 and Att3 – if so, please elaborate.
cor(q2data$Att1,q2data$Att3)
# 0.6596937

# Exercise 2g: What is the correlation between Att2 and Att3? Does there appear to be a dependence between
# Att2 and Att3 – if so, please elaborate.
cor(q2data$Att2,q2data$Att3)
# -0.0135832

# Exercise 2h: Use the R command prop.test() to perform one or more hypothesis tests on the proportion (or mean) of each of Att1, Att2 and Att3.
pairwise.prop.test(c(sum(q2data$Att1),sum(q2data$Att2),sum(q2data$Att3)), c(length(q2data$Att1),length(q2data$Att2),length(q2data$Att3)))
#	Pairwise comparisons using Pairwise comparison of proportions 
# data:  c(sum(q2data$Att1), sum(q2data$Att2), sum(q2data$Att3)) out of c(length(q2data$Att1), length(q2data$Att2), length(q2data$Att3)) 
#    1       2      
# 2 2.3e-12 -      
# 3 1       2.3e-12
# P value adjustment method: holm 

# Let's perform a different proportion test, following an example given in Comparison of Two Population Proportions at r-tutor.com
table(q2data$Att1,q2data$Att2)
prop.test(table(q2data$Att1,q2data$Att2))
#	2-sample test for equality of proportions with continuity correction
# data:  table(q2data$Att1, q2data$Att2)
# X-squared = 1.4187e-05, df = 1, p-value = 0.997
# alternative hypothesis: two.sided
# 95 percent confidence interval:
# -0.1244482  0.1040298
# sample estimates:
# prop 1    prop 2 
# 0.8247423 0.8349515 

# Let's repeat the test for Att1 and Att3
table(q2data$Att1,q2data$Att3)
prop.test(table(q2data$Att1,q2data$Att3))
# 2-sample test for equality of proportions with continuity correction
# data:  table(q2data$Att1, q2data$Att3)
# X-squared = 84.418, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 95 percent confidence interval:
# 0.5454547 0.7739328
# sample estimates:
# prop 1    prop 2 
# 0.8247423 0.1650485 

# Finally we can test Att2 and Att3
table(q2data$Att2,q2data$Att3)
prop.test(table(q2data$Att2,q2data$Att3))
#	2-sample test for equality of proportions with continuity correction
# data:  table(q2data$Att2, q2data$Att3)
# X-squared = 1.4187e-05, df = 1, p-value = 0.997
# alternative hypothesis: two.sided
# 95 percent confidence interval:
# -0.2202456  0.1841010
# sample estimates:
# prop 1    prop 2 
# 0.4819277 0.5000000 

# Exercise 2i: Does there appear to be a dependence between Att1, Att2 and Att 3? Possibly or possibly not
# referring to your answers to earlier parts to this question, please elaborate on your answer.
# Attributes have an xor relationship
xor(q2data$Att1, q2data$Att2) == q2data$Att3
# gives all TRUE values
# Doing a cor test
cor.test(xor(q2data$Att1, q2data$Att2) * 1, q2data$Att3)
#	Pearson's product-moment correlation
# data:  xor(q2data$Att1, q2data$Att2) * 1 and q2data$Att3
# t = Inf, df = 198, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 1 1
# sample estimates:
# cor 
# 1 

# Exercise 3
q3data = read.csv('Asst1Qu3.csv')
mean(q3data$Att1)
# 79.89
sd(q3data$Att1)
# 4.059893
min(q3data$Att1)
# 69
max(q3data$Att1)
# 91

# To find outlier, use the ourlier function provided in outlier.r from week3 reading material
checkeOutliers <- function(data){ 
  Q1 <- quantile(data, 0.25, na.rm = TRUE) # first quartile
  Q3 <- quantile(data, 0.75, na.rm = TRUE) # third quartile
  IQR <- IQR(data, na.rm = TRUE) # IQR is the distance between the first and third quartile
  max <- Q3 + (IQR * 1.5) # upper bound is the third quartile plus IQR times a constant 1.5
  min <- Q1 - (IQR * 1.5) # lower bound is the first quartile minus IQR times a constant 1.5                                                   
  index <- which(data < min | data > max) # get indices of all outliers
  return (data[index]) # return outliers 
}
checkeOutliers(q3data$Att1)
# integer(0)
# no outliers

hist(q3data$Att1)

boxplot(q3data$Att1)

# check sd of binomial with 0.8 prob
sqrt(0.8 * 0.2)
# 0.4 which is same as sd of distribution 4 / sqrt(100)

# Let's test if the true mean is 80 using t test
t.test(q3data,mu=80)
#	One Sample t-test
# data:  q3data
# t = -0.27094, df = 99, p-value = 0.787
# alternative hypothesis: true mean is not equal to 80
# 95 percent confidence interval:
# 79.08443 80.69557
# sample estimates:
# mean of x 
# 79.89 

# we can demonstrate by generating a random binomial distribution using probability of 0.8 out of 100 trials
# which gives an expected value of 80(mean of the dataset) and check the histogram and box plot of such a distribution and see if they are  
# similar to the ones we got

x <- rbinom(100,100,0.8)
hist(x)

boxplot(x)

sd(x)
mean(x)
min(x)
max(x)

# Let's do a t test to compare the means
t.test(q3data,x)

# TheKolmogorov Smirnov test is used to test if two distributions are the same, by constructing cumulative functions and compare them,
# because the cumulative function is sensitive to differences in both location and shape of the sample distributions(wikipedia) 
ks.test(q3data,x)

# Exercise 4
q4data = read.csv('Asst1Qu4.csv')

# Exercise 4a: Using some sort of plot as visualisation and/or any other appropriate technique, do the distributions seem uniform, unimodal or something else?
hist(q4data$att1)
hist(q4data$att2)
hist(q4data$att3)
hist(q4data$att4)
hist(q4data$att5)
hist(q4data$att6)
hist(q4data$att7)
hist(q4data$att8)
hist(q4data$att9)
hist(q4data$att10)

# Exercise 4b: Use the techniques available to you to try to model the distribution from which this data came.

# Let's construct 2 different poisson distributions with lambda 4 and 1
x <- rpois(400,lambda=4)
y <- rpois(100, lambda=1)
hist(c(x,y))

# We can perform the kolmogorov Smirnov test again to see if we can reject the two distributions being the same
ks.test(q4data$att1,c(x,y))

# Exercise 4c: For each of the 10 attributes, what is the mean and s.d.?
m1 = mean(q4data$att1)
m1
# 3.494
m2 = mean(q4data$att2)
m2
# 3.6
m3 = mean(q4data$att3)
m3
# 3.516
m4 = mean(q4data$att4)
m4
# 3.522
m5 = mean(q4data$att5)
m5
# 3.562
m6 = mean(q4data$att6)
m6
# 3.602
m7 = mean(q4data$att7)
m7
# 3.5
m8 = mean(q4data$att8)
m8
# 3.704
m9 = mean(q4data$att9)
m9
# 3.604
m10 = mean(q4data$att10)
m10
# 3.666
s1 = sd(q4data$att1)
s1
# 2.450709
s2 = sd(q4data$att2)
s2
# 2.566939
s3 = sd(q4data$att3)
s3
# 2.57663
s4 = sd(q4data$att4)
s4
# 2.424308
s5 = sd(q4data$att5)
s5
# 2.547771
s6 = sd(q4data$att6)
s6
# 2.510019
s7 = sd(q4data$att7)
s7
# 2.507304
s8 = sd(q4data$att8)
s8
# 2.756007
s9 = sd(q4data$att9)
s9
# 2.518704
s10 = sd(q4data$att10)
s10
# 2.605363

# Exercise 4d: Putting all 500 x 10 = 5000 observations together, what is the mean and s.d.?
mtotal = sum(q4data) / 5000
mtotal
# The mean of all observations is 3.577, which is the average of all 10 means from each attribute
mean(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10))
# 3.577
sd(c(q4data$att1, q4data$att2, q4data$att3, q4data$att4, q4data$att5, q4data$att6, q4data$att7, q4data$att8, q4data$att9, q4data$att10))
# s.d. of all observations is 2.546481, this is almost the same as the average of 10 sd of each attribute
(sd(q4data$att1) + sd(q4data$att2) + sd(q4data$att3) + sd(q4data$att4) + sd(q4data$att5) + sd(q4data$att6) + sd(q4data$att7) + sd(q4data$att8) + sd(q4data$att9) + sd(q4data$att10)) / 10
# 2.546378 

# Exercise 4e: How, if at all, would you explain the relation between the answers to 4c and 4d?
# below shows histogram of the mean distribution for 10 means, because of small sample size it is inconclusive to say this is a normal distribution
hist(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10))

# References
# Outliers.R (http://moodle.vle.monash.edu/mod/resource/view.php?id=4159785)
# Kolmogorov Smirnov test (https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)
# Comparison of Two Population Proportions(http://www.r-tutor.com/elementary-statistics/inference-about-two-populations/comparison-two-population-proportions)