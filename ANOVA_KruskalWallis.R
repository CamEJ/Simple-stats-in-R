# Single factor analysis of variance (ANOVA or Kruskal Wallis) 

# for when you are testing if there are differences in your data 
# resulting from ONE treatment (aka factor) with 3 or more levels 
# if there are only two levels, just do a t-test
# in other words; for example does the number
# of flies differ significantly as a function of month? 
# *If you are trying to check for multiple factors (as a function of month and number
# of spiders; then should consider regression and/or correlation

# uses the base function aov()
# which takes form:
aov(NumericVariableName ~ Treatment, data = NameOfDataFrame)

# example; checking if differences in number of bacteria resulting from diff UV 
# exposure times are significant. 

# 1 create (or import) data
# here we'll make some fake data
NumberBacteria = c(987,988,799,233,121,141,878,231,411)  # variable of no of bacteria
Treatment = c("No_UV",	"No_UV",	"No_UV", "UV_5mins",	"UV_5mins",	"UV_5mins",	"UV_1min",	"UV_1min",	"UV_1min") # & associated tmt
MyDataFrame = data.frame(NumberBacteria,Treatment) # make these as two columns of data in MyDataFrame
head(MyDataFrame)
# how it should look:
# NumberBacteria Treatment
# 1            987     No_UV
# 2            988     No_UV
# 3            799     No_UV
# 4            233  UV_5mins
# 5            121  UV_5mins
# 6            141  UV_5mins

# do anova and store results in result.aov
result.aov = aov(NumberBacteria ~ Treatment, data = MyDataFrame)
# Summary of the analysis
summary(resAc.aov)
# if  p value is < 0.01 then we reject the null hypotheses that there is 
# no effect of your treatment upon our counts
# Next, if sig P value, do a multiple comparisons test to see between which
# of the factors is the sig diff. We'll do Tukey.

TukeyHSD(result.aov)
# output should show significant diff between counts
# in UV_5mins vs No_UV treatments 

# amoung the assumptions of an ANOVA is that your data is normally distributed
# often in biological science, our data is not. So the best thing to do is 
# to test for normality before doing stats. There are a number of ways of doing this
# Here's one, using Shapiro–Wilk test:

tapply(NumberBacteria, INDEX=Treatment, FUN=shapiro.test)

# if running above way brings error then try as follows

tapply(X = MyDataFrame$NumberBacteria, INDEX=list(MyDataFrame$Treatment), FUN=shapiro.test)

# the output tests normality of data from each of my factors in Treatment 
# if all p values > 0.01 then data is normal and do ANOVA and Tukey as above
# if one or more p values are <0.05 then not all data is normal and 
# it is better to do a Kruskal Wallis which (I think) is more stringent and 
# doesnt assume normality

kruskal.test(MyDataFrame$NumberBacteria ~ MyDataFrame$Treatment) # run test
#  p-value = 0.05817 # ie shows how important to consider normality as in anova we have sig diff but not here. 

# if p value was <0.01 again we reject the null hypotheses that there is 
# no effect of your treatment upon our counts 
# Next, if sig P value, do a multiple comparisons test to see between which
# here, as we did a kruskal, we'll do a dunn test to see where differences lie

library(dunn.test) # load package

dunn.test(NumberBacteria ~ Treatment, method="bh") # run
          
          # where bh means benjamini hochberg
          # read up and choose method as appropriate 
          
          # . https://cran.r-project.org/doc/contrib/Martinez-RforBiologistv1.1.pdf


