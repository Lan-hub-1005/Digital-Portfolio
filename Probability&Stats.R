
# ------------------------------------- < Summary of the Dataset > -----------------------

# Summary of the dataset
math <- read.csv2('student-mat.csv')
View(math)
dim(math)

# Display the names of the variables/features in the dataset
colnames(math)
head(math)
str(math)
summary(math)
class(math)

# Transform the categorical variables into factors
names <- c(1,2,4:29)
math[,names] <- lapply(math[,names],factor)

summary(math)

# Look at the value of a variable

# Nominal Variable
summary(math$address)
# Ordinal Variable
summary(math$famrel)
# Interval Variable
summary(math$G3)
# Ratio Variable
summary(math$age)



# --------------------------------------- < Missing Values > -------------------------------

# If haven't install the packages
# install.packages('VIM')
# install.packages('tidyverse')

# Load the packages
library(VIM)
library(tidyverse)

# Setting the column names into lower case
colnames(math) <- tolower(colnames(math))

# Visualise the missing data level and pattern

# These are our variables of interest
varsinterest <- c('sex','age','fedu','studytime','famsup','romantic','famrel',
                  'dalc','absences','g1','g2','g3')
# Create a subset of data with just the variables of interest
mathdatasubset <- math[varsinterest]

# Check the missing value
missingCheck <- mathdatasubset[which(mathdatasubset$g1==0 | mathdatasubset$g2==0
                                     | mathdatasubset$g3==0),]
# or we can only check columns for G1, G2, G3
missingCheck2 <- subset(mathdatasubset,g1==0 | g2==0 | g3==0,select = c(g1,g2,g3))

# Change the "0" in the test scores into NAs
mathdatasubset[,c('g2','g3')][mathdatasubset[,c('g2','g3')]==0] <- NA

# Create and inspect patterns of missingness
res<-summary(VIM::aggr(mathdatasubset, sortVar=TRUE))$combinations

#Output the most common combinations
head(res[rev(order(res[,2])),])

#Dropping the rows with missing values.
mathFinalSubset <- na.omit(mathdatasubset)

dim(mathFinalSubset)
View(mathFinalSubset)





# --------------------------------------- < Descriptive Statistics -- This Part's code was generated in R commander > -------------------------------------

# This part done in R Commander.

# Frequency Distribution of "sex"

local({
        .Table <- with(mathFinalSubset, table(sex))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
with(mathFinalSubset, piechart(sex, xlab="", ylab="", main="sex", col=rainbow_hcl(2), 
                               scale="percent"))

# Frequency Distribution of "studytime"

local({
        .Table <- with(mathFinalSubset, table(studytime))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})

with(mathFinalSubset, Barplot(studytime, xlab="studytime", ylab="Frequency", label.bars=TRUE))


# Numeric Summary and Histogram of "g3"

numSummary(mathFinalSubset[,"g3", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), 
           quantiles=c(0,.25,.5,.75,1))

with(mathFinalSubset, Hist(g3, scale="frequency", breaks="Sturges", col="darkgray"))


# Numeric Summary and Histogram of "age"

numSummary(mathFinalSubset[,"age", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), 
           quantiles=c(0,.25,.5,.75,1))

Boxplot( ~ age, data=mathFinalSubset, id=list(method="y"))






# ------------------------------------- < Assess Normality > ----------------------------

# Install all the needed packages (if not installed)
# Specify your packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                              


# Load packages
library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis
library(FSA)

# --- Assessing Normality of "g3" ---

# -- Generate Histogram --

#We will allocate the histogram to a variable to allow use to manipulate it
gg_g3 <- ggplot(mathFinalSubset, aes(x=mathFinalSubset$g3))

#Change the label of the x axis
gg_g3 <- gg_g3 + labs(x="Final Math Score (g3)")

#manage binwidth and colours
gg_g3 <- gg_g3 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg_g3 <- gg_g3 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of g3
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg_g3 <- gg_g3 + stat_function(fun=dnorm, color="red",args=list(mean=mean(mathFinalSubset$g3, na.rm=TRUE), sd=sd(mathFinalSubset$g3, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg_g3


# -- Generate Q-Q Plot --

qqnorm(mathFinalSubset$g3)
qqline(mathFinalSubset$g3, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(mathFinalSubset$g3, basic=F)

# -- Standardised value of Skew and Kurtosis --
g3_skew<-semTools::skew(mathFinalSubset$g3)
g3_kurt <- semTools::kurtosis(mathFinalSubset$g3)

g3_skew[1]/g3_skew[2]  # standardised value for skew
g3_kurt[1]/g3_kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greated than 1.96
zg3<- abs(scale(mathFinalSubset$g3))

FSA::perc(as.numeric(zg3), 1.96, "gt")

# Calculate the percentage of standardised scores that are greated than 1.96
FSA::perc(as.numeric(zg3), 3.29, "gt")



# --- Assessing Normality of "absences" ---

# -- Generate Histogram --

#We will allocate the histogram to a variable to allow use to manipulate it
gg_absences <- ggplot(mathFinalSubset, aes(x=mathFinalSubset$absences))

#Change the label of the x axis
gg_absences <- gg_absences + labs(x="Absences")

#manage binwidth and colours
gg_absences <- gg_absences + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg_absences <- gg_absences + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of absences
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg_absences <- gg_absences + stat_function(fun=dnorm, color="red",args=list(mean=mean(mathFinalSubset$absences, na.rm=TRUE), sd=sd(mathFinalSubset$absences, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg_absences


# -- Generate Q-Q Plot --

qqnorm(mathFinalSubset$absences)
qqline(mathFinalSubset$absences, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(mathFinalSubset$absences, basic=F)

# -- Standardised value of Skew and Kurtosis --
absences_skew<-semTools::skew(mathFinalSubset$absences)
absences_kurt <- semTools::kurtosis(mathFinalSubset$absences)

absences_skew[1]/absences_skew[2]  # standardised value for skew
absences_kurt[1]/absences_kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greated than 1.96
zabsences<- abs(scale(mathFinalSubset$absences))

FSA::perc(as.numeric(zabsences), 1.96, "gt")

# Calculate the percentage of standardised scores that are greated than 1.96
FSA::perc(as.numeric(zabsences), 3.29, "gt")



# --- Assessing Normality of "g1" ---

# -- Generate Histogram --

#We will allocate the histogram to a variable to allow use to manipulate it
gg_g1 <- ggplot(mathFinalSubset, aes(x=mathFinalSubset$g1))

#Change the label of the x axis
gg_g1 <- gg_g1 + labs(x="First Math Score (g1)")

#manage binwidth and colours
gg_g1 <- gg_g1 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg_g1 <- gg_g1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of g1
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg_g1 <- gg_g1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(mathFinalSubset$g1, na.rm=TRUE), sd=sd(mathFinalSubset$g1, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg_g1


# -- Generate Q-Q Plot --

qqnorm(mathFinalSubset$g1)
qqline(mathFinalSubset$g1, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(mathFinalSubset$g1, basic=F)

# -- Standardised value of Skew and Kurtosis --
g1_skew<-semTools::skew(mathFinalSubset$g1)
g1_kurt <- semTools::kurtosis(mathFinalSubset$g1)

g1_skew[1]/g1_skew[2]  # standardised value for skew
g1_kurt[1]/g1_kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greated than 1.96
zg1<- abs(scale(mathFinalSubset$g1))

FSA::perc(as.numeric(zg1), 1.96, "gt")

# Calculate the percentage of standardised scores that are greated than 1.96
FSA::perc(as.numeric(zg1), 3.29, "gt")





# --- Assessing Normality of "g2" ---

# -- Generate Histogram --

#We will allocate the histogram to a variable to allow use to manipulate it
gg_g2 <- ggplot(mathFinalSubset, aes(x=mathFinalSubset$g2))

#Change the label of the x axis
gg_g2 <- gg_g2 + labs(x="Second Period Math Score (g2)")

#manage binwidth and colours
gg_g2 <- gg_g2 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg_g2 <- gg_g2 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of g2
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg_g2 <- gg_g2 + stat_function(fun=dnorm, color="red",args=list(mean=mean(mathFinalSubset$g2, na.rm=TRUE), sd=sd(mathFinalSubset$g2, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg_g2


# -- Generate Q-Q Plot --

qqnorm(mathFinalSubset$g2)
qqline(mathFinalSubset$g2, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(mathFinalSubset$g2, basic=F)

# -- Standardised value of Skew and Kurtosis --
g2_skew<-semTools::skew(mathFinalSubset$g2)
g2_kurt <- semTools::kurtosis(mathFinalSubset$g2)

g2_skew[1]/g2_skew[2]  # standardised value for skew
g2_kurt[1]/g2_kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
zg2<- abs(scale(mathFinalSubset$g2))

FSA::perc(as.numeric(zg2), 1.96, "gt")

# Calculate the percentage of standardised scores that are greated than 1.96
FSA::perc(as.numeric(zg2), 3.29, "gt")







# ------------------------------------- < Correlation > ----------------------------

# install and load packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                              

library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis


# --- G1 and G3 ---

# Scatterplot 
#aes(x,y)
scatter <- ggplot(mathFinalSubset, aes(mathFinalSubset$g1, mathFinalSubset$g3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "First Math Score G1", y = "Final Math Score G3") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(mathFinalSubset$g3, mathFinalSubset$g1, method='pearson')





# --- G2 and G3 ---

# Scatterplot 
# aes(x,y)
scatter <- ggplot(mathFinalSubset, aes(mathFinalSubset$g2, mathFinalSubset$g3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Second Period Math Score G2", y = "Final Math Score G3") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(mathFinalSubset$g3, mathFinalSubset$g2, method='pearson')


# --- G1 and G2 ---

scatter <- ggplot(mathFinalSubset, aes(mathFinalSubset$g1, mathFinalSubset$g2))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "First Math Score G1", y = "Second Period Score G2") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(mathFinalSubset$g2, mathFinalSubset$g1, method='pearson')




# ------------------------- needs to do some transformation to the variable 'absences' ------------------------------------------------
# --- Absences and G3 ---
# Scatterplot 

#Simple scatterplot of feeling of control and perceived stress
#aes(x,y)
scatter <- ggplot(mathFinalSubset, aes(mathFinalSubset$absences, mathFinalSubset$g3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Number of School Absences", y = "Final Math Score G3") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(mathFinalSubset$g3, mathFinalSubset$absences, method='kendall')






# ------------------------------------- < Difference > ----------------------------

# *************************** Involving Two Groups ************************

# --- G3 and famsup --- (Independent t-test, parametric test)

# install/load the packages needed in this section.
needed_packages <- c("pastecs", "ggplot2", "psych", "semTools", "FSA", "car", "coin", "rstatix", "effectsize")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(psych) # Some useful descriptive functions
library(semTools) #For skewness and kurtosis
library(FSA) #For percentage
library(car) # For Levene's test for homogeneity of variance 
library(coin)
library(rstatix)
library(effectsize) #To calculate effect size for t-test


# describe g3 by group (have family educational support v don't have family educataional support - variable famsup)

# Get descriptive stastitics by group - output as a matrix
psych::describeBy(mathFinalSubset$g3, mathFinalSubset$famsup, mat=TRUE)

# Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(g3 ~ famsup, data=mathFinalSubset)
#Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity

# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g3~famsup,var.equal=TRUE,data=mathFinalSubset)
# No statistically significant difference was found
res <- stats::t.test(g3~famsup,var.equal=TRUE,data=mathFinalSubset)

# Calculate Cohen's d
# artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

# Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

# Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes

# ---- Address Difference (statistically significant but small effect size)----

# Get descriptive statistics by group - output as a matrix
psych::describeBy(math$g3, math$address, mat=TRUE)

# Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(g3 ~ address, data=math)
#Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity

# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g3~address,var.equal=TRUE,data=math)
# No statistically significant difference was found
res <- stats::t.test(g3~address,var.equal=TRUE,data=math)

# Calculate Cohen's d
# artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

# Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

# Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes


# ---- Internet Difference (statistically significant but small effect size) ----

# Get descriptive statistics by group - output as a matrix
psych::describeBy(math$g3, math$internet, mat=TRUE)

# Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(g3 ~ internet, data=math)
#Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity

# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g3~internet,var.equal=TRUE,data=math)
# No statistically significant difference was found
res <- stats::t.test(g3~internet,var.equal=TRUE,data=math)

# Calculate Cohen's d
# artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

# Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

# Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes


# ---- Higher Difference (statistically significant but small effect size) ----

# Get descriptive statistics by group - output as a matrix
psych::describeBy(math$g3, math$higher, mat=TRUE)

# Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(g3 ~ higher, data=math)
#Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity

# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g3~higher,var.equal=TRUE,data=math)
# No statistically significant difference was found
res <- stats::t.test(g3~higher,var.equal=TRUE,data=math)

# Calculate Cohen's d
# artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

# Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

# Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes



# --- absences and romantic (Non-parametric test) ---

# First check that package required is installed, if not install it
# Specify packages
needed_packages <- c("ggplot2", "coin", "rstatix") 
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(ggplot2) # For creating histograms with more detail than plot
library(coin) # For Wilcox test (non-parametric)
library(rstatix)# For calculating effect size

# Create data subsets for each drink
romanticdata <- subset(mathFinalSubset, romantic=='yes')
nonromanticdata <-subset(mathFinalSubset, romantic=='no')

# Get your descriptive statistics - we are using the by function to handle the need to group
by(mathFinalSubset$absences,mathFinalSubset$romantic, median)
by(mathFinalSubset$absences,mathFinalSubset$romantic, IQR)

# Create plots of these
gs <- ggplot(romanticdata, aes(x=absences))
gs <- gs + ggtitle ("Number of school absences")
gs <- gs + labs(x="Having a romantic relationship")
gs <- gs + geom_histogram(binwidth=2, colour="black")
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm, color="red",args=list(mean=mean(romanticdata$absences, na.rm=TRUE), sd=sd(romanticdata$absences, na.rm=TRUE)))
gs

gs <- ggplot(nonromanticdata, aes(x=absences))
gs <- gs + ggtitle ("Number of school absences")
gs <- gs + labs(x="Not Having a romantic relationship")
gs <- gs + geom_histogram(binwidth=2, colour="black")
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm, color="red",args=list(mean=mean(nonromanticdata$absences, na.rm=TRUE), sd=sd(nonromanticdata$absences, na.rm=TRUE)))
gs

# Test for differences on  Sunday to get our U statistic (reported as W)
stats::wilcox.test(absences~romantic, data=mathFinalSubset) 

# To calculate Z we can use the Wilcox test from the coin package 
coin::wilcox_test(absences~as.factor(romantic), data=mathFinalSubset)

# Calculate the effect size R
reff=rstatix::wilcox_effsize(absences~romantic, data=mathFinalSubset)

# Calculate the effect r
reff$effsize


# ****************************** Involving More Than Two Groups **********************88

# Family relationship and g3 (parametric test)

# install and load the packages needed.
needed_packages <- c("pastecs", "ggplot2", "psych", "semTools", "FSA", "sjstats", "userfriendlyscience")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(psych) # Some useful descriptive functions
library(semTools) #For skewness and kurtosis
library(FSA) #For percentage
library(sjstats) #To calculate effect size for t-test
library(userfriendlyscience)

# Get descriptive stastitics by group - output as a matrix
psych::describeBy(mathFinalSubset$g3, mathFinalSubset$famrel, mat=TRUE)

# Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(g3~ famrel, data=mathFinalSubset)
# p value is > 0.05 so the result is not statistically significant so we can assume homogeneity

# Conduct ANOVA using the userfriendlyscience test oneway
# In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
# If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(mathFinalSubset$famrel),y=mathFinalSubset$g3,posthoc='Tukey')
# No statistically significant difference was found

# use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(g3~ famrel, data = mathFinalSubset)
# Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
# Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
# Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta

# In the report we are using the res2 variable to retrieve the degrees of freedom
# and the eta_sq function from the sjstats package to calculate the effect


# ---- Fedu Difference (statistically significant and effect size slightly higher than small) ----


# Get descriptive stastitics by group - output as a matrix
psych::describeBy(math$g3, math$fedu, mat=TRUE)

# Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(g3~ fedu, data=math)
# p value is > 0.05 so the result is not statistically significant so we can assume homogeneity

# Conduct ANOVA using the userfriendlyscience test oneway
# In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
# If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(math$fedu),y=math$g3,posthoc='Tukey')
# No statistically significant difference was found

# use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(g3~ fedu, data = math)
# Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
# Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
# Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta

# In the report we are using the res2 variable to retrieve the degrees of freedom
# and the eta_sq function from the sjstats package to calculate the effect



# ---- Mjob Difference ( statistically significant and effect size higher than small) ----

# Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(g3~ mjob, data=math)
# p value is > 0.05 so the result is not statistically significant so we can assume homogeneity

# Conduct ANOVA using the userfriendlyscience test oneway
# In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
# If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(math$mjob),y=math$g3,posthoc='Tukey')
# No statistically significant difference was found

# use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(g3~ mjob, data = math)
# Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
# Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
# Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta

# In the report we are using the res2 variable to retrieve the degrees of freedom
# and the eta_sq function from the sjstats package to calculate the effect



# ---- Failures Difference (Very good example, statistically significant and large effect size) ----

# Get descriptive stastitics by group - output as a matrix
psych::describeBy(math$g3, math$failures, mat=TRUE)

# Conduct Bartlett's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(g3~failures, data=math)
# p value is > 0.05 so the result is not statistically significant so we can assume homogeneity

# Conduct ANOVA using the userfriendlyscience test oneway
# In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
# If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(math$failures),y=math$g3,posthoc='Tukey')
# No statistically significant difference was found

# use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(g3~ failures, data = math)
# Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
# Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
# Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta


# --- workaday alcohol consumption and absences (Non-parametric) ---

# install and load packages
needed_packages <- c("psych", "semTools", "FSA", "sjstats", "rstatix", "foreign")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(semTools)
library(psych)
library(stats)
library(FSA)
library(rstatix)#Kruskal wallis effect size
library(sjstats)#chi-square effect size

# Get some descriptive statistics on the absences for each level of dalc
psych::describeBy(as.numeric(mathFinalSubset$absences),factor(mathFinalSubset$dalc),IQR=TRUE)

# run a kruskal wallis test
stats::kruskal.test(absences~dalc,data=mathFinalSubset)

# Post hoc testing
# Need library FSA to run the post-hoc tests
tmp<-FSA::dunnTest(x=as.numeric(mathFinalSubset$absences), g=factor(mathFinalSubset$dalc), method="bonferroni") 

# Print results so that in the output X=absences, g=dalc and the test statistic is Z with significance shown underneath
print(tmp, dunn.test.results = TRUE)

# calculate the effect size
rstatix::kruskal_effsize(mathFinalSubset, absences~dalc, ci = FALSE, conf.level = 0.95,
                         ci.type = "perc", nboot = 1000) # uses bootstrapping










# ------------------------------------- < Linear Regression > ----------------------------

# here we keep all the variables in the original dataset.
math <- read.csv2('student-mat.csv')
names <- c(1,2,4:29)
math[,names] <- lapply(math[,names],factor)
summary(math)

# drop missing values in G2 and G3
library(VIM)
library(tidyverse)
# Setting the column names into lower case
colnames(math) <- tolower(colnames(math))
math[,c('g2','g3')][math[,c('g2','g3')]==0] <- NA
#Dropping the rows with missing values.
math <- na.omit(math)
dim(math)
View(math)

# ------------- Multiple Linear Regression --------

# install and load packages
needed_packages <- c("foreign",  "lm.beta", "stargazer", "car", "ppcor", "userfriendlyscience")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 
library(foreign) #To work with SPSS data
library(lm.beta) #Will allow us to isolate the beta co-efficients
library(stargazer)#For formatting outputs/tables
library(userfriendlyscience)#Anova
library(ppcor)#partial correlation
library(car)#Levene's test

# ---- Model 1: ----- g3 and g1 ----

# Normality and linear relationship of g1 and g3 has been test before
# Simple Linear Regression --- g3 predicted by g1

model1<-lm(math$g3~math$g1)
anova(model1)

summary(model1)
stargazer(model1, type="text") # Tidy output of all the required stats

lm.beta(model1) # standardised coefficients

# ********* Test assumptions first model *********
# Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model1))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

# find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(math[influential, ])  # influential observations.

# Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?
car::outlierTest(model1) 

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model1)
plot(density(resid(model1)))  # Create histogram and density plot of the residuals


# ---- Model 2 --- Differential effect by address (g3 -- g1 and address) including dummy variable for address -----

model2<-lm(math$g3~math$g1+math$address)
anova(model2)

summary(model2)
stargazer(model2, type="text") # Tidy output of all the required stats
lm.beta(model2)
stargazer(model1, model2, type="text") # Quick model comparison

# ********* Test assumptions the second model *********

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model2))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
#find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(math[influential, ])  # influential observations.
car::outlierTest(model2) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?
car::leveragePlots(model2) # leverage plots

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model2)
plot(density(resid(model1)))  # Create histogram and density plot of the residuals

#Calculate Collinearity
vifmodel<-car::vif(model2)
vifmodel

#Calculate tolerance
1/vifmodel



# ---- Model 3 --- Interaction terms (g1 and g2) ----
math$interaction <- as.numeric(math$g1)*as.numeric(math$g2)
model3<-lm(math$g3~math$g1+math$address+math$interaction)

summary(model3)
stargazer::stargazer(model3, type="text") #Tidy output of all the required stats
lm.beta(model3) # standardise the coefficients
stargazer(model1, model2, model3, type="text") # Quick model comparison

# ********* Test assumptions the third model *********

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model3))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
#find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(math[influential, ])  # influential observations.
car::outlierTest(model3) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model3) # leverage plots

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model3)
plot(density(resid(model3))) 

#Calculate Collinearity
vifmodel<-car::vif(model3)
vifmodel

#Calculate tolerance
1/vifmodel




math$interaction <- as.numeric(math$g1)*as.numeric(math$g2)
model3<-lm(math$g3~math$address+math$interaction)

summary(model3)
stargazer::stargazer(model3, type="text") #Tidy output of all the required stats
lm.beta(model3) # standardise the coefficients
stargazer(model1, model2, model3, type="text") # Quick model comparison

# ********* Test assumptions the third model *********

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model3))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
#find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)
head(math[influential, ])  # influential observations.
car::outlierTest(model3) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

car::leveragePlots(model3) # leverage plots

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model3)
plot(density(resid(model3))) 

#Calculate Collinearity
vifmodel<-car::vif(model3)
vifmodel

#Calculate tolerance
1/vifmodel







# ------------------------------------- < Logistic Regression > ---------------------------

# ---- Binary Logistic Regression ----

# Install and load packages
needed_packages <- c("foreign",  "Epi", "arm", "DescTools", "stargazer", "lmtest",  "car", "generalhoslem", "reshape2", "nnet", "ggplot2")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org")
library(Epi)
library(stargazer)
library(lmtest)
library(DescTools)
library(nnet)# Multinomial regression
library(foreign)
library(reshape2)
library(ggplot2)
library(generalhoslem)#For test of fit for logistic regression


# here we keep all the variables in the original dataset.
math <- read.csv2('student-mat.csv')
names <- c(1,2,4:29)
math[,names] <- lapply(math[,names],factor)
summary(math)

# Can we use School, Studytime to Predict family educational support?


# ---- Model 1 --- School as predictor ----

# Make sure categorical data is used as factors
logmodel1 <- glm(famsup ~ school, data = math, na.action = na.exclude, family = binomial(link=logit))

# Full summary of the model
summary(logmodel1)

# Chi-square plus significance
lmtest::lrtest(logmodel1)

# Chi-square and Pseudo R2 calculation - THESE ARE INCLUDED FOR INFORMATION ONLY
# lmtest:lrtest achieves the same thing
modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2
chidf <- logmodel1$df.null - logmodel1$df.residual
chidf
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

# Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=math$famsup ~ math$school, plot="ROC")

# Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")

DescTools::PseudoR2(logmodel1, which="Nagelkerke")

# Summary of the model with co-efficients
stargazer(logmodel1, type="text")

# Exponentiate the co-efficients
exp(coefficients(logmodel1))

## odds ratios and 95% CI 
cbind(Estimate=round(coef(logmodel1),4),
      OR=round(exp(coef(logmodel1)),4))

# Probability of having family educational support when at school GP
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0)

# Probability of having family educational support when at school MS
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1)

# Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
# Won't give a p-value here because only one predictor
generalhoslem::logitgof(math$famsup, fitted(logmodel1))


# ---- Model 2 --- Adding Studytime as a predictor ----

library(regclass)

# Make sure categorical data is used as factors
logmodel2 <- glm(famsup ~ school+studytime, data = math, na.action = na.exclude, family = binomial(link=logit))

# Full summary of the model
summary(logmodel2)

# Chi-square plus significance
lmtest::lrtest(logmodel2)

# Pseudo Rsquared 
DescTools::PseudoR2(logmodel2, which="CoxSnell")

DescTools::PseudoR2(logmodel2, which="Nagelkerke")

# Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=math$famsup ~ math$school+math$studytime, plot="ROC")

# Summary of the model with co-efficients
stargazer(logmodel2, type="text")

# Exponentiate the co-efficients
exp(coefficients(logmodel2))

## odds ratios 
cbind(Estimate=round(coef(logmodel2),4),
      OR=round(exp(coef(logmodel2)),4))

# studytime 1 (< 2 hours),
# studytime 2 (2 to 5 hours),
# studytime 3 (5 to 10 hours),
# studytime 4 (> 10 hours).

# 1. Probability of having family educational support when students at school GP and study < 2 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0)

# 2. Probability of having family educational support when students at school MS and study < 2 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1)

# 3. Probability of having family educational support when students at school GP and study 2 to 5 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

# 4. Probability of having family educational support when students at school MS and study 2 to 5 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

# 5. Probability of having family educational support when students at school GP and study 5 to 10 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*0)

# 6. Probability of having family educational support when students at school MS and study 5 to 10 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*0)

# 7. Probability of having family educational support when students at school GP and study > 10 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*1)

# 8. Probability of having family educational support when students at school MS and study > 10 hours
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*1+coef(logmodel2)[3]*1)

regclass::confusion_matrix(logmodel2)

# Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok

generalhoslem::logitgof(math$famsup, fitted(logmodel2))

# Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel

# Tolerance
1/vifmodel




# ------------------------------------ < Dimension Reduction > ---------------------------------------

# install and load packages
needed_packages <- c("psych",  "REdaS", "Hmisc", "corrplot", "ggcorrplot", "factoextra",  "nFactors")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 
library(psych)
library(REdaS)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(factoextra) # Used for principal component analysis to get a different view of eigenvalues
library(nFactors)

# load the dataset
math <- read.csv2('student-mat.csv')

View(math)
dim(math)
colnames(math)
summary(math)

# Missing Values
# Load the packages
library(VIM)
library(tidyverse)
# Setting the column names into lower case
colnames(math) <- tolower(colnames(math))
# Check the missing value
missingCheck <- math[which(math$g1==0 | math$g2==0 | math$g3==0),]
# Change the "0" in the test scores into NAs
math[,c('g2','g3')][math[,c('g2','g3')]==0] <- NA
#Dropping the rows with missing values.
math <- na.omit(math)

dim(math)
View(math)

# These are our variables of interest
varsinterest <- c('medu','fedu','studytime',
                  'freetime','goout','walc','dalc','absences','g1','g2','g3')
# Create a subset of data with just the variables of interest
math <- math[varsinterest]

dim(math)
summary(math)




# --- Step 1: Screen the correlation matrix
# Create a correlation matrix (these are just some methods)
mathMatrix<-cor(math)
round(mathMatrix,2)
# or showing p value as well
Hmisc::rcorr(as.matrix(math))

# Visualization of significance levels at 0.05
res1 <- corrplot::cor.mtest(mathMatrix, conf.level = .95)
corrplot::corrplot(mathMatrix, p.mat = res1$p, type="lower", sig.level = .05)
# Showing p-value for non-significant results
corrplot(mathMatrix, p.mat = res1$p, type="lower",insig = "p-value")




# --- Step 2: Check if data is suitable - look at the relevant Statistics 

# Bartlett’s test
psych::cortest.bartlett(math)

# KMO 
REdaS::KMOS(math)

# Determinant 
det(mathMatrix)




# --- Step 3&4: Do the Dimension Reduction and choose factor to retain


facsol <- psych::fa(mathMatrix, nfactors=4, obs=NA, n.iter=1, rotate="varimax", fm="pa")

# Create your scree plot
plot(facsol$values, type = "b") #scree plot

# Print the Variance accounted for by each factor/component
facsol$Vaccounted

# Output the Eigenvalues
facsol$values 

# Print the components with loadings
psych::print.psych(facsol,cut=0.3, sort=TRUE)

# Print sorted list of loadings
fa.sort(facsol$loading)

# create a diagram showing the factors and how the manifest variables load
fa.diagram(facsol)

# --- Step 5: Apply rotation

# Apply rotation to try to refine the component structure
facsolrot <-  principal(math, rotate = "varimax")
# output the components
psych::print.psych(facsolrot, cut = 0.3, sort = TRUE)

# output the communalities
facsolrot$communality

# --- Step 6: Reliability Analysis

# If you know that variables are grouped, test each group as a separate scale
mathPerformance<-math[,c(9,10,11)]
mathRelax<-math[,c(5,6,7)]
mathParents<-math[,c(1,2)]

# Output our Cronbach Alpha values
psych::alpha(mathPerformance, check.keys=TRUE)
psych::alpha(mathRelax, check.keys=TRUE)
psych::alpha(mathParents, check.keys=TRUE)





