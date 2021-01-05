# -------------------------------- < Data Import > --------------------------

library(readxl)
library(tidyverse)

data <- read_excel('data_academic_performance.xlsx')

dim(data) # in total 12411 instances, 45 variables (one empty column created when importing)

data <- subset(data, select = -c(...10)) # drop the empty column

dim(data) # in total 12411 instances, 44 variables

colnames(data)

names <- c(2:24,30:32)
data[,names] <- lapply(data[,names],factor)

summary(data)
View(data)

# These are our variables of interest
varsinterest <- c('CR_S11','CC_S11','ENG_S11','CR_PRO','CC_PRO','ENG_PRO', 'FEP_PRO', 'G_SC', 'SEL', 'SEL_IHE', 'GENDER', 'PEOPLE_HOUSE', 
                  'INTERNET','TV','COMPUTER','MOBILE','REVENUE','SCHOOL_NAT', 'ACADEMIC_PROGRAM')
# Create a subset of data with just the variables of interest
subdata <- data[varsinterest]

dim(subdata)
View(subdata)

# -------------------------------- < Data Exploration > --------------------------

# transform the categorical data into factors so the summary will be more clear

names <- c(9:19)
subdata[,names] <- lapply(subdata[,names],factor)

colnames(subdata)
summary(subdata)

summary(subdata$PEOPLE_HOUSE)
summary(subdata$REVENUE)
summary(subdata$ACADEMIC_PROGRAM)

write_csv2(subdata,"Performance_Engineering.csv")

# No missing value observed


# -------------------------------- < Descriptive Statistics and Views > --------------------------


Dataset <- 
        read.table("/Users/zenglan/Desktop/Prob. Stats Project 6 Jan/Performance_Engineering.csv", 
                   header=TRUE, stringsAsFactors=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

library(abind, pos=17)
library(e1071, pos=18)
numSummary(Dataset[,c("CC_PRO", "CC_S11", "CR_PRO", "CR_S11", "ENG_PRO", "ENG_S11", 
                      "FEP_PRO", "G_SC"), drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), 
           quantiles=c(0,.25,.5,.75,1))


local({
        .Table <- with(Dataset, table(ACADEMIC_PROGRAM))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(COMPUTER))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(GENDER))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(INTERNET))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(MOBILE))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(PEOPLE_HOUSE))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(REVENUE))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(SCHOOL_NAT))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})
local({
        .Table <- with(Dataset, table(TV))
        cat("\ncounts:\n")
        print(.Table)
        cat("\npercentages:\n")
        print(round(100*.Table/sum(.Table), 2))
})




with(Dataset, Hist(G_SC, scale="frequency", breaks="Sturges", col="darkgray"))


Boxplot( ~ CR_S11, data=Dataset, id=list(method="y"))

library(colorspace, pos=19)
with(Dataset, piechart(GENDER, xlab="", ylab="", main="GENDER", col=rainbow_hcl(2), 
                       scale="percent"))

with(Dataset, piechart(REVENUE, xlab="", ylab="", main="REVENUE", col=rainbow_hcl(8), 
                       scale="percent"))




# -------------------------------- < Assess Normality > --------------------------

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

# ----------- Normality for 'FEP_PRO' ----


# -- Generate Histogram --

# We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(subdata, aes(x=subdata$FEP_PRO))

# Change the label of the x axis
gg <- gg + labs(x="Formulation of Engineering Projects (FEP_PRO)")

# manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

# adding a normal curve
# use stat_function to compute a normalised score for each value
# pass the mean and standard deviation
# use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(subdata$FEP_PRO, na.rm=TRUE), sd=sd(subdata$FEP_PRO, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


# -- Generate Q-Q Plot --

qqnorm(subdata$FEP_PRO)
qqline(subdata$FEP_PRO, col=2) # show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(subdata$FEP_PRO, basic=F)

# -- Standardised value of Skew and Kurtosis --
skew<-semTools::skew(subdata$FEP_PRO)
kurt <- semTools::kurtosis(subdata$FEP_PRO)

skew[1]/skew[2]  # standardised value for skew
kurt[1]/kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
z <- abs(scale(subdata$FEP_PRO))

FSA::perc(as.numeric(z), 1.96, "gt")

# Calculate the percentage of standardised scores that are greater than 3.29
FSA::perc(as.numeric(z), 3.29, "gt")



# ----------- Normality for 'CR_S11' ----


# -- Generate Histogram --

# We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(subdata, aes(x=subdata$CR_S11))

# Change the label of the x axis
gg <- gg + labs(x="Critical Reading Score of High School (CR_S11)")

# manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

# adding a normal curve
# use stat_function to compute a normalised score for each value
# pass the mean and standard deviation
# use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(subdata$CR_S11, na.rm=TRUE), sd=sd(subdata$CR_S11, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


# -- Generate Q-Q Plot --

qqnorm(subdata$CR_S11)
qqline(subdata$CR_S11, col=2) # show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(subdata$CR_S11, basic=F)

# -- Standardised value of Skew and Kurtosis --
skew<-semTools::skew(subdata$CR_S11)
kurt <- semTools::kurtosis(subdata$CR_S11)

skew[1]/skew[2]  # standardised value for skew
kurt[1]/kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
z <- abs(scale(subdata$CR_S11))

FSA::perc(as.numeric(z), 1.96, "gt")

# Calculate the percentage of standardised scores that are greater than 3.29
FSA::perc(as.numeric(z), 3.29, "gt")



# ----------- Normality for 'CC_S11' ----


# -- Generate Histogram --

# We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(subdata, aes(x=subdata$CC_S11))

# Change the label of the x axis
gg <- gg + labs(x="Citizen Competencies Score of High School (CC_S11)")

# manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

# adding a normal curve
# use stat_function to compute a normalised score for each value
# pass the mean and standard deviation
# use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(subdata$CC_S11, na.rm=TRUE), sd=sd(subdata$CC_S11, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


# -- Generate Q-Q Plot --

qqnorm(subdata$CC_S11)
qqline(subdata$CC_S11, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(subdata$CC_S11, basic=F)

# -- Standardised value of Skew and Kurtosis --
skew<-semTools::skew(subdata$CC_S11)
kurt <- semTools::kurtosis(subdata$CC_S11)

skew[1]/skew[2]  # standardised value for skew
kurt[1]/kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
z <- abs(scale(subdata$CC_S11))

FSA::perc(as.numeric(z), 1.96, "gt")

# Calculate the percentage of standardised scores that are greater than 3.29
FSA::perc(as.numeric(z), 3.29, "gt")


# ----------- Normality for 'CC_PRO' ----


# -- Generate Histogram --

# We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(subdata, aes(x=subdata$CC_PRO))

# Change the label of the x axis
gg <- gg + labs(x="Citizen Competencies score of professional career (CC_PRO)")

# manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

# adding a normal curve
# use stat_function to compute a normalised score for each value
# pass the mean and standard deviation
# use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(subdata$CC_PRO, na.rm=TRUE), sd=sd(subdata$CC_PRO, na.rm=TRUE)))

# to display the graph request the contents of the variable be shown
gg


# -- Generate Q-Q Plot --

qqnorm(subdata$CC_PRO)
qqline(subdata$CC_PRO, col=2) # show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(subdata$CC_PRO, basic=F)

# -- Standardised value of Skew and Kurtosis --
skew<-semTools::skew(subdata$CC_PRO)
kurt <- semTools::kurtosis(subdata$CC_PRO)

skew[1]/skew[2]  # standardised value for skew
kurt[1]/kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
z <- abs(scale(subdata$CC_PRO))

FSA::perc(as.numeric(z), 1.96, "gt")

# Calculate the percentage of standardised scores that are greater than 3.29
FSA::perc(as.numeric(z), 3.29, "gt")




# ----------- Normality for 'G_SC' ----


# -- Generate Histogram --

#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(subdata, aes(x=subdata$G_SC))

#Change the label of the x axis
gg <- gg + labs(x="Global Score (G_SC)")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(subdata$G_SC, na.rm=TRUE), sd=sd(subdata$G_SC, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg


# -- Generate Q-Q Plot --

qqnorm(subdata$G_SC)
qqline(subdata$G_SC, col=2) #show a line on the plot

# -- Generate Summary Statistics --
# Descriptive Stats
pastecs::stat.desc(subdata$G_SC, basic=F)

# -- Standardised value of Skew and Kurtosis --
skew<-semTools::skew(subdata$G_SC)
kurt <- semTools::kurtosis(subdata$G_SC)

skew[1]/skew[2]  # standardised value for skew
kurt[1]/kurt[2]  # standardised value for kurtosis

# -- Calculate the percentage of outliers --
# Calculate the percentage of standardised scores for the variable itself that are outside our acceptable range.
# Calculate the percentage of standardised scores that are greater than 1.96
z <- abs(scale(subdata$G_SC))

FSA::perc(as.numeric(z), 1.96, "gt")

# Calculate the percentage of standardised scores that are greater than 3.29
FSA::perc(as.numeric(z), 3.29, "gt")


# -------------------------------- < Assess Correlation > --------------------------

# install and load packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA")                                    
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                              

library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) #For skewness and kurtosis



# ----------- Correlation between 'FEP_PRO' and 'CR_S11' ----


# Scatterplot 
# aes(x,y)
scatter <- ggplot(subdata, aes(subdata$CR_S11, subdata$FEP_PRO))

# Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Critical Reading CR_S11", y = "Formulation of Engineering Projects FEP_PRO") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(subdata$FEP_PRO, subdata$CR_S11, method='pearson')



# ----------- Correlation between 'FEP_PRO' and 'CC_S11' ----


# Scatterplot 
# aes(x,y)
scatter <- ggplot(subdata, aes(subdata$CC_S11, subdata$FEP_PRO))

# Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Citizen Competencies CC_S11", y = "Formulation of Engineering Projects FEP_PRO") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(subdata$FEP_PRO, subdata$CC_S11, method='pearson')

# ----------- Correlation between 'G_SC' and 'CC_S11' ----



# Scatterplot 
# aes(x,y)
scatter <- ggplot(subdata, aes(subdata$CC_S11, subdata$G_SC))

# Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Citizen Competencies CC_S11", y = "Global Score G_SC") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(subdata$G_SC, subdata$CC_S11, method='pearson')


# ----------- Correlation between 'G_SC' and 'CR_S11' ----



# Scatterplot 
#aes(x,y)
scatter <- ggplot(subdata, aes(subdata$CR_S11, subdata$G_SC))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Critical Reading CR_S11", y = "Global Score G_SC") 

# --- Conducting Correlation Test --
# Parametric test should be chosen here
# Pearson Correlation Test
stats::cor.test(subdata$G_SC, subdata$CR_S11, method='pearson')


# -------------------------------- < Assess Difference > --------------------------

# -------------------- Involving Two Groups -------------

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


# ----- CC_PRO and Gender ----- (Independent t-test, parametric test) ----

# Get descriptive stastitics by group - output as a matrix
psych::describeBy(subdata$CC_PRO, subdata$GENDER, mat=TRUE)

# Conduct Levene's test for homogeneity of variance in library car - the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(CC_PRO ~ GENDER, data=subdata)
# Pr(>F) is your probability - in this case it is not statistically significant so we can assume homogeneity

# Conduct the t-test from package stats
# In this case we can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate
stats::t.test(g3~famsup,var.equal=TRUE,data=mathFinalSubset)
# No statistically significant difference was found
res <- stats::t.test(g3~famsup,var.equal=TRUE,data=mathFinalSubset)

# Calculate Cohen's d
# arithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

# Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

# Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes



# -------------------- Involving More than Two Groups -------------

# ----- G_SC and SEL ----- (ANOVA test) ----

# describe SEL by group (variable SEL, level 1,2,3,4)

# Get descriptive statistics by group - output as a matrix
psych::describeBy(subdata$G_SC, subdata$SEL, mat=TRUE)

# Conduct Bartlett's test for homogeneity of variance in library car - 
# the null hypothesis is that variances in groups are equal 
# so to assume homogeneity we would expect probability to not be statistically significant.
stats::bartlett.test(G_SC ~ SEL, data=subdata)
# p value is > 0.05 so the result is not statistically significant so we can assume homogeneity

# Conduct ANOVA using the userfriendlyscience test oneway
# In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
# If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(subdata$SEL),y=subdata$G_SC,posthoc='Tukey')
# Statistically significant difference was found

# use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(G_SC~ SEL, data = subdata)
# Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
# Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
# Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta




# ------------------------------------- < Multiple Linear Regression > ----------------------------

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


# ---- Model 1: ----- 'CR_S11' and 'CC_S11' as predictors ----

# Normality of 'CR_S11' and 'CC_S11'have been tested before
# Linear relationship of 'CR_S11', 'CC_S11' with 'G_SC' have been tested before

# Model 1

model1<-lm(subdata$G_SC~subdata$CR_S11+subdata$CC_S11)
anova(model1)

summary(model1)
stargazer(model1, type="text") # Tidy output of all the required stats

lm.beta(model1) # standardised coefficients


# ********* Test assumptions of Model 1 *********

#Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model1))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)

head(subdata[influential, ])  # influential observations.
car::outlierTest(model1) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?
car::leveragePlots(model1) # leverage plots

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model1)
plot(density(resid(model1)))  # Create histogram and density plot of the residuals

#Calculate Collinearity
vifmodel<-car::vif(model1)
vifmodel

#Calculate tolerance
1/vifmodel





# ---- Model 2 --- Differential effect induced by 'SEL' (G_SC' -- 'CR_S11', 'CC_S11', AND 'SEL')-----

model2<-lm(subdata$G_SC~subdata$CR_S11+subdata$CC_S11+subdata$SEL)
anova(model2)

summary(model2)
stargazer(model2, type="text") # Tidy output of all the required stats
lm.beta(model2)
stargazer(model1, model2, type="text") # Quick model comparison

# ********* Test assumptions the second model *********

# Influential Outliers - Cook's distance
cooksd<-sort(cooks.distance(model2))
# plot Cook's distance
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# find rows related to influential observations
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
stem(influential)

head(subdata[influential, ])  # influential observations.
car::outlierTest(model2) # Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?
car::leveragePlots(model2) # leverage plots

# First plot: Residuals vs Fitted (check linear relationship assumption)
# Second plot: Normal Q-Q plot (check the normality assumption of residuals)
# Third plot: Scale-Location plot (check homoscedasticity assumption of the residuals)
# Fourth plot: Residuals vs Leverage (check influential outliers)
plot(model2)
plot(density(resid(model2)))  # Create histogram and density plot of the residuals

#Calculate Collinearity
vifmodel<-car::vif(model2)
vifmodel

#Calculate tolerance
1/vifmodel






# -------------------------------------- Correlation Matrix of all the numeric data in the original dataset ---------------------------------


# --- Screen the correlation matrix
# Create a correlation matrix (these are just some methods)
data_interval_variables <- subset(data, select = -c(1:24,30:32))
colnames(data_interval_variables)
summary(data_interval_variables)

dataMatrix<-cor(data_interval_variables)
round(dataMatrix,2)
# or showing p value as well
Hmisc::rcorr(as.matrix(data_interval_variables))

# Visualization of significance levels at 0.05
res1 <- corrplot::cor.mtest(dataMatrix, conf.level = .95)
corrplot::corrplot(dataMatrix, p.mat = res1$p, type="lower", sig.level = .05)
# Showing p-value for non-significant results
corrplot(dataMatrix, p.mat = res1$p, type="lower",insig = "p-value")


