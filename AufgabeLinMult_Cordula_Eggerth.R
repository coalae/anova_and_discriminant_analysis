# AUFGABE: LINEARE MULTIVARIATE STATISTIK
# Cordula Eggerth (00750881)

rm(list=ls())

install.packages("nortest")
install.packages("car")
install.packages("DescTools")
install.packages("MASS")
install.packages("xtable")
install.packages("tidyverse")
install.packages("ROCR")

library(nortest)
library(car)
library(DescTools)
library(MASS)
library(tidyverse)
library(ROCR)

setwd("C:/Users/Coala/Desktop/LINMULT-HW")

#***********************************************************************************************
# EXERCISE 1
#***********************************************************************************************
# Three types of medical treatments for stress reduction were tested on groups
# of males and females. For each person it is provided which tretament was
# used ("1", "2", "3") and the stress level before and after the treatment. We
# are interested whether these treatments are equally effective and whether the
# gender plays a role. Read the stressData.csv into R and then do the following.

# 1. Graphically represent the data (for example with box plots), calculate
#    the means and medians of changes in stress levels, taking both factors
#    (treatment type and gender) into consideration.

# read data
stressData <- read.csv("StressData.csv", sep=";")

# descriptive statistics
head(stressData, n=5)
nrow(stressData)
ncol(stressData)
summary(stressData)
cols <- colnames(stressData)
cols2 <- paste(cols, collapse="+")
stressData$Treatment <- factor(stressData$Treatment)

stressData$diffStress <- stressData$stressBefore-stressData$stressAfter
stressData$group <- rep("g",nrow(stressData))
stressData$group[stressData$gender=="F" & stressData$Treatment==1] <- "F1"
stressData$group[stressData$gender=="F" & stressData$Treatment==2] <- "F2"
stressData$group[stressData$gender=="F" & stressData$Treatment==3] <- "F3"
stressData$group[stressData$gender=="M" & stressData$Treatment==1] <- "M1"
stressData$group[stressData$gender=="M" & stressData$Treatment==2] <- "M2"
stressData$group[stressData$gender=="M" & stressData$Treatment==3] <- "M3"
stressData$group <- factor(stressData$group)

# summary considering gender
summary(stressData[stressData$gender=="F",])
summary(stressData[stressData$gender=="M",])

# summary considering stress level
summary(stressData[stressData$Treatment==1,])
summary(stressData[stressData$Treatment==2,])
summary(stressData[stressData$Treatment==3,])

# boxplots
par(mfrow=c(1,1))
boxplot(stressData$stressAfter ~ stressData$gender, main="stressAfter ~ gender", 
        xlab="gender", ylab="stress level after")
boxplot(stressData$stressAfter ~ stressData$Treatment, main="stressAfter ~ Treatment",
        xlab="treatment", ylab="stress level after")

# means and medians of changes in stress levels
# (taking both factors (treatment and gender) into consideration)
factor.treatment <- factor(stressData$Treatment)
is.factor(factor.treatment)

# gender diffs
 # methode 1
diffs.gender.mean <- tapply(stressData$stressBefore-stressData$stressAfter, stressData$gender, mean)
diffs.gender.median <- tapply(stressData$stressBefore-stressData$stressAfter, stressData$gender, median)

 # methode 2 (probe: ok)
diff.stress.m <- stressData[stressData$gender=="M", ]$stressBefore - 
                 stressData[stressData$gender=="M", ]$stressAfter
mean(diff.stress.m)
median(diff.stress.m)
diff.stress.f <- stressData[stressData$gender=="F", ]$stressBefore - 
                 stressData[stressData$gender=="F", ]$stressAfter
mean(diff.stress.f)
median(diff.stress.f)

# treatment diffs
diffs.treat.mean <- tapply(stressData$stressBefore-stressData$stressAfter, stressData$Treatment, mean)
diffs.treat.median <- tapply(stressData$stressBefore-stressData$stressAfter, stressData$Treatment, median)

par(mfrow=c(2,2))
barplot(diffs.gender.mean, main="mean diff. for gender", col=c("indianred1","lightslateblue"),
        xlab="gender", ylab="diff")
barplot(diffs.gender.median, main="median diff. for gender", col=c("indianred1","lightslateblue"),
        xlab="gender", ylab="diff")
barplot(diffs.treat.mean, main="mean diff. for treatment", col=c("darkseagreen1","forestgreen","khaki4"),
        xlab="treatment", ylab="diff")
barplot(diffs.treat.median, main="median diff. for treatment", col=c("darkseagreen1","forestgreen","khaki4"),
        xlab="treatment", ylab="diff")

# sample diffs (considering gender and treatment at the same time)
diffs.gt.mean <- tapply(stressData$stressBefore-stressData$stressAfter, 
                        list(stressData$Treatment, stressData$gender), mean)
diffs.gt.median <- tapply(stressData$stressBefore-stressData$stressAfter, 
                        list(stressData$Treatment, stressData$gender), median)


# 2. Using R-built-in functions analyze whether the standard analysis of vari-
#    ance (ANOVA) assumptions are met (normal data and equal variances).

# check if residuals normal:
res <- lm(diffStress ~ gender*Treatment, data=stressData)
summary(res)

par(mfrow=c(1,1))
hist(residuals(res), breaks=15, col="slategray2")

# check if residuals equal variance: 
plot(fitted(res), residuals(res))

# check (for groups): normal data >> result: not all groups are normal
  # histogram
par(mfrow=c(2,3))
hist(stressData$diffStress[stressData$group=="F1"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="F1", 
     xlab="")
lines(density(stressData$diffStress[stressData$group=="F1"]),col=2)
hist(stressData$diffStress[stressData$group=="F2"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="F2",
     xlab="")
lines(density(stressData$diffStress[stressData$group=="F2"]),col=2)
hist(stressData$diffStress[stressData$group=="F3"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="F3", 
     xlab="")
lines(density(stressData$diffStress[stressData$group=="F3"]),col=2)
hist(stressData$diffStress[stressData$group=="M1"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="M1",
     xlab="")
lines(density(stressData$diffStress[stressData$group=="M1"]),col=2)
hist(stressData$diffStress[stressData$group=="M2"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="M2",
     xlab="")
lines(density(stressData$diffStress[stressData$group=="M2"]),col=2)
hist(stressData$diffStress[stressData$group=="M3"], probability=TRUE, 
     breaks=11, col="lavenderblush3", ylim=c(0,1), main="M3",
     xlab="")
lines(density(stressData$diffStress[stressData$group=="M3"]),col=2)

  # qqplot and qqline
par(mfrow=c(2,3))
qqnorm(stressData$diffStress[stressData$group=="F1"],main="QQ plot: F1",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="F1"])
qqnorm(stressData$diffStress[stressData$group=="F2"],main="QQ plot: F2",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="F2"])
qqnorm(stressData$diffStress[stressData$group=="F3"],main="QQ plot: F3",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="F3"])
qqnorm(stressData$diffStress[stressData$group=="M1"],main="QQ plot: M1",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="M1"])
qqnorm(stressData$diffStress[stressData$group=="M2"],main="QQ plot: M2",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="M2"])
qqnorm(stressData$diffStress[stressData$group=="M3"],main="QQ plot: M3",pch=19,col="royalblue4")
qqline(stressData$diffStress[stressData$group=="M3"])

  # shapiro-wilk test for normality (at 0.05 level)
shapiro.test(stressData$diffStress[stressData$group=="F1"]) # not normal
shapiro.test(stressData$diffStress[stressData$group=="F2"]) # normal
shapiro.test(stressData$diffStress[stressData$group=="F3"]) # not normal
shapiro.test(stressData$diffStress[stressData$group=="M1"]) # normal
shapiro.test(stressData$diffStress[stressData$group=="M2"]) # normal
shapiro.test(stressData$diffStress[stressData$group=="M3"]) # normal

  # anderson-darling test for normality (at 0.05 level)
ad.test(stressData$diffStress[stressData$group=="F1"]) # not normal
ad.test(stressData$diffStress[stressData$group=="F2"]) # normal
ad.test(stressData$diffStress[stressData$group=="F3"]) # not normal
ad.test(stressData$diffStress[stressData$group=="M1"]) # normal
ad.test(stressData$diffStress[stressData$group=="M2"]) # normal
ad.test(stressData$diffStress[stressData$group=="M3"]) # normal

# check: equal variances
  # bartlett test of homogeneity of variances 
  # (data should ideally be normal)
bartlett.test(diffStress ~ group, data=stressData) # equal variances

  # levene test (also for non-normal data)
leveneTest(diffStress ~ group, data=stressData) # equal variances

  # fligner-killeen test (also for non-normal data)
fligner.test(diffStress ~ group, data=stressData) # equal variances


# 3. Perform the analysis of variance (ANOVA).
# verified assumptions: non-normal data, but equal variances, 
# therefore, a replacement for ANOVA is chosen

# if assumptions met, ANOVA result would be:
res.aov <- aov(diffStress ~ gender * Treatment, data=stressData)
summary(res.aov)
# bonferroni correction: 
pairwise.t.test(stressData$diffStress, stressData$group, p.adjust="bonferroni")


# 4. Can we conclude that all groups perform similarly? If not, use the stan-
#    dard post-hoc tests such as Bonferroni, Tukey, etc. to investigate further.

# kruskal-wallis test
kruskal.test(stressData$diffStress ~ stressData$group, 
             data=stressData) # means are not equal

# tukey's test
par(mfrow=c(2,2))
TukeyHSD(res.aov)
plot(TukeyHSD(res.aov))
abline(v=0)

# posthoc test with scheffe method/scheffe test
PostHocTest(aov(diffStress ~ group, data=stressData), method = "scheffe")
ScheffeTest(aov(diffStress ~ group, data=stressData))

# 5. Provide a report (pdf format) which contains your code and its outputs,
#    as well as corresponding plots from R. Summarize the conclusions of your
#    analysis.
# see above


#***********************************************************************************************
# EXERCISE 2
#***********************************************************************************************
# 1. In a few sentences compare the following classification methods: linear dis-
#    criminant analysis, quadratic discriminant analysis and logistic regression.
#    Comment on the basic idea behind the method, as well as the assumptions
#    and performance. Consult the literature for this, if needed.
# see report

# 2. Look at the creditdata.csv and focus on the following variables: default
#    (whether a person defaults on a loan or not), duration (loan duration in
#    months), amount (credit amount), installment (as a percentage of dispos-
#    able income) and age.

# read data
creditData <- read.csv("creditdata.csv", sep=";")
colnames(creditData)
creditData <- creditData[,c(1,2,3,4,6)]
colnames(creditData)
factor.Default <- factor(creditData$Default)

# 3. Summarize the data (averages, normality, etc.)
head(creditData, n=10)
nrow(creditData)
ncol(creditData)
summary(creditData)

  # summary
summary(creditData[creditData$Default=="0",])
summary(creditData[creditData$Default=="1",])

  # boxplots
par(mfrow=c(1,1))
boxplot(creditData$duration ~ creditData$Default, main="duration ~ default", 
        xlab="default", ylab="duration")
boxplot(creditData$amount ~ creditData$Default, main="amount ~ default", 
        xlab="default", ylab="amount")
boxplot(creditData$installment ~ creditData$Default, main="installment ~ default", 
        xlab="default", ylab="installment")
boxplot(creditData$age ~ creditData$Default, main="age ~ default", 
        xlab="default", ylab="age")


  # check if residuals normal:
res <- lm(Default ~ duration*amount*installment*age, data=creditData)
summary(res)

par(mfrow=c(1,1))
hist(residuals(res), breaks=15, col="slategray2")

  # check if residuals equal variance: 
plot(fitted(res), residuals(res))
  
  # check if data normal  
  # shapiro-wilk test for normality 
shapiro.test(creditData$Default)  
shapiro.test(creditData$amount[creditData$Default==1])  
shapiro.test(creditData$amount[creditData$Default==0])  
shapiro.test(creditData$duration[creditData$Default==1])
shapiro.test(creditData$duration[creditData$Default==0])  
shapiro.test(creditData$installment[creditData$Default==1])  
shapiro.test(creditData$installment[creditData$Default==0])  
shapiro.test(creditData$age[creditData$Default==1])
shapiro.test(creditData$age[creditData$Default==0])
  
  # anderson-darling test for normality 
ad.test(creditData$Default)  
ad.test(creditData$amount[creditData$Default==1])  
ad.test(creditData$amount[creditData$Default==0])  
ad.test(creditData$duration[creditData$Default==1])
ad.test(creditData$duration[creditData$Default==0])  
ad.test(creditData$installment[creditData$Default==1])  
ad.test(creditData$installment[creditData$Default==0])  
ad.test(creditData$age[creditData$Default==1])
ad.test(creditData$age[creditData$Default==0])

  # check if variances equal
  # bartlett test of homogeneity of variances 
  # (data should ideally be normal)
bartlett.test(amount ~ factor.Default, data=creditData) 
  # levene test (also for non-normal data)
leveneTest(amount ~ factor.Default, data=creditData)
  # fligner-killeen test (also for non-normal data)
fligner.test(amount ~ factor.Default, data=creditData) 

# 4. Split the data set randomly into a training set (80%) and a testing set
#    (20%).

# TRAINING SET: random sample of 80%  of total data
# TEST SET: remaining 30% of total data
training_size <- round(0.8*nrow(creditData), digits=0)
random_indices <- sample(1:nrow(creditData),training_size,replace=FALSE)
training.data <- creditData[random_indices, ] 
test.data <- creditData[-random_indices, ]

# 1 ... training sample
# 2 ... test sample
splitting <- rep(2,nrow(creditData))
splitting[random_indices] <- 1 # 80% data is training sample


# 5. Using built-in-functions in R, train all three classification methods on the
#    training data.
# 6. To describe the classification performance, create confusion matrices on
#    the test data for all three methods. Calculate the prediction accuracy and
#    misclassification rate in terms of true positives and true negatives. Which
#    model performs the best?

# LINEAR DISCRIMINANT ANALYSIS (LDA)
lda.fit <- lda(Default ~ duration+amount+installment+age, 
                         training.data) # on training.data
lda.fit
lda.predict <- predict(lda.fit, test.data)$class # on test.data
lda.predict    

plot(lda.fit, col=c("honeydew3"))
plot(lda.predict)


# QUADRATIC DISCRIMINANT ANALYSIS (QDA)
qda.fit <- qda(Default ~ duration+amount+installment+age, 
                         data=training.data)
qda.fit
qda.predict <- predict(qda.fit, test.data)$class  
plot(qda.predict)


# LDA & QDA CONFUSION MATRIX
test.predicted.lda <- predict(lda.fit, newdata=test.data)
test.predicted.qda <- predict(qda.fit, newdata=test.data)

lda.confm <- table(test.data$Default, test.predicted.lda$class)
qda.confm <- table(test.data$Default, test.predicted.qda$class)

list(LDA_model=lda.confm %>% prop.table() %>% round(3),
     QDA_model=qda.confm %>% prop.table() %>% round(3))

  # accuracy for LDA:
truepos <- length(lda.predict[lda.predict==1 & test.data$Default==0])
trueneg <- length(lda.predict[lda.predict==0 & test.data$Default==0])
falsepos <- length(lda.predict[lda.predict==1 & test.data$Default==0])
falseneg <- length(lda.predict[lda.predict==0 & test.data$Default==1])
model_acc <- (truepos+trueneg)/(truepos+trueneg+falsepos+falseneg)

  # accuracy for QDA:
trueposQ <- length(qda.predict)
truenegQ <- length(qda.predict[qda.predict==0 & test.data$Default==0])
falseposQ <- length(qda.predict[qda.predict==1 & test.data$Default==0])
falsenegQ <- length(qda.predict[qda.predict==0 & test.data$Default==1])
model_accQ <- (trueposQ+truenegQ)/(trueposQ+truenegQ+falseposQ+falsenegQ)

  # error rates for LDA & QDA:
test.data %>%
  mutate(lda.predict = (test.predicted.lda$class),
         qda.predict = (test.predicted.qda$class)) %>%
  summarise(lda.error = mean(Default != lda.predict),
            qda.error = mean(Default != qda.predict))

  # ROC curves
par(mfrow=c(1, 2))

prediction(test.predicted.lda$posterior[,2], test.data$Default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(main="ROC for LDA", col="lightpink1", lwd=2)

prediction(test.predicted.qda$posterior[,2], test.data$Default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(main="ROC for QDA", col="lightsalmon1", lwd=2)

# AUC for LDA (area under curve)
prediction(test.predicted.lda$posterior[,2], test.data$Default) %>%
  performance(measure = "auc") %>%
  .@y.values

# AUC for QDA (area under curve)
prediction(test.predicted.qda$posterior[,2], test.data$Default) %>%
  performance(measure = "auc") %>%
  .@y.values


# LOGISTIC REGRESSION:
# fit:
glm.fit <- glm(factor(training.data$Default) ~ duration+amount+installment+age, 
               data=training.data,
               family=binomial)
summary(glm.fit)

# predict:
glm.prob <- predict(glm.fit, test.data, type="response")

# confusion matrix:
table(test.data$Default, ifelse(glm.prob>0.5,"1","0"))

# accuracy:
mean(ifelse(glm.prob>0.5,"1","0")==test.data$Default)

# error rate
mean(ifelse(glm.prob>0.5,"1","0")!=test.data$Default)

# AUC for logistic regression (area under curve)
prediction(glm.prob, test.data$Default) %>%
  performance(measure = "auc") %>%
  .@y.values


# COMPARE ROC CURVES OF ALL 3 METHODS:
# LOGISTIC REGRESSION
pred.log <- prediction(glm.prob, test.data$Default) %>%
  performance(measure = "tpr", x.measure = "fpr")

# LDA
pred.lda <- prediction(test.predicted.lda$posterior[,2], test.data$Default) %>%
  performance(measure = "tpr", x.measure = "fpr")

# QDA
pred.qda <- prediction(test.predicted.qda$posterior[,2], test.data$Default) %>%
  performance(measure = "tpr", x.measure = "fpr")

# plots for pred.log, pred.lda, pred.qda
par(mfrow=c(1,1))
plot(pred.log, col = "mediumpurple1", lwd=2, main="ROC curves")
plot(pred.lda, add = TRUE, col = "orange", lwd=2)
plot(pred.qda, add = TRUE, col = "seagreen3", lwd=2)
legend("bottomright", 
       legend = c("Logistic Regression", "LDA", "QDA"), 
       col = c("mediumpurple1", "orange", "seagreen3"), 
       pch = c(17,19,20), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       inset = c(0.1, 0.1))  


# 7. Predict whether a person with the following data defaults or not dura-
#    tion=12, amount=2000, installment=4 and age=60.
df.observation <- data.frame(Default=0,duration=12,amount=2000,
                             installment=4,age=60)

# LDA:
lda.predict.observation <- predict(lda.fit, df.observation)$class  

# QDA:
qda.predict.ovservation <- predict(qda.fit, df.observation)$class  
 
# LOGISTIC REGRESSION:
glm.prob.observation <- predict(glm.fit, df.observation, type="response")


#-------------------------------------------------------------------------------------------------



