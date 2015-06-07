data <- read.csv('MQIC_Detailed_slice.csv', stringsAsFactors=FALSE)
str(data)

data[,"STATE"] <- as.factor(data[,"STATE"])
data[,"AGE_CATEGORY"] <- factor(data[,"AGE_CATEGORY"], labels=c("18 to 44 years","45 to 64 years","65 to 79 years","80+ years"))
# levels(data$AGE_CATEGORY) <- c("18 to 44 years","45 to 64 years","65 to 79 years","80+ years")
data[,"GENDER"] <- as.factor(data[,"GENDER"])
data[,"DISEASE_CATEGORY"] <- factor(data[,"DISEASE_CATEGORY"], labels=c("diabetes", "hypertension"))
# levels(data$DISEASE_CATEGORY) <- c("diabetes", "hypertension")

# 1. Identify the 5 U.S. states in the dataset which have the largest number of patients
df1 <- aggregate(data$PATIENTS ~ data$STATE, data=data, FUN=sum)
df1 <- df1[order(df1[,2], decreasing=TRUE),]
head(df1, 5)

# 2. Identify the 5 U.S. states with the highest number of diabetes patients
df2 <- subset(data, data$DISEASE_CATEGORY == "diabetes")
df2 <- aggregate(df2$PATIENTS ~ df2$STATE, data=df2, FUN=sum)
df2 <- df2[order(df2[,2], decreasing=TRUE),]
head(df2, 5)

# 3. Create a single pie chart that shows the number of diabetes patients in each state
pie(df2[,2], labels=df2[,1], clockwise=TRUE)
title(main="Diabetes patients in US states", font.main=2)

# 4. Which states have the highest and lowest mean BMIs?
df4 <- subset(data, select=c(STATE, BMI_MEAN))
df4 <- df4[order(df4$BMI_MEAN),]
head(df4, 1)
tail(df4, 1)

# 5. Plot mean BMI against mean systolic blood pressure. Show your plot, and 
#    discuss whether you think they are correlated or not.
library(ggplot2)
df5 <- subset(data, select=c(BMI_MEAN, SBP_MEAN))
ggplot(data=df5, aes(x=BMI_MEAN, y=SBP_MEAN))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,size=1)

cor.test(df5$BMI_MEAN, df5$SBP_MEAN)

# 6. Plot mean BMI against mean A1C. Show your plot, and discuss whether you think they are correlated or not.
df6 <- subset(data, select=c(BMI_MEAN, A1C_MEAN))
# coef(lm(A1C_MEAN ~ BMI_MEAN, data=df6))
# ggplot(data=df6, aes(x=BMI_MEAN, y=A1C_MEAN))+
#   geom_point()+
#   geom_abline(intercept=4.51589, slope=0.07815, color="blue", size=1)

ggplot(data=df6, aes(x=BMI_MEAN, y=A1C_MEAN))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE, size=1)

cor.test(df6$BMI_MEAN, df6$A1C_MEAN)

# 7. Which age bracket has the highest frequency of hypertension?
df7 <- subset(data, DISEASE_CATEGORY=="hypertension", select=c(AGE_CATEGORY, PATIENTS))
df7 <- aggregate(PATIENTS ~ AGE_CATEGORY, data=df7, FUN=sum)
totalPatients <- aggregate(data$PATIENTS ~ data$AGE_CATEGORY, data=data, FUN=sum)
df7 <- cbind(df7,totalPatients[,2])
df7$hypertension_percentage <- (df7[,2]/df7[,3])*100
ggplot(data=df7, aes(x=AGE_CATEGORY, y=hypertension_percentage))+
  geom_bar(stat="identity")

# 8. Make a box plot (showing means, deviations) of all mean descriptors (A1C, weight, BMI, FBG, SBP, DBP)
#    for both patients with hypertension and with diabetes separately. What do you learn from these?
#library(plyr)
library(reshape2)
df8 <- subset(data, select=c(DISEASE_CATEGORY,A1C_MEAN,A1C_STDDEV,WEIGHT_MEAN,WEIGHT_STDDEV,BMI_MEAN,
                             BMI_STDDEV,FBG_MEAN,FBG_STDDEV,SBP_MEAN,SBP_STDDEV,DBP_MEAN,DBP_STDDEV))

plot_boxplot <- function(df8,y1,y2){
  df8a <- melt(df8, measure.vars = y1:y2)
  ggplot(df8a, aes(x=DISEASE_CATEGORY, y=value,fill=DISEASE_CATEGORY))+
    geom_boxplot()+
    facet_grid(.~variable)+
    labs(x="Disease category")+
    theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1))
}

plot_boxplot(df8,2,3)
plot_boxplot(df8,4,5)
plot_boxplot(df8,6,7)
plot_boxplot(df8,8,9)
plot_boxplot(df8,10,11)
plot_boxplot(df8,12,13)


# 9. Identify which of the following variables correlate: A1C_MEAN,A1C_STDDEV,WEIGHT_MEAN,WEIGHT_STDDEV,
#    BMI_MEAN,BMI_STSDEV,FBG_MEAN,FBG_STDDEV,SBP_MEAN,SBP_STDDEV,DBP_MEAN and DBP_STDDEV. 
#    Describe your results identifying which variables are highly correlated
library(corrplot)
df9 <- subset(data, select=c(A1C_MEAN,A1C_STDDEV,WEIGHT_MEAN,WEIGHT_STDDEV, BMI_MEAN,BMI_STDDEV,FBG_MEAN,FBG_STDDEV,SBP_MEAN,SBP_STDDEV,DBP_MEAN,DBP_STDDEV))
M <- cor(df9)

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(M, 0.95)

corrplot(M, type="upper", method="pie", p.mat = res1[[1]], insig="pch", order="AOE")
