install.packages("caTools")
library(caTools)
set.seed(100)
grade=read.csv("grade.csv")
grade
spt=sample.split(grade$PASS,0.75)
spt

train=subset(grade,spt==TRUE)
test=subset(grade,spt==FALSE)
table(train$PASS)
table(test$PASS)

str(test)
str(train)

reg1=glm(PASS~., data = train,binomial)
summary(reg1)
pchisq(32.596,29)
pchisq(24.160,27)
reg2=glm(PASS~ATTEND, data = train,binomial)
summary(reg2)
reg3=step(glm(PASS~., data = train,binomial))
pre=predict(reg2,type="response")
pre
1/(1+exp(-3.7+0.4*9))
table(train$PASS,pre>0.67)
pretest=predict(reg2,type = "response",newdata = test)
table(test$PASS, pretest>0.67)
finalregression=glm(PASS~ATTEND,data=grade,binomial)
summary(finalregression)
#------------------------Healthcare Dataset working
quality=read.csv("quality.csv")
quality
str(quality)
summary(quality)
plot(quality$Narcotics,quality$OfficeVisits,col=3-quality$PoorCare)
library(caTools)
set.seed(88)
spt=sample.split(quality$PoorCare,0.75)
train=subset(quality,spt==TRUE)
test=subset(quality,spt==FALSE)
str(train)
reg=lm(PoorCare~.,data = train)
summary(reg)
reg2=step(glm(PoorCare~.,data = train,binomial))
summary(reg2)
reg3=glm(PoorCare~.-MemberID,data = train, binomial)
summary(reg3)
reg4=glm(PoorCare ~ OfficeVisits + DaysSinceLastERVisit + StartedOnCombination + 
           AcuteDrugGapSmall,data = train, binomial)
summary(reg4)
pchisq(111.888,98)#gives p value without any variable
pchisq(76.595,94)#gives p value with reg1 variables
pre=predict(reg4,type="response")
table(train$PoorCare,pre>0.4)
pretest=predict(reg4,newdata = test,type="response")
resultPC=ifelse(pretest>0.4,1,0)
table(test$PoorCare,pretest>0.4)
table(resultPC,test$PoorCare)
install.packages("ROCR")
library(ROCR)
rocpred=prediction(pretest,test$PoorCare)
rocpref=performance(rocpred,"tpr","fpr")#ploting the graph for True & False Positive
plot(rocpref)
plot(rocpref,colorize=TRUE)#to color the output for optimizing the value
plot(rocpref,colorize=TRUE,
     print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))#to get cut off values in graph

rocpref=performance(rocpred,"tnr","fnr")#ploting the graph for True & False Negative
plot(rocpref)
plot(rocpref,colorize=TRUE)#to color the output for optimizing the value
plot(rocpref,colorize=TRUE,
     print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))#to get cut off values in graph

#to find the numeric value of Area under the curve "auc"
as.numeric(performance(rocpred,"auc")@y.values)