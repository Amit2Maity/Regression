getwd()
baseball=read.csv("baseball.csv")
str(baseball)

plot(baseball$W,col=(3-baseball$Playoffs))

#Red => In playoff and Green=> Not in Playoff

summary(baseball$Year)

baseball1=subset(baseball,Year<2002)
baseball2=subset(baseball,Year>=2002)

str(baseball1)

plot(baseball1$W,col=(3-baseball1$Playoffs))
#RS=> Runs scored
#RA=> Runs Allowed

#Defined RD (run difference a new var)
baseball1$RD=baseball1$RS-baseball1$RA
baseball2$RD=baseball2$RS-baseball2$RA

#Finding the relation between win(dependent var) and RD(indep var)
reg=lm(W~RD,data=baseball1)
summary(reg)

#P- value is 2e-16 < 0.05 => more significant => accept Ho=> RS-RA=RD --> RS-RD>0

# we have equation W=80.881375+0.105766*RD
# condition assumed for playoff is W > 95
# 80.881375+0.105766*RD > 95
# RD > 133.4893 ~ 134 i.e. Run diff should be 135 to qualify

#Now scoring Runs relation.........

reg1=lm(RS~BA+SLG+OBP,data = baseball1)
summary(reg1)

# R^2=0.93

reg2=lm(RS~SLG+OBP,data = baseball1)
summary(reg2)

# R^2=0.9294 So includin BA R^2 value affect only by 0.006 
# therefore remove BA

# RS=-804.63+1584.91*SLG+  2737.77*OBP--------->equation1

# Runs Allowed........
reg3=lm(RA~OSLG+OOBP,data=baseball1)
summary(reg3)

# RA=-837.38+1514.29*OSLG+2913.60*OOBP---------->equation2

#From equ1 and equ2 we have to predict case for year 2002

x=subset(baseball2,Team=="OAK"&Year==2002)

#FRom equations find RS and RA for 2002
RS=-804.63+1584.91*0.432+  2737.77*0.339
#RS= 808.1552
RA=-837.38+1514.29*0.384+2913.60*0.315
#RA=661.8914

# Now RD=RS-RA
RD=808.1552-661.8914
#RD=146.2638

#from equation of W
#W=80.881375+0.105766*RD
W=80.881375+0.105766*146.2638
W=96.35111

# Now predict for all teams from baseball2 dataset for >2002 year
preRS=predict(reg2,newdata = baseball2)
preRA=predict(reg3,newdata = baseball2)

length(preRS) #no of observations

preRD=preRS-preRA

# Predict the playoffs (1=> Qualify, 0=> Not Qualify)
predictdPlayOffs=ifelse(preRD>=135,1,0)


table(baseball2$Playoffs,predictdPlayOffs)

# % of accuracy=(236+21)/330=77.87%

#Error=4/330=0.012
#Error is where team is predicted to go into playoff but it didnt


# function whose value lies between 0 and 1
x=seq(-5,5,by=0.1)
plot(1/(1+exp(-x)))


#############################################################################
#LOGISTIC REGRESSION:

getwd()
grade=read.csv("grade.csv")
str(grade)
install.packages("caTools")
library(caTools)
set.seed(100)
spt=sample.split(grade$PASS,0.75)

?sample.split()
train=subset(grade,spt=TRUE)
test=subset(grade,spt=FALSE)
table(train$PASS)
table(test$PASS)
str(test)
str(train)
reg1=glm(PASS~.,data=train,binomial)
summary(reg1)
