rm(list=ls())

hdtrain=read.csv("Training_dataset.csv")#reading the file
names(hdtrain)#names of variable
nrow(hdtrain)#Number of rows 3198
colSums(is.na(hdtrain))#number of NA under each variable

hdtrain=hdtrain[ ,-1 ]#removing row id column


hdtrain=hdtrain[ ,-29]#removing homicide per 100k column 
colSums(is.na(hdtrain))
names(hdtrain)

hdtrain=hdtrain[ ,-26]#removing excessive drinking


#impute data in all the columns except homicide per 100k and excessive drinking

tempdata<- mice(hdtrain,m=1,maxit=50,meth='pmm',seed=500)

summary(tempdata)

hdtrain <- complete(tempdata,1)
nrow(hdtrain)
colSums(is.na(hdtrain))

write.csv(hdtrain,"hdtrain.csv",row.names=FALSE)#saving the imputed data in excel(csv format)


hdtrainnew=read.csv("hdtrain.csv")# reading the imputed file
names(hdtrainnew)#names of the variables


#create separate dataset, one with excessive drinking and one without excessive drinking


# predict health__pct_excessive_drinking 
#train data will have non NA values, test data will have NA values
hdtraindrinking=subset(hdtrainnew,health__pct_excessive_drinking!="NA")
nrow(hdtraindrinking)
#2220
View(hdtraindrinking)

#test data for drinking will have NA values
hdtrainnodrinking=hdtrainnew[ is.na(hdtrainnew$health__pct_excessive_drinking), ]
nrow(hdtrainnodrinking)
#978
View(hdtestdrinking)



colSums(is.na(hdtraindrinking))#no missing vales in hdtraindrinking

colSums(is.na(hdtrainnodrinking))#no missing values except excessive drinking

########################################################
#make model to predict health__pct_excessive_drinking

hdtraindrinking.lm=lm(health__pct_excessive_drinking~.
                      ,data=hdtraindrinking)

summary(hdtraindrinking.lm)
hdtraindrinking.lm.final=step(hdtraindrinking.lm)
summary(hdtraindrinking.lm.final)


pred=predict(hdtraindrinking.lm.final,newdata=hdtraindrinking)
pred

mean((hdtraindrinking$health__pct_excessive_drinking-pred)^2)
#0.00139058

pred1=predict(hdtraindrinking.lm.final,newdata=hdtrainnodrinking)

pred1

View(pred1)
write.csv(pred1,"pred1.csv")


write.csv(hdtrainnodrinking,"hdtrainnodrinking.csv")
write.csv(hdtraindrinking,"hdtraindrinking.csv")
###############################################################################


hdtest=read.csv("Test_dataset.csv")# read test dataset

colSums(is.na(hdtest))#number of NA under each column

nrow(hdtest)#number of rows 3080
names(hdtest)#list of variables
hdtest=hdtest[ ,-1 ]#removing row id column
colSums(is.na(hdtest))
names(hdtest)

hdtest=hdtest[ ,-29]#removing homicide per 100k column
colSums(is.na(hdtest))
names(hdtest)

hdtest=hdtest[ ,-26]#removing excessive drinking
colSums(is.na(hdtest))
names(hdtest)



#impute data in all the columns except homicide per 100k and excessive drinking

tempdata1<- mice(hdtest,m=1,maxit=50,meth='pmm',seed=500)

summary(tempdata1)
hdtest <- complete(tempdata,1)
nrow(hdtest)
colSums(is.na(hdtest))

write.csv(hdtest,"hdtest.csv",row.names=FALSE)

hdtestnew=read.csv("hdtest.csv")
View(hdtestnew)
names(hdtestnew)
colSums(is.na(hdtestnew))

#create separate dataset, one with excessive drinking and one without excessive drinking


# predict health__pct_excessive_drinking 
#train data will have non NA values, test data will have NA values
hdtestdrinking=subset(hdtestnew,health__pct_excessive_drinking!="NA")
nrow(hdtestdrinking)
#2220
View(hdtestdrinking)

#test data for drinking will have NA values
hdtestnodrinking=hdtestnew[ is.na(hdtestnew$health__pct_excessive_drinking), ]

nrow(hdtestnodrinking)
#860
View(hdtestnodrinking)



colSums(is.na(hdtestdrinking))#no missing vales in hdtestdrinking

colSums(is.na(hdtestnodrinking))#no missing values except excessive drinking






pred3=predict(hdtraindrinking.lm.final,hdtestdrinking)
pred3#predicted values for hd test drinking





pred4=predict(hdtraindrinking.lm.final,hdtestnodrinking)
pred4
View(hdtestnodrinking)
View(pred4)
write.csv(pred4,"pred4.csv")
write.csv(hdtestnodrinking,"hdtestnodrinking.csv")
write.csv(hdtestdrinking,"hdtestdrinking.csv")

########################################################################
#Now buidl model for  heart disease mortality

#hdtrain final is the final excel sheet with imputed data and predicted values for excessive drinking






hdtrainfinal=read.csv("hdtrainfinal.csv")
levels(hdtrainfinal$area__rucc)
levels(hdtrainfinal$area__urban_influence)
nrow(hdtrainfinal)#number of rows 3198
colSums(is.na(hdtrainfinal))

hdtrainfinal=hdtrainfinal[ , -1]#remove row id
names(hdtrainfinal)


##buidling model
hdtrainfinal.lm=lm(hdtrainfinal$heart_disease_mortality_per_100k~.
                   ,data=hdtrainfinal)

summary(hdtrainfinal.lm)
hdtrainfinalstep.lm=step(hdtrainfinal.lm)
summary(hdtrainfinalstep.lm)
#Adjusted R-squared:  0.6827


#predict for heart disease mortality in test data file
predfortrain=predict(hdtrainfinalstep.lm,hdtrainfinal)
View(predfortrain)



hdtestfinal=read.csv("hdtestfinal.csv")
nrow(hdtestfinal)# number of rows 3113
colSums(is.na(hdtestfinal))
hdtestfinal=hdtestfinal[ ,-1]#remove row id
predfinal=predict(hdtrainfinalstep.lm, hdtestfinal)
View(predfinal)

write.csv(predfinal,"predfinal.csv")


#######################################################
# 2nd approach

rm(list=ls())


hdtrain=read.csv("Training_dataset.csv")#reading the file
names(hdtrain)# namaes of variable
nrow(hdtrain)#number of rows 3198

colSums(is.na(hdtrain))# number of NA under each variable

hdtrain=hdtrain[ ,-1 ]#removing row id column

####deleting the rows of missing values#####
hdtrain=na.omit(hdtrain)
nrow(hdtrain)
# number of rows 1069

hdtrain.lm=lm(heart_disease_mortality_per_100k~.,data=hdtrain)
summary(hdtrain.lm)
hdtrainstep.lm=step(hdtrain.lm)
summary(hdtrainstep.lm)

####adjusted R square 80.09%

