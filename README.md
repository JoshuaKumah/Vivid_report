# Vivid_report
#This data gives an account of various crime scenes in the cities of Europe. 
#The codes to run are these
#ASSIGNMENT 2 : PART 1

# Loading of the dataArrests data set and having a variable Arrests assigned to it.
Arrests = read.csv('dataArrests.csv', header =TRUE, sep=';')
View(Arrests)
str(Arrests)
str

# These R functions should be downloaded, installed and run for effective working of the codes
library(tsoutliers)
library(ggplot2)
library(dplyr)
library(corrplot)

# Removing of missing values in the data set
any(is.na(Arrests))
Arrests=Arrests[complete.cases(Arrests),]
any(is.na(Arrests))

# Visualization of the Variables for Explanatory purposes.
# A histogram and summary function used in Visualization and summary 
hisAssault=hist(Arrests$Assault,xlab='Assault',ylab='Frequency',main = 'Assault',col='red',
                xlim=c(-10,400),ylim = c(0,400))
text(hisAssault$mids,hisAssault$counts,labels =hisAssault$counts,font = 2,adj = c(0.5,-0.5))
summary(Arrests$Assault)

hisUrbanPop=hist(Arrests$UrbanPop,xlab='UrbanPop',ylab='Frequency',main = 'UrbanPop', col='blue',
                 xlim=c(0,110),ylim = c(0,150))
text(hisUrbanPop$mids,hisUrbanPop$counts,labels =hisUrbanPop$counts,font = 2,adj = c(0.5,-0.5))
summary(Arrests$UrbanPop)

hisTraffic=hist(Arrests$Traffic,xlab='Traffic',ylab='Frequency',main = 'Traffic',col='green',
                xlim =c(500,7000),ylim=c(0,150))
text(hisTraffic$mids,hisTraffic$counts,labels =hisTraffic$counts,font = 2,adj = c(0.5,-0.5))
summary(Arrests$Traffic)

histCarAccidents=hist(Arrests$CarAccidents,xlab='CarAccidents',ylab='Frequency',main = 'CarAccident',col='yellow',
                      xlim=c(-100,6000),ylim = c(0,150))
text(histCarAccidents$mids,histCarAccidents$counts,labels =histCarAccidents$counts,font = 2,adj = c(0.5,-0.5))
summary(Arrests$CarAccidents)

histMurder=hist(Arrests$Murder,xlab='Arrests$Murder',ylab='Frequency',main = 'Murder',col='orange',
                xlim = c(0,30),ylim=c(0,200))
text(histMurder$mids,histMurder$counts,labels =histMurder$counts,font = 2,adj = c(0.5,-0.5))
summary(Arrests$Murder)

# Correlation of our variables
Correlation_Arrests=cor(Arrests)
corrplot(Correlation_Arrests,method = 'number')
ACR=abs(cor(Arrests))
corrplot(ACR,'number')
corrplot(ACR,'pie')

# Assigning our variables into dependent and independent variables
depvar = Arrests$Murder
exavar = Arrests[,2:10]

#OLS Assumption:Checking that explanatory variables are not linearly dependent
#Correlating of the independent variable and having its absolute correlation
cor(exavar)
cormat=abs(cor(exavar))
diag(cormat)=0

# A loop function to remove highly correlated variables
while (max(cormat)>=0.8) {
  
  maxvar = which(cormat==max(cormat), arr.ind = TRUE)
  
  maxavg = which.max(rowMeans(cormat[maxvar[,1],]))
  
  
  exavar = exavar[,-maxvar[maxavg,1]]
  
  cormat = cormat[-maxvar[maxavg,1],-maxvar[maxavg,1]]
  
}
str(exavar)

# Assigning the new variables of the independent to the dependent
New_Arrests = cbind(Murder=depvar,exavar)
New_Arrests
str(New_Arrests)

# Initial Model of the data set
LModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Kidnapping+Domestic+Alcohol, data=New_Arrests)
summary(LModel)

# First iteration step: Removing Kidnapping
LModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic+Alcohol, data=New_Arrests)
summary(LModel)

# Second iteration: Removing of Alcohol 
LModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic, data=New_Arrests)
summary(LModel)

# Third iteration : Removing of Domestic
LModel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber, data=New_Arrests)
summary(LModel)

# Final Model selected
Selected_Model = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber, data=New_Arrests)
summary(LModel)

# OLS Assumptions :Checking for Residual value
mean(residuals((Selected_Model)))

#OLS Assumptions : checking for Homoskedasticity
plot(residuals(Selected_Model), type='p',col='blue',ylim=c(-30,30),pch=16,
     ylab='Residuals', main='Residuals over time')
abline(a=3*sd(residuals(Selected_Model)),b=0, col='red', lty = 2)
abline(a=-3*sd(residuals(Selected_Model)),b=0, col='red', lty = 2)
abline(a=0,b=0, col='black', lty = 2)

#OLS Assumptions : Checking if Residuals are linear
cor(residuals(Selected_Model),New_Arrests$Assault)
cor(residuals(Selected_Model),New_Arrests$UrbanPop)
cor(residuals(Selected_Model),New_Arrests$Drug)
cor(residuals(Selected_Model),New_Arrests$Cyber)
cor(residuals(Selected_Model),New_Arrests$Traffic)

#OLS Assumptions: Checking for correlation between Residuals and Independent
JarqueBera.test(residuals(Selected_Model))



# ASSIGNMENT 2 : PART 2

# These R functions should be downloaded, installed and run for effective working of the codes
 
library(scales)
library(NbClust)
library(ggplot2)
library(dplyr)
library(purrr)
library(corrplot)
rm(list = ls())

# Loading of the Wholesale data set
WS = read.csv('Wholesale.csv',header = TRUE, sep = ',')
str(WS)
summary(WS)
View(WS)

#Checking for missing values
any(is.na(WS))

# Visualization of the variables

# A ggplot and summary function used for Channel and Region
ggChannel=ggplot(WS, aes(Channel))+
  geom_bar()
summary(WS$Channel)

ggRegion=ggplot(WS, aes(Region))+
  geom_bar()
summary(WS$Region)

# A histogram and summary function used for the other variables
hisFresh=hist(WS$Fresh,xlab='Fresh',ylab='Frequency',main = 'Fresh',col='red',
              ylim=c(0,300))
text(hisFresh$mids,hisFresh$counts,labels =hisFresh$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Fresh)

hisMilk=hist(WS$Milk,xlab='Milk',ylab='Frequency',main = 'Milk',col='blue',
             ylim = c(0,500))
text(hisMilk$mids,hisMilk$counts,labels =hisMilk$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Milk)

hisGrocery=hist(WS$Grocery,xlab='Grocery',ylab='Frequency',main = 'Grocery',col='yellow',
                ylim = c(0,500))
text(hisGrocery$mids,hisGrocery$counts,labels =hisGrocery$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Grocery)

hisFrozen=hist(WS$Frozen,xlab='Frozen',ylab='Frequency',main = 'Frozen',col='orange',
               ylim = c(0,500))
text(hisFrozen$mids,hisFrozen$counts,labels =hisFrozen$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Frozen)

hisDetergents_Paper=hist(WS$Detergents_Paper,xlab='Detergents_Paper',ylab='Frequency',main = 'Detergents_Paper',col='black',
                         ylim = c(0,500))
text(hisDetergents_Paper$mids,hisDetergents_Paper$counts,labels =hisDetergents_Paper$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Detergents_Paper)

hisDelicassen=hist(WS$Delicassen,xlab='Delicassen',ylab='Frequency',main = 'Delicassen',col='violet',
                   ylim = c(0,500))
text(hisDelicassen$mids,hisDelicassen$counts,labels =hisDelicassen$counts,font = 2,adj = c(0.5,-0.5))
summary(WS$Delicassen)

# Correlation of the variables
WScor=cor(WS)
corrplot(WScor,'number')

#Scaling of the data set
NorWS=apply(WS,2,rescale,to=c(0,1))
View(NorWS)
NorWS

# Using the methods for Selecting a cluster value

# K-means method on the Normalized data ('NorWS')
kmeansAlo = kmeans(NorWS,centers=2,nstart = 25) 

kmeansAlo$cluster
kmeansAlo$centers
kmeansAlo$size
kmeansAlo$withinss
kmeansAlo$tot.withinss

# A plot for variable Milk and Grocery
plot(NorWS[,4],NorWS[,5],col = kmeansAlo$cluster,pch=16,
     main='kmeansclustering',panel.first = grid())


#using the Elbow method and its plot
tot_with_ss = map_dbl(1:10,function(k){
  model = kmeans(NorWS, centers=k, nstart = 25)
  model$tot.withinss
})

plot(1:10,tot_with_ss,type='o',xlab='Number of Clusters',
     ylab = 'Total WSS', main = 'Elbow Method',panel.first = grid())

# Using the Silhouette method and its plot
SilClust = NbClust(NorWS,distance='euclidean',min.nc=2,max.nc = 10,
                   method = 'kmeans',index = 'silhouette')


plot(2:10, SilClust$All.index,type = 'o',xlab='No of clusters',main = 'silhouette method',
     ylab = 'Silhouette',panel.first = grid())

#Using the Gap Method and its plot
GapClust = NbClust(NorWS,distance='euclidean',min.nc=2,max.nc = 10,
                   method = 'kmeans',index = 'gap')

plot(2:10, GapClust$All.index,type = 'o',xlab='No of clusters',main = 'Gap Statistics',
     ylab = 'Gap Statistics',panel.first = grid())

# Using the Calinski-Harabasz method and its plot
CHClust = NbClust(NorWS,distance='euclidean',min.nc=2,max.nc = 10,
                  method = 'kmeans',index = 'ch')

plot(2:10, CHClust$All.index,type = 'o',xlab='No of clusters',main = 'Calinks Harabasz',
     ylab = 'Calinks Harabasz',panel.first = grid())


# The number of cluster selected and hence applying it to our unnormalized data set
# The K-means Alogarithm
kmeansmdl= kmeans(WS, centers = 2, nstart = 25)

#A new data set to contain the members of the cluster and our variables
datanew= WS %>% mutate(member = factor(kmeansmdl$cluster))

# Seeking to know the contribution of our variables by seeking for the mean
datanew %>%
  group_by(member) %>%
  summarise_all(list(avg = mean))

# Seeking to know the contribution of our variables by seeking for the standard deviation of the mean
datanew %>%
  group_by(member) %>%
  summarise_all(list(avg = mean, std = sd))

# Visualization of the Fresh and Grocery variables as they contributed most to our clustering group
ggplot(datanew,aes(x=Fresh,y=Grocery,col = member))+
  geom_point()+
  ggtitle('clusters in the data set')

#Visualization of the variables independently with its cluster membership
# Fresh visualization
ggplot(datanew, aes(x=Fresh,y=member,col = member))+
  geom_point()+
  ggtitle('Fresh Grouping')

# Milk visualization
ggplot(datanew, aes(x=Milk,y=member,col = member))+
  geom_point()+
  ggtitle('Milk Grouping')

# Grocery visualization
ggplot(datanew, aes(x=Grocery,y=member,col = member))+
  geom_point()+
  ggtitle('Grocery Grouping')

# Detergents visualization
ggplot(datanew, aes(x=Detergents_Paper,y=member,col = member))+
  geom_point()+
  ggtitle('Detergents_paper Grouping')

# Delicassen visualization
ggplot(datanew, aes(x=Delicassen,y=member,col = member))+
  geom_point()+
  ggtitle('Delicassen Grouping')

# Frozen visualization
ggplot(datanew, aes(x=Frozen,y=member,col = member))+
  geom_point()+
  ggtitle('Frozen Grouping')

