#Set Working Directory
setwd("C:\\Users\\rohan\\Downloads")

#Import real estate dataset
real <- read.csv('RealEstate.csv',header=T,stringsAsFactors=F)

#EDA to choose the house locations
library(data.table)
real <- data.table(real)
eda <- real[,.(Avg_price = mean(Price),House_count = length(MLS)),by=.(Location)]
real <- data.frame(real)

#Subset the data for SVM
real <- real[which(real$Location %in% c('Arroyo Grande','Lompoc')),]
real <- real[,c('Location','Price.SQ.Ft','Size')]

#Create training and test dataset
## 75% of the sample size
smp_size <- floor(0.80 * nrow(real))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(real)), size = smp_size)

train <- real[train_ind, ]
test <- real[-train_ind, ]

#Import library
library(ggplot2)

#Scatter Plot
ggplot(train,aes(y=Size,x=Price.SQ.Ft,color=Location)) +
  geom_point() +  labs(x = 'Size in Sq. Ft.',y='Price per Sq.Ft.')

#Apply SVM on training set
library(e1071)
class.f <- train[,'Location']
model <- svm(Location~.,data=train,kernel='linear',type='C-classification',scale='FALSE')

#Plot
#Get parameters of hyperplane
w <- t(model$coefs) %*% model$SV
b <- -model$rho

#Equation of hyperplane => w[1,1]*x + w[1,2]*y + b = 0

#Plot the data with the separating hyperplane
ggplot(train,aes(y=Size,x=Price.SQ.Ft,color=Location)) +
  geom_point() +  
  geom_abline(intercept=-b/w[1,2],slope=-w[1,1]/w[1,2],colour='blue',linetype='dashed')+
  labs(x = 'Size in Sq. Ft.',y='Price per Sq.Ft.')
  


