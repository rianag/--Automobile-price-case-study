
#**********************************************************************************************************
#                                  Car Assignment - Linear regression                                           *                                                                             *
#**********************************************************************************************************                                                                                                      *

# Group Members:                                                                                         *
#1. Ria Nag                                                                                          
             

# Business objective:                                                                                     *
#A Chinese automobile company Geely Auto wants to know:
             #Which variables are significant in predicting the price of a car
             #How well those variables describe the price of a car
             #Based on various market surveys, the consulting firm has gathered a large dataset of different types of cars across the American market. 
             
             
             
#Goal of this assignment
               
#We are required to model the price of cars with the available independent variables.
#It will be used by the management to understand how exactly the prices vary with the independent variables.

# Input:                                                                                                  *                                                                                                        *
# 1. CarPrice_Assignment.csv  - a large dataset of different types of cars across the American market. 
             
#**********************************************************************************************************                                                                                                      *




#** Installing and Loading required Package **
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("stringr")
install.packages("ade4")
install.packages("MASS")
install.packages("car")

library(ade4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(MASS)
library(car)

##########Set your working directory to the directory where the "CarPrice_Assignment.csv" file exists 
setwd("C:/Users/WEL COME/Downloads")


#** Clear environment variables **
rm(list=ls())

car<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)

############DATA PREPARATION AND CLEANING###########################################################################
# there are any duplicate Id
table(duplicated(car$car_ID))
#check for blank values "" in any of the fields of dataset
which(sapply(car, function(x) length(which(x == ""))>0)==T) # Returned 0 -> No blanks

#removing columns where all values are NA values
car<-car[,colSums(is.na(car))<nrow(car)]
#removing columns where variance is 0 that is having same values throughout
car<-car[,sapply(car, function(x) length(unique(x[!(is.na(x))]))>1)]
#check for NAs
colSums(is.na(car))#<-no NAs
#Cleaning carname column to extract only the carcompany name
car$CarName<-str_replace(car$CarName, " ", "-")
car<-separate(car,CarName,c("name","model"),"-",remove=T)
#removing model data as it is not required
car<-car[,-4]
#converting all colmns to uppercase
car<-car%>%mutate_if(is.character,toupper)

#converting wrong names to right ones
car$name[which(car$name=="MAXDA")]<-"MAZDA"
car$name[which(car$name=="TOYOUTA")]<-"TOYOTA"
car$name[which(car$name=="PORCSHCE")]<-"PORSCHE"
car$name[which(car$name=="VW"|car$name=="VOKSWAGEN")]<-"VOLKSWAGEN"
#converting symboling column to factor
car$symboling<-as.factor(car$symboling)
summary(car$symboling)

#DATA manipulation
#check for outliers for numeric variables
str(car)
quantile(car$wheelbase,seq(0,1,0.01))   
quantile(car$carlength,seq(0,1,0.01))
quantile(car$carwidth,seq(0,1,0.01))  
quantile(car$carheight,seq(0,1,0.01))  
quantile(car$curbweight,seq(0,1,0.01))   
#setting values of curbweight lower than 1874.00 to  1874.00
car$curbweight[which(car$curbweight<1874.00)]<-1874.00
quantile(car$enginesize,seq(0,1,0.01)) 
#setting values of enginesize higher than  209.00 to  209.00 
car$enginesize[which(car$enginesize>209.00)]<-209.00
quantile(car$boreratio,seq(0,1,0.01))
#setting values of boreratio lower than  2.9100  to  2.9100 
car$boreratio[which(car$boreratio<2.9100 )]<-2.9100 
quantile(car$stroke,seq(0,1,0.01))
quantile(car$compressionratio,seq(0,1,0.01))
#setting values of compressionratio higher than   10.9400   to   10.9400  
car$compressionratio[which(car$compressionratio> 10.9400 )]<- 10.9400 
quantile(car$horsepower,seq(0,1,0.01))
#setting values of horsepower higher than   207.00   to   207.00  
car$horsepower[which(car$horsepower>207.00 )]<-207.00
quantile(car$peakrpm,seq(0,1,0.01))
#setting values of peakrpm higher than  6000   to   6000   
car$peakrpm[which(car$peakrpm>6000  )]<-6000 
quantile(car$citympg,seq(0,1,0.01))
#setting values of citympg higher than   38.00   to    38.00 
car$citympg[which(car$citympg> 38.00 )]<- 38.00 
quantile(car$highwaympg,seq(0,1,0.01))
#setting values of highwaympg higher than  46.92    to    46.92  
car$highwaympg[which(car$highwaympg> 46.92)]<- 46.92 


###############EDA##################################################################################################

#univariate analysis
#price
boxplot(car$price)
#CONCLUSION:median price of cars is $10,000

#name analysis
ggplot(car,aes(name))+geom_bar()
#CONCLUSION:highest number of cars in the dataset is from the brand/company Toyota

#cylindernumber analysis
ggplot(car,aes(cylindernumber))+geom_bar()
#CONCLUSION:highest number of cars in the dataset is having cylindernumber four.

#BIVARIATE ANALYSIS
boxplot(price~name,car)
boxplot(price~cylindernumber,car)
#conclusion:there is no specific pattern in relationship between car price and cylinder number
boxplot(price~carbody,car)
#conclusion:median carprices for hardtop carbody is highest among all carbody types
boxplot(price~enginelocation,car)
#conclusion:median car price  with rear engine location are higher than than those with front engine location
plot(car$peakrpm,car$price)
#conclusion:there is no specific pattern in relationship between car price and peakrpm
plot(car$curbweight,car$price)
#conclusion:car price seems to increase with increase in curbweight
boxplot(price~enginetype,car)
#conclusion:median car price for engine type dohcv is highest among all engine types


#DERIVED METRICS : bucketing car brands into 3 groups high,low and medium based on their median prices.
car_brand<-aggregate(price~name,car,median)
car_brand <- mutate(car_brand,segments=if_else(price<=10000,'low_brand',
                                                          if_else(price>10000 & price<=20000,'mid_brand','high_brands')))
#removing price variable from car_brand
car_brand<-car_brand[,-2]
car<-full_join(car,car_brand,"name")
#removing name and id variable  from car dataset as it is no longer requied 
car<-car[,-c(1,3)]
#converting charater columns to factor
car<- car%>% mutate_if(is.character,as.factor)
str(car)

#converting factor variables with more than 2 levels to dummy variables
car2<-car[,sapply(car,function(x)length(levels(x))>2)]
car3<-car[,!colnames(car)%in%colnames(car2)]

for(i in 1:ncol(car2))
{
dummy<-acm.disjonctif(data.frame(car2[,i]))
car3<-cbind(car3,dummy[,-1])
}

car<-car3

#converting factor with 2 levels to dummy variables
levels(car$fueltype) <- c(1,0)
car$fueltype <- as.numeric(levels(car$fueltype))[car$fueltype]

levels(car$aspiration) <- c(1,0)
car$aspiration <- as.numeric(levels(car$aspiration))[car$aspiration]

levels(car$doornumber) <- c(1,0)
car$doornumber <- as.numeric(levels(car$doornumber))[car$doornumber]

levels(car$enginelocation) <- c(1,0)
car$enginelocation <- as.numeric(levels(car$enginelocation))[car$enginelocation]
#DERIVED METRICS
car$height_width_ratio<-car$carheight/car$carwidth

###################################################################################################
#linear regression model

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]

############################################################################################


#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=car)

# Check the summary of model. 
summary(model_1)
# Check if the correlation matrix givessome insight.
cor<-cor(car)

step<-stepAIC(model_1, direction="both")
step
model_2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
              carlength + carwidth + carheight + curbweight + stroke + 
              horsepower + peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + 
              car2...i..HARDTOP + car2...i..HATCHBACK + car2...i..SEDAN + 
              car2...i..WAGON + car2...i..DOHCV + car2...i..L + car2...i..OHC + 
              car2...i..OHCV + car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + 
              car2...i..SIX + car2...i..THREE + car2...i..TWELVE + car2...i..low_brand + 
              car2...i..mid_brand + height_width_ratio, data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
##########################################################################################################
#removing  carheight  with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
              carlength + carwidth  + curbweight + stroke + 
              horsepower + peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + 
              car2...i..HARDTOP + car2...i..HATCHBACK + car2...i..SEDAN + 
              car2...i..WAGON + car2...i..DOHCV + car2...i..L + car2...i..OHC + 
              car2...i..OHCV + car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + 
              car2...i..SIX + car2...i..THREE + car2...i..TWELVE + car2...i..low_brand + 
              car2...i..mid_brand + height_width_ratio, data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
##########################################################################################################
#removing  height_width_ratio  with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
              carlength + carwidth  + curbweight + stroke + 
              horsepower + peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + 
              car2...i..HARDTOP + car2...i..HATCHBACK + car2...i..SEDAN + 
              car2...i..WAGON + car2...i..DOHCV + car2...i..L + car2...i..OHC + 
              car2...i..OHCV + car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + 
              car2...i..SIX + car2...i..THREE + car2...i..TWELVE + car2...i..low_brand + 
              car2...i..mid_brand , data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
##########################################################################################################
#removing  car2...i..THREE  with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
              carlength + carwidth  + curbweight + stroke + 
              horsepower + peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + 
              car2...i..HARDTOP + car2...i..HATCHBACK + car2...i..SEDAN + 
              car2...i..WAGON + car2...i..DOHCV + car2...i..L + car2...i..OHC + 
              car2...i..OHCV + car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + 
              car2...i..SIX  + car2...i..TWELVE + car2...i..low_brand + 
              car2...i..mid_brand , data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))

##########################################################################################################
#removing  carlength  with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0 + car2...i..1 + car2...i..2 +
              car2...i..HARDTOP + car2...i..HATCHBACK + car2...i..SEDAN + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L + car2...i..OHC + car2...i..OHCV + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
##########################################################################################################
#removing   wheelbase  with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation  + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..SEDAN + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L + car2...i..OHC + car2...i..OHCV + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
##########################################################################################################
#removing    car2...i..OHC   with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation  + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..SEDAN + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L + car2...i..OHCV + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
#############################################################################################################################
#removing    car2...i..SEDAN    with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation  + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0 + car2...i..1 + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L + car2...i..OHCV + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
#############################################################################################################################
#removing   car2...i..1    with high vif and high p value
model_2<-lm(formula = price ~ fueltype + enginelocation  + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
#############################################################################################################################
#removing   fueltype   with high vif and high p value
model_2<-lm(formula = price ~   enginelocation  + carwidth + curbweight + stroke + horsepower + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
#############################################################################################################################
#removing  horsepower    with high vif and high p value
model_2<-lm(formula = price ~   enginelocation  + carwidth + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE + car2...i..FOUR + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2)
#adjusted R square doesn't change much
model2_table<-data.frame(vif(model_2))
#ALL VARIABLES HAVE EITHER HIGH SIGNIFICANCE OR VIF LESS THAN 2.
#############################################################################################################################
#removing  car2...i..FOUR    with high vif
model_2_final<-lm(formula = price ~   enginelocation  + carwidth + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_2_final)
#adjusted R square doesn't change much and is equal to 0.9258
model2_final_table<-data.frame(vif(model_2_final))
#############################################################################################################################
#removing  curbweight   with high vif
model_3<-lm(formula = price ~   enginelocation  + carwidth + stroke + 
                    peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
                    car2...i..HATCHBACK + car2...i..WAGON + 
                    car2...i..DOHCV + car2...i..L +  + 
                    car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
                    car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
                  data = car)
summary(model_3)
# adjusted r square changes drastically so this model 3 is rejected
#############################################################################################################################
#removing  carwidth   with high vif
model_3<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_3)
# adjusted r square doesnot change much so this model 3 can be accepted

model_3_table<-data.frame(vif(model_3))
#############################################################################################################################
#removing  car2...i..low_brand   with high vif
model_4<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
              car2...i..TWELVE + car2...i..mid_brand, 
            data = car)
summary(model_4)
# adjusted r square changes drastically so this model 4 is rejected
#############################################################################################################################
#removing  car2...i..mid_brand   with high vif
model_5<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
              car2...i..TWELVE + car2...i..low_brand , 
            data = car)
summary(model_5)
# adjusted r square changes drastically so this model 5 is rejected

#############################################################################################################################
#LETS LOOK AT VARIABLES WITH HIGH P-VALUES of model_3
#############################################################################################################################
#removing  car2...i..TWELVE    with high p value
model_6<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
              car2...i..ROTOR + car2...i..FIVE  + car2...i..SIX + 
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_6)
# adjusted r square doesnot change much so this model 6 can be accepted

model_6_table<-data.frame(vif(model_6))
#############################################################################################################################
#removing  car2...i..ROTOR    with high p value
model_7<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..DOHCV + car2...i..L +  + 
               car2...i..FIVE  + car2...i..SIX + 
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_7)
# adjusted r square doesnot change much so this model 7 can be accepted

model_7_table<-data.frame(vif(model_7))
#############################################################################################################################
#removing  car2...i..DOHCV    with high p value
model_8<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
               car2...i..L +  
              car2...i..FIVE  + car2...i..SIX + 
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_8)
# adjusted r square doesnot change much so this model 8 can be accepted

model_8_table<-data.frame(vif(model_8))
#############################################################################################################################
#removing  car2...i..FIVE    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
                 car2...i..SIX + 
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted

model_9_table<-data.frame(vif(model_9))
#############################################################################################################################
#removing  car2...i..SIX    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2 + car2...i..HARDTOP + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted

#############################################################################################################################
#removing  car2...i..HARDTOP    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm + car2...i..0  + car2...i..2  + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted


#############################################################################################################################
#removing  car2...i..0    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm   + car2...i..2  + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted



#############################################################################################################################
#removing  car2...i..2    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight + stroke + 
              peakrpm     + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted

model_9_table<-data.frame(vif(model_9))

#############################################################################################################################
#removing  stroke    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..HATCHBACK + car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
# adjusted r square doesnot change much so this model 9 can be accepted

#############################################################################################################################
#removing  car2...i..HATCHBACK    with high p value
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
               car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
model_9_table<-data.frame(vif(model_9))
##############################################################################################################################
#EXPLANATION OF FINAL MODEL WHICH IS MODEL9
###############################################################################################################################
# adjusted r square doesnot change much so this model 9 can be accepted
#The driver variables in this model that are significant in predicting car prices are 
#enginelocation, curbweight,peakrpm, carbody type wagon, Engine Type L and the car brand.
#All variables are highly signifiant(p value less than 0.001)
#the vifs for all variables in this model are below 5
#The beta  nought coefficient or x intercept for this model is -3.627e+02
#The beta coefficients of the variables in this model are 
  
  
#enginelocation ->       -8.904e+03  
#  curbweight   ->        9.795e+00  
 # peakrpm     ->         1.314e+00  
#  car2...i..WAGON ->    -2.053e+03   
  #car2...i..L    ->     -3.059e+03  
 #car2...i..low_brand -> -1.071e+04 
#car2...i..mid_brand->  -8.837e+03 



#END OF LINEAR REGRESSION MODEL######################################################
#MODEL ESTIMATION
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.899 which is within 5% deviation from the adjusted rsquare of our final model of the value 0.9258
#also adjusted Rsquare and Rsquare of our model are pretty close of values 0.9258 and 0.9323
#thus our final model is acceptable.
###########Further model evaluation by changing testing and training datasets[BOOT STRAPPING METHOD]############################
#iteration number 1############################################################################################################
set.seed(50)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.944 which is within 5% deviation from the adjusted rsquare of our final model
#thus our final model is acceptable.
#iteration number 2############################################################################################################
set.seed(60)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.9297 which is within 5% deviation from the adjusted rsquare of our final model
#thus our final model is acceptable.
#iteration number 3############################################################################################################
set.seed(40)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.916 which is within 5% deviation from the adjusted rsquare of our final model
#thus our final model is acceptable.
#iteration number 4############################################################################################################
set.seed(70)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.9483 which is within 5% deviation from the adjusted rsquare of our final model
#thus our final model is acceptable.
#iteration number 5############################################################################################################
set.seed(30)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car), 0.7*nrow(car))
# generate the train data set
train = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car[-trainindices,]
model_9<-lm(formula = price ~   enginelocation + curbweight  + 
              peakrpm     + 
              car2...i..WAGON + 
              car2...i..L +   
              car2...i..low_brand + car2...i..mid_brand, 
            data = car)
summary(model_9)
Predict<-predict(model_9, test[, -18])
rsquared<-(cor(test$price, Predict))^2
rsquared
#r square is 0.8923 which is within 5% deviation from the adjusted rsquare of our final model
#thus our final model is acceptable.