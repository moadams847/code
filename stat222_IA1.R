### data loading------------------------------------------------------------------------
##-------------------------------------------------------------------------------------

setwd("E:/Data Analytics/Stat222_IA/STAT222-master") ## workig from this directory.

gh_apparel<- read.csv("GHApparel.csv", 
                stringsAsFactors = FALSE, header = T) ### read the csv file from the 
#### working directory

### the structure of the data-------------------------
str(gh_apparel)

###### view the data in spread sheet format------------------
View(gh_apparel)


######random sampling from the dataset-----------------------------------------------

#the code means select 400 values out of a sequence of integers from 1 to 500
set.seed(123)
sampling <- sample(500, 400)

str(sampling)   ### the structre of the sequence

####### subset with random sequence generated above-----------------------------
gh_apparel_sample<-gh_apparel[sampling,] ### now using the sequence to select a random
##sample of 400 from the original data set

str(gh_apparel_sample)  #### view the structure of the data set
View(gh_apparel_sample) #### view in spread sheet form

######## question (i)---------------------------------------------------------------------
####--------------------------------------------------------------------------------------

######## DATA EXPLORATION------
sapply(gh_apparel_sample, class) ### displays class of each variable

### it can be concluded that the data set has four categorical variables
### and four continous variablele

### Exploring the categorical variables.
attach(gh_apparel_sample) ### allows us to call features from the dataset easily.

#### 1 method of payment feature -------------

## table for method of payment
Method.of.Payment_table<-table(Method.of.Payment)
Method.of.Payment_table

#### percentage for method of payment
Method.of.Payment_prop<-prop.table(Method.of.Payment_table)*100
Method.of.Payment_prop

#### from the table and percentages it can be concluded that majority used discover  
### while a few of the customers used Proprietary Card

###### barplot
install.packages('ggplot2') ### install the ggplot 2 package
library(ggplot2)
ggplot(gh_apparel_sample,aes(x=Method.of.Payment, fill=Method.of.Payment))+geom_bar()

#### it is also clear from the barplot that discover was used more by customers 

###### 2 type of customer feature-------------

## table
custom_type<-table(gh_apparel_sample$Type.of.Customer)

## percentage
custom_type_percebtage<-prop.table(custom_type)*100

### barchart
ggplot(gh_apparel_sample,aes(x=Type.of.Customer, fill=Type.of.Customer))+geom_bar()

##### alot of customers made purchase using using the discount coupon.


###### 3 Gender featurer-----------------------

####### table
gender_custome<-table(gh_apparel_sample$Gender)

###percentage
gender_custome_percentage<-prop.table(gender_custome)*100

### chart
ggplot(gh_apparel_sample,aes(x=Gender, fill=Gender))+geom_bar()

##### there were more males than females customers

####### 4 Marital.Status feature ---------------------------------

## table
Marital.Status_table<-table(gh_apparel_sample$Marital.Status)

## percentages
marital.Status_percentages<-prop.table(Marital.Status_table)*100

#### chart
ggplot(gh_apparel_sample,aes(x=Marital.Status, fill=Marital.Status))+geom_bar()

##### there are lots of divorced customers in the dataset

### Exploring the  continous variables----------------------------------

###### 1 items feature
attach(gh_apparel_sample)

hist(gh_apparel_sample$Items)##### histogram for net sales
hist(Items,freq = F)
plot(density(Items)) ##### density plot of net sales

##### the item column has subgroups
#### alot of customers bought items less than 2 followed by 
#### those who bought more than 10 items but less than 12

##### 2 Net.Sales feature
hist(Net.Sales)  ##### histogram for net sales
hist(Net.Sales,freq =F)
plot(density(Net.Sales)) ##### density plot of net sales

#### the net sales feature has subgroups
### it peaks at points(100,200 and 240)

#### 3 Age feature
hist(gh_apparel_sample$Age) ##### histogram for net sales
plot(density(gh_apparel_sample$Age)) ##### density plot of net sales

#### age also has subgroups 
### there are alot of people above 80 but below 100
### and followed by those who are age 40
#### most of the customers are those with old age

###### summary of the entire dataset
summary(gh_apparel_sample)

################# quesion (ii)----------------------------------------------------
###-------------------------------------------------------------------------------

####charts showing the number of customer purchases 
##### attributable to the method of payment.

#### group mean of items by method of payment
means<-aggregate(gh_apparel_sample$Items,
                 by=list(as.factor(gh_apparel_sample$Method.of.Payment)), 
                 FUN = mean)  

means <- means[order(means$x),] ## sorts from lowest to highest

##### barplot of items by method of payment
barplot(means$x,names.arg = means$Group.1, ### adds names to each bar
        main = "Mean Illiteracy Rate", col = c(4,6,11,10),
        ylab = "AVERAGE RATE")


##### ANOTHER VERSION OF THE PLOT

###### boxplot of items sold given method of payment.
library(ggplot2)
gh_apparel_sample$Method.of.Payment <- as.factor(gh_apparel_sample$Method.of.Payment)
qplot(gh_apparel_sample$Method.of.Payment, gh_apparel_sample$Items, 
      data=gh_apparel_sample, 
      geom=c("boxplot"),
      main="Box plots with superimposed data points",
      xlab= "payment method",
      ylab="The total number of items purchased",  fill=gh_apparel_sample$Method.of.Payment)


##### on average the total number of items purchased with Proprietary Card
##### is the highest with visa being the least


##### question(iii)---------------------------------------------------------------
#####-----------------------------------------------------------------------------

means<-aggregate(gh_apparel_sample$Net.Sales,
                 by=list(as.factor(gh_apparel_sample$Type.of.Customer)), 
                 FUN = mean) 
#### the average net sales given by type of customer is higher for promotional
## than for regular
## hence there is no similarity


##### question(iv)--------------------------------------------------------------------
######--------------------------------------------------------------------------------

attach(gh_apparel_sample) ### alows us to call columns from the data set easily
plot(Net.Sales,Age)

##### there is no clear relationship between customer age and net sales.


########## question (v)-------------------------------------------------------------
########-----------------------------------------------------------------------------

###The distribution of age by payment method.

gh_apparel_sample$Method.of.Payment <- as.factor(gh_apparel_sample$Method.of.Payment)

qplot(gh_apparel_sample$Method.of.Payment, gh_apparel_sample$Age, 
      data=gh_apparel_sample, 
      geom=c("boxplot"),
      main="Box plots with superimposed data points",
      xlab= "payment method",
      ylab="The age of the customers",  fill=gh_apparel_sample$Method.of.Payment)


##### all methods of payment is common among the aged but
#### the average age given by method of payment is higher for discover and
### low given Proprietary Card 


####### question (vi)----------------------------------------------------------------
#####--------------------------------------------------------------------------------

means<-aggregate(gh_apparel_sample$Net.Sales,
                 by=list(as.factor(gh_apparel_sample$Gender)), 
                 FUN = mean) 

##### the average sales of men is rather greater than that of women.

##### question(vii)---------------------------------------------------------------------
########--------------------------------------------------------------------------------

means<-aggregate(gh_apparel_sample$Net.Sales,
                 by=list(as.factor(gh_apparel_sample$Type.of.Customer)), 
                 FUN = mean) 

### it is false, the average sales of regular customer type is rather
####GHC7 less than that of promotional customer type

####### question(viii)-----------------------------------------------------------------
#####---------------------------------------------------------------------------------

aggregate(gh_apparel_sample$Net.Sales, by=list(gh_apparel_sample$Method.of.Paymen), 
          FUN = mean) ##group mean

aggregate(gh_apparel_sample$Net.Sales, by=list(gh_apparel_sample$Method.of.Payment),
          FUN = sd)  ##group standard deviation.

####  anova for net sales explained by method of payment
fit <- aov(gh_apparel_sample$Net.Sales ~ gh_apparel_sample$Method.of.Payment) 

# since the p>alapha value we fail to reject Ho.
#it provides evidence that the four methods of payment
# are all equal.

summary(fit) ### summary of the fit
##since theere is no significant difference among method of payment
#GHApparel should go into agreement 
#with any of them.

####### question(ix)-------------------------------------------------------------
#####----------------------------------------------------------------------------

aggregate(gh_apparel_sample$Net.Sales, by=list(gh_apparel_sample$Method.of.Paymen), 
          FUN = mean) ##group means

##### The net sales given the method of payment with least average sales is visa
### thus going by the group means.





