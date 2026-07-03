                           #Mall Customers Project

#Motive : investigate factors that affect the customer spending behaviour and 
#score using data visualisation and analysis including descriptive stats,
#hypothesis,and ANOVA test.

#############################################################################
                             LIBRARIES
############################################################################
library(tidyverse) #this package loads everything including ggplot for graphs

##############################################################################
                              DATASET
##############################################################################
Mall_Customers <- read.csv(file.choose(), header = TRUE)
View(Mall_Customers)
# breakdown of the dataset
head(Mall_Customers,n=8) #gives first few values of dataset
str(Mall_Customers) #tells about the structure of data including no. of observations 
summary(Mall_Customers) #summary of all variables
names(Mall_Customers) #gives names of all the variables involved 

# check for any NA or missing values
colSums(is.na(Mall_Customers))

##############################################################################
                          DESCRIPTIVE STATISTICS
##############################################################################

#Age
mean(Mall_Customers$Age) #calculates the mean of age 
median(Mall_Customers$Age) #calculates the median of age
sd(Mall_Customers$Age) #calculates the standard dev of age

#Annual.Income
mean(Mall_Customers$Annual.Income..k..)#calculates the mean of annual income
median(Mall_Customers$Annual.Income..k..)#calculates the mean of annual income
sd(Mall_Customers$Annual.Income..k..)#calculates the mean of annual income

#Spending score
mean(Mall_Customers$Spending.Score..1.100.)#calculates the mean of spending score
median(Mall_Customers$Spending.Score..1.100.)#calculates the mean of spending score
sd(Mall_Customers$Spending.Score..1.100.)#calculates the mean of spending score


###############################################################################
                             VISUALISATION 
##############################################################################

#HISTOGRAM #to view the distributions of numeric variables

#Age
ggplot(Mall_Customers,aes(x=Age)) + geom_histogram(binwidth = 5)

#Annual Income
ggplot(Mall_Customers,aes(x=Annual.Income..k..)) + geom_histogram(binwidth = 5)

#spending score
ggplot(Mall_Customers,aes(x=Spending.Score..1.100.)) + geom_histogram(binwidth = 5)

#Among the three numerical variables, Spending Score appears to be more 
#evenly distributed than Age and Annual Income.

#boxplot

#gender and annual income
ggplot(Mall_Customers,aes(x=Gender,y=Annual.Income..k..)) + geom_boxplot()
#there is no significant impact of annual income based on gender as the difference
#between annual income categorised by male and female is very small.

#gender and spending score
ggplot(Mall_Customers,aes(x=Gender,y=Spending.Score..1.100.)) + geom_boxplot()

#the spending score for both the genders is same i.e 50 hence based on
#boxplot it cannot be concluded that gender affects the spending score.

#scatterplot

#age and annual income

ggplot(Mall_Customers,aes(x=Age,y=Annual.Income..k..)) + geom_point()

#there is a very weak relationship between age of customers and annual income 
#as the points are scattered far away from eachother

#age and spending score

ggplot(Mall_Customers,aes(x=Age,y=Spending.Score..1.100.)) + geom_point()

#overall weak relationship between these two numeric variables however the 
#points are not very far apart 

ggplot(Mall_Customers,aes(x=Annual.Income..k..,y=Spending.Score..1.100.)) + geom_point()

#there is no strong relationship between annual income and spending score
#since the points are scattered with no upward or downward trend.

##############################################################################
                                CORRELATION    
#############################################################################

#age and annual income

corelation_1 <- cor(Mall_Customers$Age,Mall_Customers$Annual.Income..k..)
corelation_1

#negative and weak correlation between age and annual income since negative value 
#obtained hence spending score and annual income doesnot have a strong 
#relationship.

#age and spending income 

corelation_2 <- cor(Mall_Customers$Age,Mall_Customers$Spending.Score..1.100.)
corelation_2
#negative and weak relationship

#spending income and annual income

corelation_3<- cor(Mall_Customers$Annual.Income..k..,Mall_Customers$Spending.Score..1.100.)
corelation_3

#weak but positive correlation between the two so as annual score increases
#the spending score also increases however it is weak.

###############################################################################
                                   T TEST 
##############################################################################

#ANNUAL INCOME AND GENDER

# H0: There is no significant difference in the annual income of male and
#female customers.
# H1: There is a significant difference in the annual income  of male 
#and female customers.

t.test(Annual.Income..k..~Gender,data=Mall_Customers)
#p-value = 0.4289

#the p value is greater than 0.05 hence we donot reject the hypothesis so h0 is 
#not rejected and hence there is no significant difference in annual income of
#male and female customers


#SPENDING SCORE AND GENDER

# H0: There is no significant difference in the spending score of male and
#female customers.
# H1: There is a significant difference in the spending score  of male 
#and female customers.

t.test(Spending.Score..1.100.~Gender,data=Mall_Customers)
#p-value = 0.422
#the p value is greater than 0.05 hence we donot reject the hypothesis so h0 is 
#not rejected and hence there is no significant difference in spending score of
#male and female customers.

################################################################################
                     Average Of Annual Income By Gender
################################################################################

Mall_Customers %>%
  group_by(Gender) %>%
  summarise(avg_income=mean(Annual.Income..k..))

#chr>       <dbl>
1 Female       59.2
2 Male         62.2

#this table shows numerically that male customers have slightly higher income 
#than female ones so close to no significance difference

###############################################################################
                          Average Spending Score By Gender
##############################################################################

Mall_Customers %>%
  group_by(Gender) %>%
  summarise(avg_score=mean(Spending.Score..1.100.))

#  Gender avg_score
<chr>      <dbl>
1 Female      51.5
2 Male        48.5

#the table shows the numeric values of spending scores of male and female 
#customers and females have a slightly higher spending score than males

###############################################################################
                                 CONCLUSION
###############################################################################

# The analysis suggests that:

# 1. Age and annual income have a weak negative relationship.

# 2. Annual income and spending score have a weak positive relationship.

# 3. Male and female customers do not differ significantly in annual income.

# 4. Male and female customers do not differ significantly in spending score.

# Overall, gender does not appear to be a strong factor influencing customer
# spending behaviour in this dataset.

