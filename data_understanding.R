# load data
customers <-read.csv("http://ballings.co/hidden/aCRM/data/chapter3/customers.csv",
                     colClasses="character")

purchases <- read.csv("http://ballings.co/hidden/aCRM/data/chapter3/purchases.csv",
             colClasses=c("character","Date","character","numeric"))

registrations <- read.csv("http://ballings.co/hidden/aCRM/data/chapter3/registrations.csv", 
             colClasses=c("character","character","character","character","Date"))

#Look at the data
str(customers, vec.len=1)
#-# 'data.frame': 2114 obs. of 5 variables:
#-# $ CustomerID : chr "1" ...
#-# $ ContactName : chr "Mbengue, Randy" ...
#-# $ CompanyName : chr "Action Wars" ...
#-# $ CompanyAddress: chr "474 Orchard Avenue, Moorhead, MN 56560" ...
#-# $ PhoneNumber : chr "8907248356" ...
str(purchases, vec.len=1)
#-# 'data.frame': 2114 obs. of 4 variables:
#-# $ CustomerID : chr "1" ...
#-# $ PurchaseDate : Date, format: "2015-02-05" ...
#-# $ NumberUsers : chr "2" ...
#-# $ PurchasePrice: num 99.9 ...
str(registrations, vec.len=1)
#-# 'data.frame': 25714 obs. of 5 variables:
#-# $ ContactName : chr "Bonner, Braydon" ...
#-# $ CompanyName : chr "Action Academy" ...
#-# $ CompanyAddress : chr "1112 3rd Drive North, Woburn, MA 01801" ...
#-# $ PhoneNumber : chr "6193847620" ...
#-# $ RegistrationDate: Date, format: "2014-05-31" ...
#Obtain information required for the ERD
#Relationship 1
#purchases and customers can be linked through CustomerID
#What are the characteristics of this relationship?
#Is CustomerID unique in both tables?
#It should be in customers,
#but we expect it won't in purchases.
length(customers$CustomerID) ==
    length(unique(customers$CustomerID))
#-# [1] TRUE
#Yes it is unique in customers
length(purchases$CustomerID) ==
    length(unique(purchases$CustomerID))
#-# [1] TRUE
#It is also unique in purchases. This means
#each customer has only made one purchase up to now.
#This is plausible since it is a new product
#Does each customer have one purchase?
all(customers$CustomerID %in% purchases$CustomerID)
#-# [1] TRUE
#Yes
#In sum
#purchases 1 CustomerID 1 customers
#1 purchase has 1 customer
#1 customer has 1 purchase
#they are linked by CustomerID
#This means that we could simply merge customers and
#purchases without aggregating
#Relationship 2:
#customers and registrations can be linked
#through CompanyName. Is CompanyName unique
#in customers?
length(customers$CompanyName) ==
    length(unique(customers$CompanyName))
#-# [1] TRUE
#Yes
#Is CompanyName unique in registrations?
length(registrations$CompanyName) ==
    length(unique(registrations$CompanyName))
#-# [1] FALSE
#No. This is was we expected since individual
#users of companies are registering
#How many registrations does a customer typically have?
reg_agg <- aggregate(registrations$CompanyName,
                     by=list(CompanyName=registrations$CompanyName),
                     length)
merged <- merge(customers[,"CompanyName",drop=FALSE],
                reg_agg,by="CompanyName",all.x=TRUE)
head(merged)
# CompanyName x
# 1      Action Wars 2
# 2     Air Missions 1
# 3       Air Square 2
# 4   Airport Family 2
# 5  Airport Hunters 3
# 6 Airport Princess 2
summary(merged$x)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max. NA's
#-# 1.000 1.000 2.000 2.614 3.000 8.000 5
#We see that there are NAs. This means that there are 0's
#We can safely say that the relationship is 0..N.
#This means there are customers that don't have registrations
#It is unclear how this could happen. Mabye a data quality problem.
#There are only 5 NAs: it might be that some companies received test
# accounts. These are the customers that don't have a registration.
merged[is.na(merged$x),]
# CompanyName  x
# 145      Backyard Boss NA
# 366        Box Brother NA
# 464      Casino Border NA
# 756 Coral Intervention NA
# 824       Dance School NA
summary(merged$x)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max. NA's
#-# 1 1 1 1 1 1 20201
#Indeed, there are NA's so this means that not every
#registration has a customer, and the only other value
#is a 1. Hence it is a 0..1 relationship.
#In sum
# customers 0..1 CompanyName 0..N registrations
#Let's have a lookg at the frequency of purchases
#and registrations over time. Registrations and purchases
# have definitely picked up.
hist(purchases$PurchaseDate,
     breaks="weeks",format = "%b %Y")
hist(registrations$RegistrationDate,
     breaks="weeks",format = "%b %Y")


range(purchases$PurchaseDate)
#-# [1] "2015-02-05" "2015-08-30"
range(registrations$RegistrationDate)
#-# [1] "2014-05-31" "2015-08-30"
#How are registration date en purchase date related?
mer <- merge(registrations[,c("RegistrationDate","CompanyName")],
             customers[,c("CompanyName","CustomerID")],by="CompanyName")
mer <- merge(mer,purchases[,c("PurchaseDate","CustomerID")],
             by="CustomerID")
str(mer,vec.len=1)
#-# 'data.frame': 5513 obs. of 4 variables:
#-# $ CustomerID : chr "1" ...
#-# $ CompanyName : chr "Action Wars" ...
#-# $ RegistrationDate: Date, format: "2015-01-04" ...
#-# $ PurchaseDate : Date, format: "2015-02-05" ...
summary(as.integer(mer$PurchaseDate- mer$RegistrationDate))
#-# Min. 1st Qu. Median Mean 3rd Qu. Max.
#-# 32.00 32.00 32.00 32.01 32.00 33.00
# This is very peculiar, this means that all employees of a company
# always registered on the exact same day. Moreover that day is always
# 32 days before the actual purchase date. It could be that there is/was
# a trial period of 32 days but it doesn't explain why all employees
# always register on the same day. Is there a script running
# in the database that resets the registration date to the minimum
# or maximum registration date within a company? Also, are these
# free trials of the premium version? Or is this the base version?
# If we have answers to these questions we potentially simplify our modeling,
# but because we haven't we cannot do that at this point.
             
             