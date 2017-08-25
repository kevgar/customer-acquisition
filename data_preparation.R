library(data.table)
library(dplyr)
library(stringr)

#The first question to answer is always: do we need
#all available tables?
#At first sight we need all tables:
#customers: to compute the dependent,
#registrations: to compute the predictors
#purchases: for the time window (purchase date)
# Wich variables will we compute?
#dependent:
# acquisition: Is company in the customer table. If yes: acquired.
#independents:
# number of registrations
# recency of registration
# elapsed time since first registration
# variance registration time
# state
# zip code first three digits
#Because they have a 1:1 relationship, we can merge
#purchases and customers
pur_cus <- data.table(inner_join(purchases,customers,by="CustomerID"))
# Create time window
#Set t4 to the maximum purchase date
(t4 <- max(pur_cus$PurchaseDate))
#-# [1] "2015-08-30"
#The software company wants a dependent
#period of 30 days,
(t3 <- t4-30)
#-# [1] "2015-07-31"
#Use an operational period of 1 day
(t2 <- t3 - 1)
#-# [1] "2015-07-30"
#Set t1 to the minimum registration data
(t1 <- min(registrations$RegistrationDate))
#-# [1] "2014-05-31"
#Split data into independent and dependent data
registrationsIND <- registrations[registrations$RegistrationDate <= t2 &
                                      registrations$RegistrationDate >= t1,]
pur_cusDEP <- pur_cus[pur_cus$PurchaseDate > t3 &
                          pur_cus$PurchaseDate <= t4,]
#Make sure to only select companies that have not purchased before t2
pur_cus_bef_t2 <- unique(pur_cus[pur_cus$PurchaseDate <= t2,"CompanyName"])
registrationsIND <-
    registrationsIND[!(registrationsIND$CompanyName %in%
                         pur_cus_bef_t2),]
#Compute dependent
#This comes down to merging registrationsIND with pur_cusDEP
#Note the left outer merge
dependent <- 
    unique(registrationsIND[,"CompanyName"]) %>% 
    left_join(pur_cusDEP[,"CompanyName",drop=FALSE][,Acquisition:=1]) %>% 
    data.table()

dependent[,Acquisition:=as.factor(ifelse(is.na(Acquisition),0,Acquisition))]

str(dependent, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName: chr  "Action Academy" ...
# $ Acquisition: Factor w/ 2 levels "0","1": 1 1 ...
# - attr(*, ".internal.selfref")=<externalptr> 
table(dependent$Acquisition, useNA="ifany")
# Note: freq 0s does not match original solution
# 0    1 
# 3777 1213 

#Compute predictor variables
#We can only use the registrationsIND table
str(registrationsIND, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	12621 obs. of  5 variables:
#     $ ContactName     : chr  "Bonner, Braydon" ...
# $ CompanyName     : chr  "Action Academy" ...
# $ CompanyAddress  : chr  "1112 3rd Drive North, Woburn, MA 01801" ...
# $ PhoneNumber     : chr  "6193847620" ...
# $ RegistrationDate: Date, format: "2014-05-31" ...
# - attr(*, ".internal.selfref")=<externalptr> 

NBR_registrations <- registrationsIND[,list(NBR_registrations=.N),"CompanyName"]

str(NBR_registrations, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName      : chr  "Action Academy" ...
# $ NBR_registrations: int  2 5 ...
# - attr(*, ".internal.selfref")=<externalptr> 
summary(NBR_registrations$NBR_registration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.529   3.000   9.000 
hist(NBR_registrations$NBR_registrations)
# recency of registration
MAX_registration_date <-
    registrationsIND[,list(MAX_registration_date=max(RegistrationDate)),CompanyName]

str(MAX_registration_date, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName          : chr  "Action Academy" ...
# $ MAX_registration_date: Date, format: "2014-07-02" ...
# - attr(*, ".internal.selfref")=<externalptr> 

REC_registrations <-
    MAX_registration_date[,REC_registrations := as.numeric(t2) - 
                              as.numeric(MAX_registration_date)][,-2]
            
summary(REC_registrations$REC_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   13.00   31.00   44.33   61.00  393.00 
hist(REC_registrations$REC_registrations)

# elapsed time since first registration
MIN_registration_date <- registrationsIND[,list(MIN_registration_date=min(RegistrationDate)),
                                          by=CompanyName]


str(MIN_registration_date, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName          : chr  "Action Academy" ...
# $ MIN_registration_date: Date, format: "2014-07-02" ...
# - attr(*, ".internal.selfref")=<externalptr> 

# duration of registration
DUR_registrations <-
    MIN_registration_date[,DUR_registrations := as.numeric(t2) - 
                              as.numeric(MIN_registration_date)][,-2]

str(DUR_registrations, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName      : chr  "Action Academy" ...
# $ DUR_registrations: num  425 373 ...
# - attr(*, ".internal.selfref")=<externalptr> 
summary(DUR_registrations$DUR_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   13.00   31.00   44.39   61.00  425.00 
hist(DUR_registrations$DUR_registrations)
# variance registration time
VAR_registrations <- 
    registrationsIND[, list(VAR_registration_date=var(RegistrationDate)),
                     by=CompanyName]
    
VAR_registrations[is.na(VAR_registration_date),VAR_registration_date:=0]

str(VAR_registrations, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName          : chr  "Action Academy" ...
# $ VAR_registration_date: num  512 ...
# - attr(*, ".internal.selfref")=<externalptr> 
summary(VAR_registrations$VAR_registration_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.1882   0.0000 512.0000 
hist(VAR_registrations$VAR_registration_date)
# state
#randomForest can handle categorical predictors
#with up to 53 categories. Since there are only
#50 states in the US we do not need to make
#dummy variables if we only use randomForest.
str(registrationsIND$CompanyAddress)
#-# chr [1:10487] "1112 3rd Drive North, Woburn, MA 01801" ...
#remove duplicates
registrationsIND <- unique(registrationsIND[,c("CompanyName","CompanyAddress")])
str(registrationsIND, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName   : chr  "Action Academy" ...
# $ CompanyAddress: chr  "1112 3rd Drive North, Woburn, MA 01801" ...
# - attr(*, ".internal.selfref")=<externalptr> 

get_state <- function(address) {
    address %>% 
        str_split(",") %>% 
        lapply(X=.,FUN=function(x) x[3]) %>% 
        unlist() %>% 
        str_split(" ") %>% 
        lapply(X=.,FUN=function(x) x[2]) %>%
        unlist()
    }

state <- registrationsIND[,CompanyState:=get_state(CompanyAddress)][,-2]


str(state, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  2 variables:
#     $ CompanyName : chr  "Action Academy" ...
# $ CompanyState: chr  "MA" ...
# - attr(*, ".internal.selfref")=<externalptr> 
barplot(table(state$CompanyState))
# Merge independents and dependent
data <- list(state,
             VAR_registrations,
             DUR_registrations,
             REC_registrations,
             NBR_registrations,
             dependent)
str(data, vec.len=1)
#-# List of 6
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName: chr [1:4128] "Action Academy" ...
#-# ..$ state : chr [1:4128] "MA" ...
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName : chr [1:4128] "Action Academy" ...
#-# ..$ VAR_registration_date: num [1:4128] 512 ...
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName : chr [1:4128] "Action Academy" ...
#-# ..$ DUR_registrations: num [1:4128] 425 373 ...
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName : chr [1:4128] "Action Academy" ...
#-# ..$ REC_registrations: num [1:4128] 393 331 ...
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName : chr [1:4128] "Action Academy" ...
#-# ..$ NBR_registrations: int [1:4128] 2 5 ...
#-# $ :'data.frame': 4128 obs. of 2 variables:
#-# ..$ CompanyName: chr [1:4128] "Action Academy" ...
#-# ..$ Acquisition: Factor w/ 2 levels "0","1": 1 1 ...
# Look at the dimensions
# ?Number of instances should be identical
lapply(data,dim)
# [[1]]
# [1] 4990    2
# 
# [[2]]
# [1] 4990    2
# 
# [[3]]
# [1] 4990    2
# 
# [[4]]
# [1] 4990    2
# 
# [[5]]
# [1] 4990    2
# 
# [[6]]
# [1] 4990    2
basetable <- Reduce(function(x,y) merge(x,y,by='CompanyName'),
                    data)


str(basetable, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  7 variables:
#     $ CompanyName          : chr  "Action Academy" ...
# $ CompanyState         : chr  "MA" ...
# $ VAR_registration_date: num  512 ...
# $ DUR_registrations    : num  425 373 ...
# $ REC_registrations    : num  393 331 ...
# $ NBR_registrations    : int  2 5 ...
# $ Acquisition          : Factor w/ 2 levels "0","1": 1 1 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#     - attr(*, "sorted")= chr "CompanyName"
basetable$CompanyName <- NULL
Acquisition <- basetable$Acquisition
basetable$Acquisition <- NULL
str(basetable, vec.len=1)
# Classes ‘data.table’ and 'data.frame':	4990 obs. of  5 variables:
#     $ CompanyState         : chr  "MA" ...
# $ VAR_registration_date: num  512 ...
# $ DUR_registrations    : num  425 373 ...
# $ REC_registrations    : num  393 331 ...
# $ NBR_registrations    : int  2 5 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#Any missing values?
colSums(is.na(basetable))
# CompanyState VAR_registration_date     DUR_registrations 
# 0                     0                     0 
# REC_registrations     NBR_registrations 
# 0                     0 
sum(is.na(Acquisition))
# [1] 0
#Change state to factor for the modeling phase
basetable[,CompanyState:=as.factor(CompanyState)]
#Our basetable is now ready and we can
#move on the modeling phase

