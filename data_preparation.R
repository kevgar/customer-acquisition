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
pur_cus <- merge(purchases,customers,by="CustomerID")
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
    registrationsIND[!registrationsIND$CompanyName %in%
                         pur_cus_bef_t2,]
#Compute dependent
#This comes down to merging registrationsIND with pur_cusDEP
#Note the left outer merge
dependent <-
    merge(data.frame(CompanyName=unique(registrationsIND[,"CompanyName"]),
                     stringsAsFactors=FALSE),
          data.frame(pur_cusDEP[,"CompanyName",drop=FALSE],Acquisition=1),
          by="CompanyName", all.x=TRUE)
dependent$Acquisition[is.na(dependent$Acquisition)] <- 0
dependent$Acquisition <- as.factor(dependent$Acquisition)
str(dependent, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName: chr "Action Academy" ...
#-# $ Acquisition: Factor w/ 2 levels "0","1": 1 1 ...
table(dependent$Acquisition, useNA="ifany")
#-#
#-# 0 1
#-# 2915 1213
#Compute predictor variables
#We can only use the registrationsIND table
str(registrationsIND, vec.len=1)
#-# 'data.frame': 10487 obs. of 5 variables:
#-# $ ContactName : chr "Bonner, Braydon" ...
#-# $ CompanyName : chr "Action Academy" ...
#-# $ CompanyAddress : chr "1112 3rd Drive North, Woburn, MA 01801" ...
#-# $ PhoneNumber : chr "6193847620" ...
#-# $ RegistrationDate: Date, format: "2014-05-31" ...
# Number of registrations
NBR_registrations <- aggregate(registrationsIND[,"CompanyName",drop=FALSE],
                               by=list(CompanyName=registrationsIND$CompanyName),
                               length)
colnames(NBR_registrations)[2] <- "NBR_registrations"
str(NBR_registrations, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ NBR_registrations: int 2 5 ...
summary(NBR_registrations$NBR_registration)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max.
#-# 1.00 1.00 2.00 2.54 3.00 9.00
hist(NBR_registrations$NBR_registrations)
# recency of registration
MAX_registration_date <-
    aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
              by=list(CompanyName=registrationsIND$CompanyName),
              max)
colnames(MAX_registration_date)[2] <- "MAX_registration_date"
str(MAX_registration_date, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ MAX_registration_date: Date, format: "2014-07-02" ...
REC_registrations <-
    data.frame(CompanyName= MAX_registration_date$CompanyName,
               REC_registrations= as.numeric(t2) -
                   as.numeric(MAX_registration_date$MAX_registration_date),
               stringsAsFactors=FALSE)
summary(REC_registrations$REC_registrations)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max.
#-# 0.00 10.00 23.00 41.03 54.00 393.00
hist(REC_registrations$REC_registrations)
# elapsed time since first registration
MIN_registration_date <-
    aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
              by=list(CompanyName=registrationsIND$CompanyName),
              min)
colnames(MIN_registration_date)[2] <- "MIN_registration_date"
str(MIN_registration_date, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ MIN_registration_date: Date, format: "2014-05-31" ...
DUR_registrations <-
    data.frame(CompanyName=MIN_registration_date$CompanyName,
               DUR_registrations= as.numeric(t2) -
                   as.numeric(MIN_registration_date$MIN_registration_date),
               stringsAsFactors=FALSE)
str(DUR_registrations, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ DUR_registrations: num 425 373 ...
summary(DUR_registrations$DUR_registrations)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max.
#-# 0.00 10.00 23.00 41.09 54.00 425.00
hist(DUR_registrations$DUR_registrations)
# variance registration time
VAR_registrations <-
    aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
              by=list(CompanyName=registrationsIND$CompanyName),
              var)
colnames(VAR_registrations)[2] <- "VAR_registration_date"
NAs <- is.na(VAR_registrations$VAR_registration_date)
VAR_registrations$VAR_registration_date[NAs] <- 0
str(VAR_registrations, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ VAR_registration_date: num 512 ...
summary(VAR_registrations$VAR_registration_date)
#-# Min. 1st Qu. Median Mean 3rd Qu. Max.
#-# 0.0000 0.0000 0.0000 0.2263 0.0000 512.0000
hist(VAR_registrations$VAR_registration_date)
# state
#randomForest can handle categorical predictors
#with up to 53 categories. Since there are only
#50 states in the US we do not need to make
#dummy variables if we only use randomForest.
str(registrationsIND$CompanyAddress)
#-# chr [1:10487] "1112 3rd Drive North, Woburn, MA 01801" ...
#remove duplicates
dups <- duplicated(registrationsIND[,c("CompanyName","CompanyAddress")])
registrationsIND <-
    registrationsIND[!dups, c("CompanyName","CompanyAddress")]
str(registrationsIND, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ CompanyAddress: chr "1112 3rd Drive North, Woburn, MA 01801" ...
pos <- unlist(gregexpr(",",registrationsIND$CompanyAddress))
pos <- pos[seq(2, length(pos), 2)]
state <- substr(registrationsIND$CompanyAddress,pos+2,pos+2+1)
state <- data.frame(registrationsIND[,"CompanyName"],
                    state,
                    stringsAsFactors=FALSE)
colnames(state)[1] <- "CompanyName"
str(state, vec.len=1)
#-# 'data.frame': 4128 obs. of 2 variables:
#-# $ CompanyName: chr "Action Academy" ...
#-# $ state : chr "MA" ...
barplot(table(state$state))
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
#-# [[1]]
#-# [1] 4128 2
#-#
#-# [[2]]
#-# [1] 4128 2
#-#
#-# [[3]]
#-# [1] 4128 2
#-#
#-# [[4]]
#-# [1] 4128 2
#-#
#-# [[5]]
#-# [1] 4128 2
#-#
#-# [[6]]
#-# [1] 4128 2
basetable <- Reduce(function(x,y) merge(x,y,by='CompanyName'),
                    data)
str(basetable, vec.len=1)
#-# 'data.frame': 4128 obs. of 7 variables:
#-# $ CompanyName : chr "Action Academy" ...
#-# $ state : chr "MA" ...
#-# $ VAR_registration_date: num 512 ...
#-# $ DUR_registrations : num 425 373 ...
#-# $ REC_registrations : num 393 331 ...
#-# $ NBR_registrations : int 2 5 ...
#-# $ Acquisition : Factor w/ 2 levels "0","1": 1 1 ...
basetable$CompanyName <- NULL
Acquisition <- basetable$Acquisition
basetable$Acquisition <- NULL
str(basetable, vec.len=1)
#-# 'data.frame': 4128 obs. of 5 variables:
#-# $ state : chr "MA" ...
#-# $ VAR_registration_date: num 512 ...
#-# $ DUR_registrations : num 425 373 ...
#-# $ REC_registrations : num 393 331 ...
#-# $ NBR_registrations : int 2 5 ...
#Any missing values?
colSums(is.na(basetable))
#-# state VAR_registration_date
#-# 0 0
#-# DUR_registrations REC_registrations
#-# 0 0
#-# NBR_registrations
#-# 0
sum(is.na(Acquisition))
#-# [1] 0
#Change state to factor for the modeling phase
basetable$state <- as.factor(basetable$state)
#Our basetable is now ready and we can
#move on the modeling phase