library(data.table)
library(dplyr)
library(stringr)

# Description of variables to be computed

# dependent variable (from customers table)
# acquisition: Is company in the customer table. If yes: acquired.

# time window (from purchases table)
# purchase date

# independent variabels (from registrations table):
# number of registrations
# number of days since last registration
# number of days since first registration
# range of registration date
# standard deviation in registration date
# state
# zip code prefix

# join purchases and customers on "CustomerID"
purchases_customers <- data.table(inner_join(purchases,customers))

# Specify the time window

# NOTE: 
# t1 stands for the start of the independent period, 
# t2 stands for the end of the independent period, 
# t3 stands for the start of the dependent period, and 
# t4 stands for the end of the dependent period.

# Let t4 be the maximum purchase date
(t4 <- max(purchases_customers$PurchaseDate)) # [1] "2015-08-30"

# Let the dependent period be 32 days
(t3 <- t4-32) # [1] "2015-07-31"

# Let the operational period be 1 day
(t2 <- t3 - 1) # [1] "2015-07-30"

# Let t1 be the earliest registration date
(t1 <- min(registrations$RegistrationDate)) # [1] "2014-05-31"

# Split data into independent and dependent period
registrationsIND <- registrations[RegistrationDate >= t1 & RegistrationDate <= t2,]
purchases_customersDEP <- purchases_customers[PurchaseDate > t3 & PurchaseDate <= t4,]

# Include only those companies that were not a customer at t2 (i.e., prospects). I
# In addition we also need all the prospects that did not become customers between 
# t3 and t4.

purchases_customers_before_t2 <- unique(purchases_customers[PurchaseDate <= t2,"CompanyName"])
registrationsIND <- registrationsIND[!(CompanyName %in% purchases_customers_before_t2),]

# Compute dependent
# LEFT JOIN registrationsIND and purchases_customersDEP on "CompanyName"

dependent <- 
    # LHS: the companies that were prospects
    unique(registrationsIND[,"CompanyName"]) %>%
    # RHS: the companies that became customers
    left_join(purchases_customersDEP[,"CompanyName",drop=FALSE][,Acquisition:=1]) %>% 
    data.table()

# The NA's introduced by the join represent prospects who did not 
# become customers. Thus, for these cases we set Acquisition to be 0.
dependent[,Acquisition:=as.factor(ifelse(is.na(Acquisition),0,Acquisition))]
str(dependent, vec.len=1)

table(dependent$Acquisition, useNA="ifany")
# 0    1 
# 3777 1213 

# Compute independent variabels (from registrationsIND table)
str(registrationsIND, vec.len=1)

# Compute number of registrations for each company
num_registrations <- registrationsIND[,list(num_registrations=.N),"CompanyName"]
str(num_registrations, vec.len=1)
summary(num_registrations$num_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.529   3.000   9.000 

hist(num_registrations$num_registrations)

# compute max registration date for each company
max_registration_date <- registrationsIND[,list(max_registration_date=max(RegistrationDate)),CompanyName]
str(max_registration_date, vec.len=1)

# compute how many days have past since last registration
recency_registrations <- max_registration_date[,recency_registrations := as.numeric(t2) - as.numeric(max_registration_date)][,-2]
            
summary(recency_registrations$recency_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   13.00   31.00   44.33   61.00  393.00 
hist(recency_registrations$recency_registrations)

# elapsed time since first registration
min_registration_date <- registrationsIND[,list(min_registration_date=min(RegistrationDate)), by=CompanyName]
str(min_registration_date, vec.len=1)

# duration of registration
duration_registrations <- min_registration_date[,duration_registrations := as.numeric(t2) - as.numeric(min_registration_date)][,-2]
str(duration_registrations, vec.len=1)

summary(duration_registrations$duration_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   13.00   31.00   44.39   61.00  425.00 
hist(duration_registrations$duration_registrations)

# variance in registration date?
variance_registrations <- registrationsIND[, list(variance_registration_date=var(RegistrationDate)), by=CompanyName]
variance_registrations[is.na(variance_registration_date), variance_registration_date:=0]


# number of same-day registrations
sameday_registrations <- registrationsIND[, list(sameday_registrations=max(table(RegistrationDate))), by=CompanyName]

# number of unique registrat dates?
unique_registration_dates <- registrationsIND[, list(unique_registration_dates=length(unique(RegistrationDate))), by=CompanyName]

str(sameday_registrations, vec.len=1)
summary(sameday_registrations$sameday_registrations)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00    1.00    2.00    2.49    3.00    9.00
# hist(same_registration_date$same_registration_date)

# extract state from the address string 
str(registrationsIND$CompanyAddress) # chr [1:12621] "1112 3rd Drive North, Woburn, MA 01801" ...

#remove duplicates
registrationsIND <- unique(registrationsIND[,c("CompanyName","CompanyAddress")])
str(registrationsIND, vec.len=1)

get_state <- function(address) {
    address %>% 
        str_split(",") %>% 
        lapply(X=.,FUN=function(x) x[3]) %>% 
        unlist() %>% 
        str_split(" ") %>% 
        lapply(X=.,FUN=function(x) x[2]) %>%
        unlist()
}
get_state("1112 3rd Drive North, Woburn, MA 01801") # [1] "MA"

get_zip_first_digit <- function(address) {
    address %>%
        str_split(",") %>% 
        lapply(X=.,FUN=function(x) x[3]) %>% 
        unlist() %>% 
        str_split(" ") %>% 
        lapply(X=.,FUN=function(x) x[3]) %>%
        unlist() %>%
    str_split("") %>% 
    lapply(X=.,FUN=function(x) x[1]) %>%
    unlist()
}
get_zip_first_digit("1112 3rd Drive North, Woburn, MA 01801") # [1] 0

state <- registrationsIND[,CompanyState:=get_state(CompanyAddress)][,-2]
str(state, vec.len=1)
barplot(table(state$CompanyState))

zip_first_digit <- registrationsIND[,CompanyZipFirstDigit:=get_zip_first_digit(CompanyAddress)][,-c(2,3)]
str(zip_first_digit, vec.len=1)
barplot(table(zip_first_digit$CompanyZipFirstDigit))

# join independent and dependent 
data <- list(state,
             zip_first_digit,
             duration_registrations,
             recency_registrations,
             num_registrations,
             sameday_registrations,
             variance_registrations,
             unique_registration_dates,
             dependent)
str(data, vec.len=1)

# verify dimensions match
lapply(data,dim)
# verify there are no NAs
lapply(data,anyNA)

basetable <- data.table(Reduce(inner_join, data))
str(basetable, vec.len=1)

# Store the response variable
Acquisition <- basetable$Acquisition
# Remove unnecesary columns
basetable[,c("CompanyName","Acquisition")] <- NULL
str(basetable, vec.len=1)

# Convert character columns to factor
basetable[,CompanyState:=as.factor(CompanyState)]
basetable[,CompanyZipFirstDigit:=as.factor(CompanyZipFirstDigit)]
str(basetable, vec.len=1)

#Our basetable is now ready and we can
#move on the modeling phase
