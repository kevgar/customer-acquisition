get_data <- function(){
    
    # load data
    customers <- 
        data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/customers.csv",
                            colClasses="character"))
    purchases <- 
        data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/purchases.csv",
                            colClasses=c("character","Date","character","numeric")))
    registrations <- 
        data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/registrations.csv",
                            colClasses=c("character","character","character","character","Date")))
    
    # join purchases and customers on "CustomerID"
    purchases_customers <- data.table(inner_join(purchases,customers))
    
    
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
    
    # Compute independent variabels (from registrationsIND table)
    
    # Compute number of registrations for each company
    num_registrations <- registrationsIND[,list(num_registrations=.N),"CompanyName"]
    str(num_registrations, vec.len=1)
    summary(num_registrations$num_registrations)
    
    # compute max registration date for each company
    max_registration_date <- registrationsIND[,list(max_registration_date=max(RegistrationDate)),CompanyName]
    
    # compute how many days have past since last registration
    recency_registrations <- max_registration_date[,recency_registrations := as.numeric(t2) - as.numeric(max_registration_date)][,-2]
    
    # elapsed time since first registration
    min_registration_date <- registrationsIND[,list(min_registration_date=min(RegistrationDate)), by=CompanyName]
    
    # duration of registration
    duration_registrations <- min_registration_date[,duration_registrations := as.numeric(t2) - as.numeric(min_registration_date)][,-2]
    
    # variance in registration date?
    variance_registrations <- registrationsIND[, list(variance_registration_date=var(RegistrationDate)), by=CompanyName]
    variance_registrations[is.na(variance_registration_date), variance_registration_date:=0]
    
    # number of same-day registrations
    sameday_registrations <- registrationsIND[, list(sameday_registrations=max(table(RegistrationDate))), by=CompanyName]
    
    # number of unique registrat dates?
    unique_registration_dates <- registrationsIND[, list(unique_registration_dates=length(unique(RegistrationDate))), by=CompanyName]
    
    # Get unique CompanyName and CompanyAddress
    registrationsIND <- unique(registrationsIND[,c("CompanyName","CompanyAddress")])
    
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
    
    zip_first_digit <- registrationsIND[,CompanyZipFirstDigit:=get_zip_first_digit(CompanyAddress)][,-c(2,3)]
    
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
    
    # return basetable
    data.table(Reduce(inner_join, data))
    
    }

# Overview of predict.acquire using pseudo code:
deployer <- function(basetable) {
    
    # browser() # use for debugging
    
    # Load required packages
    library(data.table)
    library(dplyr)
    library(stringr)
    library(randomForest)
    library(AUC)
    library(lift)
    
    # Call get_data to create basetable
    basetable <- get_data()
    
    ####################################################
    ## Predict
    ####################################################
    
    # Split the data in a train and test set
    ind <- 1:nrow(basetable)
    indTrain <- sample(ind,round(0.5*length(ind)))
    indTest <- ind[-indTrain]
    
    # Store the response variable
    Acquisition <- basetable$Acquisition
    df <- data.frame(CompanyName=basetable$CompanyName)
    
    # Remove unnecesary columns
    basetable[,c("CompanyName","Acquisition")] <- NULL
    
    # Convert character columns to factor
    basetable[,CompanyState:=as.factor(CompanyState)]
    basetable[,CompanyZipFirstDigit:=as.factor(CompanyZipFirstDigit)]
    
    # fit model to the training set
    model <- randomForest(x=basetable[indTrain,], y=Acquisition[indTrain])
    # predict on the test set
    prob <- as.numeric(predict(model,basetable[indTest,], type="prob")[,2])
    # compute the auc
    auc <- AUC::auc(roc(prob,Acquisition[indTest]))
    # compute the decile lift
    top.decile.lift <- TopDecileLift(prob,Acquisition[indTest])
    
    df$prob <- prob
    
    list(df = df[order(-df$prob),],
         model = model,
         auc = auc,
         top.decile.lift = top.decile.lift)
    
    }

result <- deployer()

result$df[1:10,]
result$model
result$auc
result$top.decile.lift

