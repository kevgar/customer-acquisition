rm(list=ls())
# Overview of predict.acquire using pseudo code:
create_model <- function(start_ind, end_ind, start_dep, end_dep) {
    
    # browser() # use for debugging
    
    # Load required packages
    library(data.table, quietly = TRUE)
    library(dplyr, quietly = TRUE)
    library(stringr, quietly = TRUE)
    library(randomForest, quietly = TRUE)
    library(AUC, quietly = TRUE)
    library(lift, quietly = TRUE)
    
    get_data <- function(...){
        
        # browser() # use for debugging
        
        # load data
        cat("Loading data..\n")
        customers <<-
            data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/customers.csv",
                                colClasses="character"))
        purchases <<-
            data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/purchases.csv",
                                colClasses=c("character","Date","character","numeric")))
        registrations <<-
            data.table(read.csv("http://ballings.co/hidden/aCRM/data/chapter3/registrations.csv",
                                colClasses=c("character","character","character","character","Date")))
        
        get_state <- function(address) {
            address %>%
                str_split(",") %>%
                lapply(X=.,FUN=function(x) x[3]) %>%
                unlist() %>%
                str_split(" ") %>%
                lapply(X=.,FUN=function(x) x[2]) %>%
                unlist()
        }
        
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
        
        
        
        registrations[, state:=get_state(CompanyAddress)]
        stateLevels <- levels(as.factor(registrations$state))
        
        registrations[, zip_first_digit:=get_zip_first_digit(CompanyAddress)]
        zip_first_digitLevels <- levels(as.factor(registrations$zip_first_digit))
        
        
        
        cat("Creating basetable..\n")
        # join purchases and customers on "CustomerID"
        purchases_customers <- data.table(inner_join(purchases,customers,by = "CustomerID"))
        
        
        # if(train==TRUE){
        # Let t4 be the maximum purchase date
        t4 <- as.Date(end_dep, "%Y-%m-%d") #dump date
        # Let the dependent period be 32 days
        t3 <- as.Date(start_dep, "%Y-%m-%d")
        # Let the operational period be 1 day
        t2 <- as.Date(end_ind, "%Y-%m-%d")
        # Let t1 be the earliest registration date
        t1 <- as.Date(start_ind, "%Y-%m-%d")
        # Shift time window foreward by this ammount
        length_ind <- t2 - t1
        
        # Split data into independent and dependent period
        registrationsIND <- registrations[RegistrationDate >= t1 & RegistrationDate <= t2,]
        purchases_customersDEP <- purchases_customers[PurchaseDate >= t3 & PurchaseDate <= t4,]
        
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
            left_join(purchases_customersDEP[,"CompanyName",drop=FALSE][,Acquisition:=1], by = "CompanyName") %>%
            data.table()
        
        # The NA's introduced by the join represent prospects who did not
        # become customers. Thus, for these cases we set Acquisition to be 0.
        dependent[,Acquisition:=as.factor(ifelse(is.na(Acquisition),0,Acquisition))]
        
        # Compute independent variabels (from registrationsIND table)
        
        # Compute number of registrations for each company
        num_registrations <- registrationsIND[,list(num_registrations=.N),"CompanyName"]
        
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
        
        
        # Extract state from CompanyAddress
        state <- registrationsIND[,CompanyState:=get_state(CompanyAddress)][,-2]
        state[,CompanyState:=factor(CompanyState,levels=stateLevels)]
        
        # Extract zip_first_digit from CompanyAddress
        zip_first_digit <- registrationsIND[,CompanyZipFirstDigit:=get_zip_first_digit(CompanyAddress)][,-c(2,3)]
        zip_first_digit[,CompanyZipFirstDigit:=factor(CompanyZipFirstDigit,levels=zip_first_digitLevels)]
        
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
        data.table(Reduce(function(x,y) inner_join(x, y, by="CompanyName"), data))
        
    }
    
    # Call get_data to create basetable
    basetable <- get_data(start_indx=start_ind, end_indx=end_ind, 
                          start_depx=start_dep, end_depx=end_dep)
    
    ####################################################
    ## Predict
    ####################################################
    
    cat("Creating train and test set..\n")
    
    # Split the data in a train and test set
    set.seed(7) # for reproducability
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
    
    cat("Fitting random forest..\n")
    
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
         top.decile.lift = top.decile.lift,
         data=list(customers=customers,
                   purchases=purchases,
                   registrations=registrations)
         
         
    )
}

deploy_model <- function(start_ind, end_ind, model) {
    
    # browser() # use for debugging
    
    # Load required packages
    library(data.table, quietly = TRUE)
    library(dplyr, quietly = TRUE)
    library(stringr, quietly = TRUE)
    library(randomForest, quietly = TRUE)
    
    get_data <- function(...){
        
        # browser() # use for debugging
        
        # load data
        cat("Loading data..\n")
        customers <- result$data$customers
        purchases <- result$data$purchases
        registrations <-result$data$registrations
        
        # registrations[, state:=get_state(CompanyAddress)]
        stateLevels <- levels(as.factor(registrations$state))
        
        # registrations[, zip_first_digit:=get_zip_first_digit(CompanyAddress)]
        zip_first_digitLevels <- levels(as.factor(registrations$zip_first_digit))
        
        cat("Creating basetable..\n")
        # join purchases and customers on "CustomerID"
        purchases_customers <- data.table(inner_join(purchases,customers,by = "CustomerID"))
        
        # Let the operational period be 1 day
        t2 <- as.Date(end_ind, "%Y-%m-%d")
        # Let t1 be the earliest registration date
        t1 <- as.Date(start_ind, "%Y-%m-%d")
        
        # Select independent data
        registrationsIND <- registrations[RegistrationDate >= t1 & RegistrationDate <= t2,]
        
        # Include only those companies that were not a customer at t2 (i.e., prospects). I
        # In addition we also need all the prospects that did not become customers between
        # t3 and t4.
        
        purchases_customers_before_t2 <- unique(purchases_customers[PurchaseDate <= t2,"CompanyName"])
        registrationsIND <- registrationsIND[!(CompanyName %in% purchases_customers_before_t2),]
        # Compute independent variabels (from registrationsIND table)
        
        # Compute number of registrations for each company
        num_registrations <- registrationsIND[,list(num_registrations=.N),"CompanyName"]
        
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
        
        
        # Extract state from CompanyAddress
        state <- registrationsIND[,CompanyState:=get_state(CompanyAddress)][,c("CompanyName", "CompanyState")]
        state[,CompanyState:=factor(CompanyState,levels=stateLevels)]
        
        # Extract zip_first_digit from CompanyAddress
        zip_first_digit <- registrationsIND[,CompanyZipFirstDigit:=get_zip_first_digit(CompanyAddress)][,c("CompanyName", "CompanyZipFirstDigit")]
        zip_first_digit[,CompanyZipFirstDigit:=factor(CompanyZipFirstDigit,levels=zip_first_digitLevels)]
        
        # join independent and dependent
        data <- list(state,
                     zip_first_digit,
                     duration_registrations,
                     recency_registrations,
                     num_registrations,
                     sameday_registrations,
                     variance_registrations,
                     unique_registration_dates)
        
        # return basetable
        data.table(Reduce(function(x,y) inner_join(x, y, by="CompanyName"), data))
        
    }
    
    # Call get_data to create basetable
    basetable <- get_data(start_indx=start_ind, end_indx=end_ind)
    
    ####################################################
    ## Predict
    ####################################################
    
    df <- data.frame(CompanyName=basetable$CompanyName)
    
    # Remove unnecesary columns
    basetable[,c("CompanyName")] <- NULL
    
    # Convert character columns to factor
    basetable[,CompanyState:=as.factor(CompanyState)]
    basetable[,CompanyZipFirstDigit:=as.factor(CompanyZipFirstDigit)]
    
    cat("Scoring prospects..\n")
    
    # predict on the test set
    prob <- as.numeric(predict(model,basetable, type="prob")[,2])
    
    df$prob <- prob
    
    list(df = df[order(-df$prob),],
         model = result$model)
}





system.time(result <- create_model(start_ind="2014-05-31", # Earliest registration date
                                   end_ind="2015-07-30", # Operational period be 1 day
                                   start_dep="2015-07-31", # Dependent period be 32 days
                                   end_dep="2015-08-30" # The maximum purchase date
))
# Loading data..
# Creating basetable..
# Creating train and test set..
# Fitting random forest..
# user  system elapsed 
# 6.362   0.208  15.644 



system.time(result2 <- deploy_model(start_ind="2014-07-03", 
                                    end_ind="2015-08-30", 
                                    model=result$model))
# Loading data..
# Creating basetable..
# Scoring prospects..
# user  system elapsed 
# 3.979   0.050   3.893 





