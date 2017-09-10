rm(list=ls())

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

system.time(result2 <- deploy_model(start_ind="2014-07-03",
                                    end_ind="2015-08-30", 
                                    model=result$model))
# Loading data..
# Creating basetable..
# Scoring prospects..
# user  system elapsed 
# 4.941   0.182  35.356 

result2$df[1:10,]
# CompanyName  prob
# 6715        Rainbow Polar 0.916
# 7386 Seashell Consignment 0.878
# 6742          Raisin Fire 0.862
# 9295        Warrior Mania 0.640
# 4951         Mango Videos 0.506
# 4913        Mango Dolphin 0.504
# 5296     Mushroom Leopard 0.494
# 8249           Sunny Room 0.492
# 5459            Myth Days 0.490
# 4905   Mango Casting Call 0.474

result2$model
# Call:
#     randomForest(x = basetable[indTrain, ], y = Acquisition[indTrain]) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 23.57%
# Confusion matrix:
#     0  1 class.error
# 0 1845 49  0.02587117
# 1  539 62  0.89683860

