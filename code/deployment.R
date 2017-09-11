rm(list=ls())
# acquireR reads in the data, prepares the predictors and
# the dependent variable, evaluates the model and creates
# the final model.
# There are six parameters:
# start_ind Character string. The start of the independent period ("%m/%d/%Y")
# end_ind Character string. The end of the independent period ("%m/%d/%Y")
# start_dep Character string. The start of the dependent period ("%m/%d/%Y")
# end_dep Character string. The end of the dependent period ("%m/%d/%Y")
# evaluate Boolean. Should the model be evaluated?
# An overview of the function using pseudo code:
# acquireR <- function(start_ind,
# end_ind,
# start_dep,
# end_dep,
# evaluate=TRUE){
#
#
# Load required packages
#
# Create function readAndPrepareData as follows
# readAndPrepareData(train=TRUE,...){
# read in data
# create predictors
# if train==TRUE return predictors and dependent, otherwise predictors
# }
#
# Call readAndPrepareData
#
# If evaluate==TRUE create train and test and evaluate model
#
# Create model on all data
#
# Store in a list:
# the model
# the readAndPrepareData function
# the format of the dates
# the length of the independent period
#
# Set that list to class "acquireR"
#
# Return that list
# }


acquireR <- function(start_ind,
                     end_ind,
                     start_dep,
                     end_dep,
                     evaluate=TRUE) {
    f <- "%m/%d/%Y"
    t1 <- as.Date(start_ind, f)
    t2 <- as.Date(end_ind, f)
    t3 <- as.Date(start_dep, f)
    t4 <- as.Date(end_dep, f) #dump date
    length_ind <- t2 - t1
    #Load all required packages
    for (i in c("AUC","lift","randomForest")) {
        if (!require(i,character.only=TRUE,quietly=TRUE)) {
            install.packages(i,
                             repos='http://cran.rstudio.com',
                             quiet=TRUE)
            require(i,
                    character.only=TRUE,
                    quietly=TRUE)
        }
    }
    readAndPrepareData <- function(train=TRUE,...){
        #DATA UNDERSTANDING
        ###############################################################
        cat("Reading in data:")
        time <- Sys.time()
        #We can read the data using the read.csv function
        customers <-
            read.csv("http://ballings.co/hidden/aCRM/data/chapter3/customers.csv",
                     header=TRUE,
                     colClasses="character")
        purchases <-
            read.csv("http://ballings.co/hidden/aCRM/data/chapter3/purchases.csv",
                     header=TRUE,
                     colClasses=c("character",
                                  "Date",
                                  "character",
                                  "numeric"))
        registrations <-
            read.csv("http://ballings.co/hidden/aCRM/data/chapter3/registrations.csv",
                     header=TRUE,
                     colClasses=c("character",
                                  "character",
                                  "character",
                                  "character",
                                  "Date"))
        if (train==TRUE){
            #To avoid error message in predict:
            #New factor levels not present in the training data
            pos <- unlist(gregexpr(",",registrations$CompanyAddress))
            pos <- pos[seq(2, length(pos), 2)]
            state <- substr(registrations$CompanyAddress,pos+2,pos+2+1)
            levelsState <- levels(as.factor(state))
        }
        cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
            attr(Sys.time()- time,"units"), "\n")
        cat("Preparing basetable:")
        time <- Sys.time()
        ###############################################################
        #DATA PREPARATION
        #Because they have a 1:1 relationship, we can merge
        #purchases and customers
        pur_cus <- merge(purchases,customers,by="CustomerID")
        # When we are calling readAndPrepareData in predict:
        if (train==FALSE){
            dots <- list(...) # list(...) evaluates all arguments and
            # returns them in a named list
            t2 <- dots$end_ind
            t1 <- t2 - dots$length_ind
            rm(dots)
        }
        #Otherwise just fetch timewindow from
        #surrounding environment
        #Split data into independent and dependent data
        registrationsIND <- registrations[registrations$RegistrationDate <= t2 &
                                              registrations$RegistrationDate >= t1,]
        if (train==TRUE){
            pur_cusDEP <- pur_cus[pur_cus$PurchaseDate > t3 &
                                      pur_cus$PurchaseDate <= t4,]
        }
        #Make sure to only select companies that have not purchased before t2
        pur_cus_bef_t2 <- unique(pur_cus[pur_cus$PurchaseDate <= t2,"CompanyName"])
        registrationsIND <-
            registrationsIND[!registrationsIND$CompanyName %in%
                                 pur_cus_bef_t2,]
        if (train==TRUE){
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
        }
        #Compute predictor variables
        #We can only use the registrationsIND table
        # Number of registrations
        NBR_registrations <- aggregate(registrationsIND[,"CompanyName",drop=FALSE],
                                       by=list(CompanyName=registrationsIND$CompanyName),
                                       length)
        colnames(NBR_registrations)[2] <- "NBR_registrations"
        # recency of registration
        MAX_registration_date <-
            aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
                      by=list(CompanyName=registrationsIND$CompanyName),
                      max)
        colnames(MAX_registration_date)[2] <- "MAX_registration_date"
        REC_registrations <-
            data.frame(CompanyName= MAX_registration_date$CompanyName,
                       REC_registrations= as.numeric(t2) -
                           as.numeric(MAX_registration_date$MAX_registration_date),
                       stringsAsFactors=FALSE)
        # elapsed time since first registration
        MIN_registration_date <-
            aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
                      by=list(CompanyName=registrationsIND$CompanyName),
                      min)
        colnames(MIN_registration_date)[2] <- "MIN_registration_date"
        DUR_registrations <-
            data.frame(CompanyName=MIN_registration_date$CompanyName,
                       DUR_registrations= as.numeric(t2) -
                           as.numeric(MIN_registration_date$MIN_registration_date),
                       stringsAsFactors=FALSE)
        # variance registration time
        VAR_registrations <-
            aggregate(registrationsIND[,"RegistrationDate",drop=FALSE],
                      by=list(CompanyName=registrationsIND$CompanyName),
                      var)
        colnames(VAR_registrations)[2] <- "VAR_registration_date"
        NAs <- is.na(VAR_registrations$VAR_registration_date)
        VAR_registrations$VAR_registration_date[NAs] <- 0
        # state
        #remove duplicates
        dups <- duplicated(registrationsIND[,c("CompanyName","CompanyAddress")])
        registrationsIND <-
            registrationsIND[!dups, c("CompanyName","CompanyAddress")]
        pos <- unlist(gregexpr(",",registrationsIND$CompanyAddress))
        pos <- pos[seq(2, length(pos), 2)]
        state <- substr(registrationsIND$CompanyAddress,pos+2,pos+2+1)
        state <- data.frame(registrationsIND[,"CompanyName"],
                            state,
                            stringsAsFactors=FALSE)
        #Change state to factor for the modeling phase
        state$state <- as.factor(state$state)
        if (train==TRUE){
            #To avoid error in predict:
            #New factor levels not present in the training data
            levels(state$state) <- levelsState
        }
        colnames(state)[1] <- "CompanyName"
        # Merge independents and dependent
        if (train==TRUE){
            data <- list(state,
                         VAR_registrations,
                         DUR_registrations,
                         REC_registrations,
                         NBR_registrations,
                         dependent)
        } else {
            data <- list(state,
                         VAR_registrations,
                         DUR_registrations,
                         REC_registrations,
                         NBR_registrations)
        }
        basetable <- Reduce(function(x,y) merge(x,y,by='CompanyName'),
                            data)
        if (train==TRUE){
            basetable$CompanyName <- NULL
            Acquisition <- basetable$Acquisition
            basetable$Acquisition <- NULL
        }
        cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
            attr(Sys.time()- time,"units"), "\n")
        #Our basetable is now ready and we can
        #move on the modeling phase
        if (train==TRUE){
            return(list(predictors=basetable, Acquisition=Acquisition))
        } else {
            return(basetable)
        }
    }#end readAndPrepareData
    basetable <- readAndPrepareData()
    ###############################################################
    
    #MODELING
    if (evaluate==TRUE){
        cat("Evaluating model:")
        time <- Sys.time()
        #We'll be using random forest and hence we will
        #not require a validation set.
        #Split the data in a train and test set
        ind <- 1:nrow(basetable$predictors)
        indTRAIN <- sample(ind,round(0.5*length(ind)))
        indTEST <- ind[-indTRAIN]
        #Fit the random forest on the training set
        rf <- randomForest(x=basetable$predictors[indTRAIN,],
                           y=basetable$Acquisition[indTRAIN])
        #Deploy the forest on the test set
        pred <- predict(rf,basetable$predictors[indTEST,],
                        type="prob")[,2]
        cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
            attr(Sys.time()- time,"units"), "\n")
        cat(" Number of predictors:", ncol(basetable$predictors[indTRAIN,]),
            "predictors\n")
        cat(" AUROC:",
            round(auc(roc(pred,basetable$Acquisition[indTEST])),4),"\n")
        cat(" Top decile lift of:",
            round(TopDecileLift(pred,basetable$Acquisition[indTEST]),4),"\n")
    }
    cat("Creating model:")
    time <- Sys.time()
    #Build model on all data
    rf <- randomForest(x=basetable$predictors,
                       y=basetable$Acquisition)
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    l <- list(rf = rf,
              readAndPrepareData = readAndPrepareData,
              f = f,
              length_ind = length_ind)
    class(l) <- "acquireR"
    return(l)
}

acquisitionModel <-
    acquireR(start_ind="05/31/2014",
             end_ind="07/30/2015",
             start_dep="07/31/2015",
             end_dep="08/30/2015",
             evaluate=TRUE)
#-# Reading in data: 3.6 secs
#-# Preparing basetable: 0.3 secs
#-# Evaluating model: 0.8 secs
#-# Number of predictors: 5 predictors
#-# Error in rank(prob): argument "prob" is missing, with no default














# predict.acquireR reads in the data, prepares the predictors and
# creates the predictions
# There are three parameters:
# object The output of the acquireR function
# dumpDate Character string. The date that the data was extracted
# from the database ("%m/%d/%Y")
# Overview of predict.acquire using pseudo code:
# predict.acquireR <- function(object, dumpDate) {
#
# Load required packages
#
# Call object$readAndPrepareData function with train=FALSE
# to create predictors
#
# Predict
#
# Store company name and predictions in data frame
#
# Sort by predictions
#
# Return the data frame
# }
predict.acquireR <- function(object, dumpDate) {
    #Load all required packages
    for (i in c("AUC","lift","randomForest")) {
        if (!require(i,character.only=TRUE,quietly=TRUE)) {
            install.packages(i,
                             repos='http://cran.rstudio.com',
                             quiet=TRUE)
            require(i,
                    character.only=TRUE,
                    quietly=TRUE)
        }
    }
    #Make sure all variables in the readAndPrepareData
    #enclosing environment (where it was defined) are removed
    #to avoid unexpected results
    environment(object$readAndPrepareData) <- environment()
    basetable <- object$readAndPrepareData(train=FALSE,
                                           end_ind= as.Date(dumpDate, object$f),
                                           length_ind=object$length_ind)
    cat("Predicting: ")
    time <- Sys.time()
    ans <- data.frame(CompanyName=basetable$CompanyName,
                      Score=predict(object=object$rf,
                                    newdata=basetable,
                                    type="prob")[,2])
    ans <- ans[order(ans$Score, decreasing=TRUE),]
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    ans
}
pred <-
    predict(object=acquisitionModel,
            dumpDate="08/30/2015")
#-# Error in predict(object = acquisitionModel, dumpDate = "08/30/2015"): object ’acquisitionModel’ not found
head(pred)
#-# Error in head(pred): object ’pred’ not found