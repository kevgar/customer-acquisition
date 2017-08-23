
#We'll be using random forest and hence we will
#not require a validation set.
#Split the data in a train and test set
ind <- 1:nrow(basetable)
indTRAIN <- sample(ind,round(0.5*length(ind)))
indTEST <- ind[-indTRAIN]
#Load the randomForest package
if (!require('randomForest')){
    install.packages('randomForest',
                     repos='http://cran.rstudio.com',
                     quiet=TRUE)
    library("randomForest")
}
#Fit the random forest on the training set
rf <- randomForest(x=basetable[indTRAIN,],
                   y=Acquisition[indTRAIN])
#Deploy the forest on the test set
pred <- predict(rf,basetable[indTEST,],
                type="prob")[,2]
#Next we need to evaluate our model. If our model
#perfomance is not satisfactory we might need
#to go back to data modeling or even data
#preparation.

#Load the AUC package
if (!require('AUC')){
    install.packages('AUC',
                     repos='http://cran.rstudio.com',
                     quiet=TRUE)
    library("AUC")
}
#Plot the ROC curve
plot(roc(pred,Acquisition[indTEST]))

#Compute the AUC
auc(roc(pred,Acquisition[indTEST]))
#-# Error in rank(prob): argument "prob" is missing, with no default
#This is a very good AUC
#Load the lift package
if (!require('lift')){
    install.packages('lift',
                     repos='http://cran.rstudio.com',
                     quiet=TRUE)
    library('lift')
}
#Plot the lift curve
plotLift(pred,Acquisition[indTEST])

#Compute the top decile lift
TopDecileLift(pred,Acquisition[indTEST])
#-# [1] 1.967
#We want to have some insight into our model
#and make sure the relationships are plausible
#The first step is to look at which variables
#are important
varImpPlot(rf)
VAR_registration
#The second step is to look at some of the relationships
partialPlot(x=rf,x.var="VAR_registration_date",
            pred.data=basetable[indTEST,],which.class=1)
#This plot tells us that more variance in the
#registration date results in a higher
#propensity to be acquired
partialPlot(x=rf,x.var="DUR_registrations",
            pred.data=basetable[indTEST,],which.class=1)
#The more time has elapsed since the first registration
#the lower the propensity to be acquired
partialPlot(x=rf,x.var="REC_registrations",
            pred.data=basetable[indTEST,],which.class=1)

#The more time has elapsed since the last registration
#the lower the propensity to be acquired
partialPlot(x=rf,x.var="NBR_registrations",
            pred.data=basetable[indTEST,],which.class=1)

#More registrations results in a higher propensity
#to be acquired
#All the relationships are plausible and intuitive.
#In addition the lift and AUC are very good.
#Hence we can conclude the modeling phase

