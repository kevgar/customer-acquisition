library("randomForest")
library("AUC")
library('lift')

# Split the data in a train and test set
set.seed(7)
ind <- 1:nrow(basetable)
indTrain <- sample(ind,round(0.5*length(ind)))
indTest <- ind[-indTrain]

# fit random forest model on training set
rf <- randomForest(x=basetable[indTrain,], y=Acquisition[indTrain])

# deploy model on the test set
pred <- predict(rf,basetable[indTest,], type="prob")[,2]

# evaluate model performance

# plot the ROC curve
plot(roc(pred,Acquisition[indTest]))

# compute the AUC
auc(roc(pred,Acquisition[indTest])) # [1] 0.8564123
# this is a good AUC

# plot the lift curve
plotLift(pred,Acquisition[indTest])

#Compute the top decile lift
TopDecileLift(pred,Acquisition[indTest]) # [1] 2.387

#We want to have some insight into our model
#and make sure the relationships are plausible
#The first step is to look at which variables
#are important
varImpPlot(rf)

# next we look at some of the relationships
partialPlot(x=rf,x.var="variance_registration_date",
            pred.data=basetable[indTest,],which.class=1)
# More variance in the registration date results 
# in a higher propensity to be acquired

partialPlot(x=rf,x.var="unique_registration_dates",
            pred.data=basetable[indTest,],which.class=1)
# More unique registration dates results 
# in a higher propensity to be acquired

partialPlot(x=rf,x.var="duration_registrations",
            pred.data=basetable[indTest,],which.class=1)
# More time since the first registration results
# in a lower propensity to be acquired

partialPlot(x=rf,x.var="recency_registrations",
            pred.data=basetable[indTest,],which.class=1)
# More time has elapsed since the last registration
# the lower the propensity to be acquired

partialPlot(x=rf,x.var="num_registrations",
            pred.data=basetable[indTest,],which.class=1)
# More registrations results in a higher propensity
# to be acquired

# The relationships match what we intuitively expect.

# In addition the lift and AUC are very good.
# Hence we can conclude the modeling phase
