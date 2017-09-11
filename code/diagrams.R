library(CityPlot)

download.file(url="http://ballings.co/hidden/aCRM/data/chapter3/customers.csv", 
              destfile="data/customers.csv")

download.file(url="http://ballings.co/hidden/aCRM/data/chapter3/purchases.csv", 
              destfile="data/purchases.csv")

download.file(url="http://ballings.co/hidden/aCRM/data/chapter3/registrations.csv", 
              destfile="data/registrations.csv")


data.frame(customers=names(customers))
# customers
# 1     CustomerID
# 2    ContactName
# 3    CompanyName
# 4 CompanyAddress
# 5    PhoneNumber

data.frame(purchases=names(purchases))
# purchases
# 1    CustomerID
# 2  PurchaseDate
# 3   NumberUsers
# 4 PurchasePrice

data.frame(registrations=names(registrations))
# registrations
# 1      ContactName
# 2      CompanyName
# 3   CompanyAddress
# 4      PhoneNumber
# 5 RegistrationDate

?CityPlot
CityPlot(pdfmode = "Y")
