# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaAAPL = as.list(all$`Change AAPL`)
deltaMSFT = as.list(all$`Change MSFT`)
deltaCRM = as.list(all$`Change CRM`)

# creating an empty list of the AAPL stock before and after covid 
AAPLBeforeCovid <- c()
AAPLAfterCovid <- c()
counter = 0

# loop through the deltaAAPL list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaAAPL){
  if (i != "NA"){
    if (counter < 640){
      AAPLBeforeCovid <- append(AAPLBeforeCovid, i)
    } else if (counter >= 640){
      AAPLAfterCovid <- append(AAPLAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalAAPLBeforeCovid <-as.numeric(unlist(AAPLBeforeCovid))
numericalAAPLAfterCovid <- as.numeric(unlist(AAPLAfterCovid))


# creating an empty list of the MSFT stock before and after covid
MSFTBeforeCovid <- c()
MSFTAfterCovid <- c()
counter = 0


# loop through the deltaMSFT list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaMSFT){
  if (i != "NA"){
    if (counter < 640){
      MSFTBeforeCovid <- append(MSFTBeforeCovid, i)
    } else if (counter >= 640){
      MSFTAfterCovid <- append(MSFTAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalMSFTBeforeCovid <-as.numeric(unlist(MSFTBeforeCovid))
numericalMSFTAfterCovid <- as.numeric(unlist(MSFTAfterCovid))


# creating an empty list of the CRM stock before and after covid
CRMBeforeCovid <- c()
CRMAfterCovid <- c()
counter = 0


# loop through the deltaCRM list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaCRM){
  if (i != "NA"){
    if (counter < 640){
      CRMBeforeCovid <- append(CRMBeforeCovid, i)
    } else if (counter >= 640){
      CRMAfterCovid <- append(CRMAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalCRMBeforeCovid <-as.numeric(unlist(CRMBeforeCovid))
numericalCRMAfterCovid <- as.numeric(unlist(CRMAfterCovid))


# hypothesis testing (t-test) for change in AAPL
t.test(numericalAAPLBeforeCovid, numericalAAPLAfterCovid)

# hypothesis testing (t-test) for change in MSFT
t.test(numericalMSFTBeforeCovid, numericalMSFTAfterCovid)

# hypothesis testing (t-test) for change in CRM
t.test(numericalCRMBeforeCovid, numericalCRMAfterCovid)
