# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaBA = as.list(all$`Change BA`)
deltaCAT = as.list(all$`Change CAT`)
deltaHON = as.list(all$`Change HON`)

# creating an empty list of the BA stock before and after covid 
BABeforeCovid <- c()
BAAfterCovid <- c()
counter = 0

# loop through the deltaBA list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaBA){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      BABeforeCovid <- append(BABeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      BAAfterCovid <- append(BAAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalBABeforeCovid <-as.numeric(unlist(BABeforeCovid))
numericalBAAfterCovid <- as.numeric(unlist(BAAfterCovid))


# creating an empty list of the CAT stock before and after covid
CATBeforeCovid <- c()
CATAfterCovid <- c()
counter = 0


# loop through the deltaCAT list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaCAT){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      CATBeforeCovid <- append(CATBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      CATAfterCovid <- append(CATAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalCATBeforeCovid <-as.numeric(unlist(CATBeforeCovid))
numericalCATAfterCovid <- as.numeric(unlist(CATAfterCovid))


# creating an empty list of the HON stock before and after covid
HONBeforeCovid <- c()
HONAfterCovid <- c()
counter = 0


# loop through the deltaHON list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaHON){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      HONBeforeCovid <- append(HONBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      HONAfterCovid <- append(HONAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalHONBeforeCovid <-as.numeric(unlist(HONBeforeCovid))
numericalHONAfterCovid <- as.numeric(unlist(HONAfterCovid))


# hypothesis testing (t-test) for change in BA
t.test(numericalBABeforeCovid, numericalBAAfterCovid)

# hypothesis testing (t-test) for change in CAT
t.test(numericalCATBeforeCovid, numericalCATAfterCovid)

# hypothesis testing (t-test) for change in HON
t.test(numericalHONBeforeCovid, numericalHONAfterCovid)
