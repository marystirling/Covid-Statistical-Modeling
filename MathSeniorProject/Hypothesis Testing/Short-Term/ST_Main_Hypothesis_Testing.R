# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaDowList = as.list(all$`Change Dow Jones`)
deltaSPList = as.list(all$`Change S&P 500`)
deltaNasdaqList = as.list(all$`Change NASDAQ`)


# creating an empty list of the Dow Jones before and after covid 
dowBeforeCovid <- c()
dowAfterCovid <- c()
counter = 0

# loop through the deltaDowList and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaDowList){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      dowBeforeCovid <- append(dowBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      dowAfterCovid <- append(dowAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalDowBeforeCovid <-as.numeric(unlist(dowBeforeCovid))
numericalDowAfterCovid <- as.numeric(unlist(dowAfterCovid))


# creating an empty list of the S&P 500 before and after covid
spBeforeCovid <- c()
spAfterCovid <- c()
counter = 0


# loop through the deltaSPList and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaSPList){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      spBeforeCovid <- append(spBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      spAfterCovid <- append(spAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalSPBeforeCovid <-as.numeric(unlist(spBeforeCovid))
numericalSPAfterCovid <- as.numeric(unlist(spAfterCovid))


# creating an empty list of the NASDAQ before and after covid
nasdaqpBeforeCovid <- c()
nasdaqAfterCovid <- c()
counter = 0


# loop through the deltaNasdaqlist and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaNasdaqList){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      nasdaqpBeforeCovid <- append(nasdaqpBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      nasdaqAfterCovid <- append(nasdaqAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalNasdaqBeforeCovid <-as.numeric(unlist(nasdaqpBeforeCovid))
numericalNasdaqAfterCovid <- as.numeric(unlist(nasdaqAfterCovid))


# hypothesis testing (t-test) for change in the Dow
t.test(numericalDowBeforeCovid, numericalDowAfterCovid)

# hypothesis testing (t-test) for change in the S&P 500
t.test(numericalSPBeforeCovid, numericalSPAfterCovid)

# hypothesis testing (t-test) for change in the NASDAQ
t.test(numericalNasdaqBeforeCovid, numericalNasdaqAfterCovid)