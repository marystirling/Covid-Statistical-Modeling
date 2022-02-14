# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaHD = as.list(all$`Change HD`)
deltaMCD = as.list(all$`Change MCD`)
deltaDIS = as.list(all$`Change DIS`)

# creating an empty list of the HD stock before and after covid 
HDBeforeCovid <- c()
HDAfterCovid <- c()
counter = 0

# loop through the deltaHD list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaHD){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      HDBeforeCovid <- append(HDBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      HDAfterCovid <- append(HDAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalHDBeforeCovid <-as.numeric(unlist(HDBeforeCovid))
numericalHDAfterCovid <- as.numeric(unlist(HDAfterCovid))


# creating an empty list of the MCD stock before and after covid
MCDBeforeCovid <- c()
MCDAfterCovid <- c()
counter = 0


# loop through the deltaMCD list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaMCD){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      MCDBeforeCovid <- append(MCDBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      MCDAfterCovid <- append(MCDAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalMCDBeforeCovid <-as.numeric(unlist(MCDBeforeCovid))
numericalMCDAfterCovid <- as.numeric(unlist(MCDAfterCovid))


# creating an empty list of the DIS stock before and after covid
DISBeforeCovid <- c()
DISAfterCovid <- c()
counter = 0


# loop through the deltaDIS list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaDIS){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      DISBeforeCovid <- append(DISBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      DISAfterCovid <- append(DISAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalDISBeforeCovid <-as.numeric(unlist(DISBeforeCovid))
numericalDISAfterCovid <- as.numeric(unlist(DISAfterCovid))


# hypothesis testing (t-test) for change in HD
t.test(numericalHDBeforeCovid, numericalHDAfterCovid)

# hypothesis testing (t-test) for change in MCD
t.test(numericalMCDBeforeCovid, numericalMCDAfterCovid)

# hypothesis testing (t-test) for change in DIS
t.test(numericalDISBeforeCovid, numericalDISAfterCovid)