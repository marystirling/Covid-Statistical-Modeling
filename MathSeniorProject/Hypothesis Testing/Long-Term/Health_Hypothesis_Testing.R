# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaUNH = as.list(all$`Change UNH`)
deltaAMGN = as.list(all$`Change AMGN`)
deltaJNJ = as.list(all$`Change JNJ`)

# creating an empty list of the UNH stock before and after covid 
UNHBeforeCovid <- c()
UNHAfterCovid <- c()
counter = 0

# loop through the deltaUNH list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaUNH){
  if (i != "NA"){
    if (counter < 640){
      UNHBeforeCovid <- append(UNHBeforeCovid, i)
    } else if (counter >= 640){
      UNHAfterCovid <- append(UNHAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalUNHBeforeCovid <-as.numeric(unlist(UNHBeforeCovid))
numericalUNHAfterCovid <- as.numeric(unlist(UNHAfterCovid))


# creating an empty list of the AMGN stock before and after covid
AMGNBeforeCovid <- c()
AMGNAfterCovid <- c()
counter = 0


# loop through the deltaAMGN list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaAMGN){
  if (i != "NA"){
    if (counter < 640){
      AMGNBeforeCovid <- append(AMGNBeforeCovid, i)
    } else if (counter >= 640){
      AMGNAfterCovid <- append(AMGNAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalAMGNBeforeCovid <-as.numeric(unlist(AMGNBeforeCovid))
numericalAMGNAfterCovid <- as.numeric(unlist(AMGNAfterCovid))


# creating an empty list of the JNJ stock before and after covid
JNJBeforeCovid <- c()
JNJAfterCovid <- c()
counter = 0


# loop through the deltaJNJ list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaJNJ){
  if (i != "NA"){
    if (counter < 640){
      JNJBeforeCovid <- append(JNJBeforeCovid, i)
    } else if (counter >= 640){
      JNJAfterCovid <- append(JNJAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalJNJBeforeCovid <-as.numeric(unlist(JNJBeforeCovid))
numericalJNJAfterCovid <- as.numeric(unlist(JNJAfterCovid))


# hypothesis testing (t-test) for change in UNH
t.test(numericalUNHBeforeCovid, numericalUNHAfterCovid)

# hypothesis testing (t-test) for change in AMGN
t.test(numericalAMGNBeforeCovid, numericalAMGNAfterCovid)

# hypothesis testing (t-test) for change in JNJ
t.test(numericalJNJBeforeCovid, numericalJNJAfterCovid)
