# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')


# reading columns of delta values of different stocks into a list
deltaGS = as.list(all$`Change GS`)
deltaV = as.list(all$`Change V`)
deltaAXP = as.list(all$`Change AXP`)

# creating an empty list of the GS stock before and after covid 
GSBeforeCovid <- c()
GSAfterCovid <- c()
counter = 0

# loop through the deltaGS list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaGS){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      GSBeforeCovid <- append(GSBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      GSAfterCovid <- append(GSAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalGSBeforeCovid <-as.numeric(unlist(GSBeforeCovid))
numericalGSAfterCovid <- as.numeric(unlist(GSAfterCovid))


# creating an empty list of the V stock before and after covid
VBeforeCovid <- c()
VAfterCovid <- c()
counter = 0


# loop through the deltaV list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaV){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      VBeforeCovid <- append(VBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      VAfterCovid <- append(VAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalVBeforeCovid <-as.numeric(unlist(VBeforeCovid))
numericalVAfterCovid <- as.numeric(unlist(VAfterCovid))


# creating an empty list of the AXP stock before and after covid
AXPBeforeCovid <- c()
AXPAfterCovid <- c()
counter = 0


# loop through the deltaAXP list and add the values before and after covid, omitting invalid values of "NA"
for (i in deltaAXP){
  if (i != "NA"){
    if (counter >= 581 && counter < 640){
      AXPBeforeCovid <- append(AXPBeforeCovid, i)
    } else if (counter >= 640 && counter < 701){
      AXPAfterCovid <- append(AXPAfterCovid, i)
    }
  }
  counter = counter + 1
}


# converting the items in the list to a numeric value for hypothesis testing
numericalAXPBeforeCovid <-as.numeric(unlist(AXPBeforeCovid))
numericalAXPAfterCovid <- as.numeric(unlist(AXPAfterCovid))


# hypothesis testing (t-test) for change in GS
t.test(numericalGSBeforeCovid, numericalGSAfterCovid)

# hypothesis testing (t-test) for change in V
t.test(numericalVBeforeCovid, numericalVAfterCovid)

# hypothesis testing (t-test) for change in AXP
t.test(numericalAXPBeforeCovid, numericalAXPAfterCovid)
