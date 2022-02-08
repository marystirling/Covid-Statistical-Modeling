library(readxl)
all = read_excel('C:\\Users\\marys\\Downloads\\Main_Data.xlsx')
# print(all)


deltaDowList = as.list(all$`Change Dow Jones`)
# print(deltaDowList)


dowBeforeCovid <- c()
dowAfterCovid <- c()
counter = 0

for (i in deltaDowList){
  if (i != "NA"){
    if (counter < 640){
      # print(i)
      dowBeforeCovid <- append(dowBeforeCovid, i)
      # print(beforeCovidDow)
    } else if (counter >= 640){
      # print(i)
      dowAfterCovid <- append(dowAfterCovid, i)
      # print(dowAfterCovid)
   }
  }
  counter = counter + 1
}

# print(dowBeforeCovid)
# print(dowAfterCovid)

numericalDowBeforeCovid <-as.numeric(unlist(dowBeforeCovid))
# print(numericalDowBeforeCovid)

numericalDowAfterCovid <- as.numeric(unlist(dowAfterCovid))
# print(numericalDowAfterCovid)

t.test(numericalDowBeforeCovid, numericalDowAfterCovid)
