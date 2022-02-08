library(readxl)
all = read_excel('C:\\Users\\marys\\Downloads\\Main_Data.xlsx')
# print(all)

deltaSPList = as.list(all$`Change S&P 500`)
# print(deltaSPList)

spBeforeCovid <- c()
spAfterCovid <- c()
counter = 0

for (i in deltaSPList){
  if (i != "NA"){
    if (counter < 640){
      # print(i)
      spBeforeCovid <- append(spBeforeCovid, i)
      # print(spBeforeCovid)
    } else if (counter >= 640){
      # print(i)
      spAfterCovid <- append(spAfterCovid, i)
      # print(spAfterCovid)
    }
  }
  counter = counter + 1
}

# print(spBeforeCovid)
# print(spAfterCovid)

numericalSPBeforeCovid <-as.numeric(unlist(spBeforeCovid))
# print(numericalSPBeforeCovid)

numericalSPAfterCovid <- as.numeric(unlist(spAfterCovid))
# print(numericalSPAfterCovid)

t.test(numericalSPBeforeCovid, numericalSPAfterCovid)
