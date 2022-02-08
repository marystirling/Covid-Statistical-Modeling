library(readxl)
all = read_excel('C:\\Users\\marys\\Downloads\\Main_Data.xlsx')
# print(all)

deltaNasdaqList = as.list(all$`Change NASDAQ`)
# print(deltaNasdaqList)

nasdaqpBeforeCovid <- c()
nasdaqAfterCovid <- c()
counter = 0

for (i in deltaNasdaqList){
  if (i != "NA"){
    if (counter < 640){
      # print(i)
      nasdaqpBeforeCovid <- append(nasdaqpBeforeCovid, i)
      # print(nasdaqpBeforeCovid)
    } else if (counter >= 640){
      # print(i)
      nasdaqAfterCovid <- append(nasdaqAfterCovid, i)
      # print(nasdaqAfterCovid)
    }
  }
  counter = counter + 1
}

# print(nasdaqpBeforeCovid)
# print(nasdaqAfterCovid)

numericalNasdaqBeforeCovid <-as.numeric(unlist(nasdaqpBeforeCovid))
# print(numericalNasdaqBeforeCovid)

numericalNasdaqAfterCovid <- as.numeric(unlist(nasdaqAfterCovid))
# print(numericalNasdaqAfterCovid)

t.test(numericalNasdaqBeforeCovid, numericalNasdaqAfterCovid)
