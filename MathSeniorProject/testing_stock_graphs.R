library(ggplot2)
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')



# reading columns of delta values of different stocks into a list
stock_list = as.list(all$JNJ)

# reads dates in
date = as.Date(all$Date)

# reads covid increase in daily cases as list and makes them numeric
covid_daily_list = as.list(all$`Covid-19 Daily Increase`)
covid_daily = as.numeric(unlist(covid_daily_list))


# removes NA values in stock columns with previous price
for (i in 1:length(stock_list)){
  if (stock_list[[i]] == "NA"){
    stock_list[[i]] <- stock_list[[i-1]]
  }
}

# converting the items in the list to a numeric value
stock <-as.numeric(unlist(stock_list))


df_b <- data.frame(covid_daily, stock)

ggplot(df_b)+
  geom_line(aes_string(date, stock), col = 'red')
