# reading excel and putting data into variable called "all"
# adjust path as needed to where the data is downloaded
library(ggplot2)
library(readxl)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx')



# reading columns of delta values of different stocks into a list
dow_list = as.list(all$`Dow Jones`)
sp_list = as.list(all$`S&P 500`)
nasdaq_list = as.list(all$NASDAQ)

# reads dates in
date = as.Date(all$Date)

# reads covid increase in daily cases as list and makes them numeric
vacc_daily_list = as.list(all$`% Fully Vaccinated (USA)`)
vacc_daily = as.numeric(unlist(vacc_daily_list))


# reads vaccine percentage of U.S as list and makes them numeric
vaccine_list = as.list(all$`% Fully Vaccinated (USA)`)
vaccine = as.numeric(unlist(vaccine_list))


# removes NA values in stock columns with previous price
for (i in 1:length(dow_list)){
  if (dow_list[[i]] == "NA"){
    dow_list[[i]] <- dow_list[[i-1]]
    sp_list[[i]] <- sp_list[[i-1]]
    nasdaq_list[[i]] <- nasdaq_list[[i-1]]
  }
}


# converting the items in the list to a numeric value
dow <-as.numeric(unlist(dow_list))
sp <- as.numeric(unlist(sp_list))
nasdaq <- as.numeric(unlist(nasdaq_list))

limited_date <-c()

df_dow <- c()
df_vacc_daily <- c()
df_sp <- c()
df_nasdaq <- c()

counter = 0
for (i in dow){
  if (counter >= 947){
    df_dow <- append(df_dow,i)
  }
  counter = counter + 1
}

counter = 0
for (i in sp){
  if (counter >= 947){
    df_sp <- append(df_sp,i)
  }
  counter = counter + 1
}

counter = 0
for (i in nasdaq){
  if (counter >= 947){
    df_nasdaq <- append(df_nasdaq,i)
  }
  counter = counter + 1
}


counter = 0
for (i in vacc_daily){
  if (counter >= 947){
    df_vacc_daily <- append(df_vacc_daily,i)
  }
  counter = counter + 1
}

counter = 0
print(length(date))
for (i in 1:length(date)){
  if (counter >=947){
    limited_date <- append(limited_date, date[[i]])
  }
  counter = counter + 1
}

print(limited_date)
print(df_dow)
print(df_vacc_daily)
print(df_sp)
print(df_nasdaq)

cor(df_vacc_daily, df_dow)
cor(df_vacc_daily, df_sp)
cor(df_vacc_daily, df_nasdaq)
