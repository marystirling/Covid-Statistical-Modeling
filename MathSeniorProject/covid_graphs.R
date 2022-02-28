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
covid_daily_list = as.list(all$`Covid-19 Daily Increase`)
covid_daily = as.numeric(unlist(covid_daily_list))


# reads vaccine percentage of U.S as list and makes them numeric
vaccine_list = as.list(all$`% Fully Vaccinated (USA)`)
vaccine = as.numeric(unlist(vaccine_list))


# removes NA values in stock columns with previous price
for (i in 1:length(dow_list)){
  if (dow_list[[i]] == "NA"){
    dow_list[[i]] <- dow_list[[i-1]]
    sp_list[[i]] <- sp_list[[i-1]]
    nasdaq_list[[i]] <- sp_list[[i-1]]
  }
}


# converting the items in the list to a numeric value
dow <-as.numeric(unlist(dow_list))
sp <- as.numeric(unlist(sp_list))
nasdaq <- as.numeric(unlist(nasdaq_list))

limited_date <-c()

df_dow <- c()
df_covid_daily <- c()

counter = 0
for (i in dow){
  if (counter >= 581){
    df_dow <- append(df_dow,i)
  }
  counter = counter + 1
}
counter = 0
for (i in covid_daily){
  if (counter >= 581){
    df_covid_daily <- append(df_covid_daily,i)
  }
  counter = counter + 1
}

counter = 0
print(length(date))
for (i in 1:length(date)){
  if (counter >=581){
    limited_date <- append(limited_date, date[[i]])
  }
  counter = counter + 1
}

print(limited_date)
print(df_dow)
print(df_covid_daily)

df_a <- data.frame(df_covid_daily, df_dow)


df <- data.frame(covid_daily, dow)

coeff <- 12

ggplot(df, aes(x=date)) +
  
  geom_line( aes(y=covid_daily/coeff), color = "blue") + 
  geom_line( aes(y=dow), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases of COVID-19",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Dow Jones") 
  )

coeff = 15
ggplot(df_a, aes(x=limited_date)) +
  
  geom_line( aes(y=df_covid_daily/coeff), color = "blue") + 
  geom_line( aes(y=df_dow), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases of COVID-19",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Dow Jones") 
  )
f

