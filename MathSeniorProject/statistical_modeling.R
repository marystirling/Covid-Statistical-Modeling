library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# reading columns of delta values of different stocks into a list
dow_list = as.list(all$`Dow Jones`)
sp_list = as.list(all$`S&P 500`)
nasdaq_list = as.list(all$NASDAQ)

# reads covid increase in daily cases as list and makes them numeric
covid_daily_list = as.list(all$`Covid-19 Daily Increase`)
covid_daily = as.numeric(unlist(covid_daily_list))

# reads vaccine percentage of population as list and makes them numeric
vacc_daily_list = as.list(all$`% Fully Vaccinated (USA)`)
vacc_daily = as.numeric(unlist(vacc_daily_list))


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


# creating new list for each value to get it to have start date of January 1, 2020
df_dow <- c()
df_covid_daily <- c()
df_sp <- c()
df_nasdaq <- c()
df_vacc_daily <- c()

# appends value onto new list starting from January 1, 2020 for each variable below
counter = 0
for (i in dow){
  if (counter >= 581){
    df_dow <- append(df_dow,i)
  }
  counter = counter + 1
}

counter = 0
for (i in sp){
  if (counter >= 581){
    df_sp <- append(df_sp,i)
  }
  counter = counter + 1
}

counter = 0
for (i in nasdaq){
  if (counter >= 581){
    df_nasdaq <- append(df_nasdaq,i)
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
for (i in vacc_daily){
  if (counter >= 581){
    df_vacc_daily <- append(df_vacc_daily,i)
  }
  counter = counter + 1
}
print(df_vacc_daily)


# linear regression model for Dow Jones from June 1, 2018 to December 31, 2021
dow.lm<-lm(dow ~ covid_daily + vacc_daily)
summary(dow.lm)

# linear regression model for Dow Jones from January 1, 2020 to December 31, 2021
df_dow.lm <- lm(df_dow ~ df_covid_daily + df_vacc_daily)
summary(df_dow.lm)

# linear regression model for S&P 500 from June 1, 2018 to December 31, 2021
sp.lm<-lm(sp ~ covid_daily + vacc_daily)
summary(sp.lm)

# linear regression model for S&P 500 from January 1, 2020 to December 31, 2021
df_sp.lm <- lm(df_sp ~ df_covid_daily + df_vacc_daily)
summary(df_sp.lm)


# linear regression model for Nasdaq from June 1, 2018 to December 31, 2021
nasdaq.lm<-lm(nasdaq ~ covid_daily + vacc_daily)
summary(nasdaq.lm)

# linear regression model for Nasdaq from January 1, 2020 to December 31, 2021
df_nasdaq.lm <- lm(df_nasdaq ~ df_covid_daily + df_vacc_daily)
summary(df_nasdaq.lm)
