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

print(dow_list)
print(sp_list)
print(nasdaq_list)
print(covid_daily)
print(vacc_daily)

dow.lm<-lm(dow ~ covid_daily + vacc_daily)

summary(dow.lm)
