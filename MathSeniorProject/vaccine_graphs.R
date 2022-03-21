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

df_a <- data.frame(df_vacc_daily, df_dow)


df <- data.frame(vacc_daily, dow)
df_sp_main <- data.frame(df_vacc_daily, df_sp)
df_nasdaq_main <- data.frame(df_vacc_daily, df_nasdaq)

# Dow Jones scaled graph
a <- range(df_vacc_daily)
print(a)
print(a[2])
b <- range(df_dow)
print(b)
print(b[2])
scale_factor <- diff(a)/diff(b)
print(scale_factor)
# df_dow <- ((b[2] - b[1]) * scale_factor) + a[1]

for (i in 1:length(df_dow)){
  df_dow[i] <- ((df_dow[i] - b[1]) * scale_factor) + a[1]
}
trans <- ~ ((. - a[1]) / scale_factor) + b[1]
print(df_dow)

ggplot(df_a) +
  geom_line(aes_string(limited_date, df_vacc_daily), col = "blue") + 
  geom_line(aes_string(limited_date, df_dow), col='red') + 
  scale_y_continuous(name = "Percentage of U.S. Population Fully Vaccinated", 
                     sec.axis = sec_axis(trans=trans, name="Dow Jones Industrial Average"))+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

# S&P 500 scaled data graph 
a <- range(df_vacc_daily)
print(a)
print(a[2])
b <- range(df_sp)
print(b)
print(b[2])
scale_factor <- diff(a)/diff(b)
print(scale_factor)


for (i in 1:length(df_dow)){
  df_sp[i] <- ((df_sp[i] - b[1]) * scale_factor) + a[1]
}
trans <- ~ ((. - a[1]) / scale_factor) + b[1]
print(df_sp)

ggplot(df_sp_main) +
  geom_line(aes_string(limited_date, df_vacc_daily), col = "blue") + 
  geom_line(aes_string(limited_date, df_sp), col='red') + 
  scale_y_continuous(name = "Percentage of U.S. Population Fully Vaccinated", 
                     sec.axis = sec_axis(trans=trans, name="S&P 500"))+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

# S&P 500 scaled data graph 
a <- range(df_vacc_daily)
print(a)
print(a[2])
b <- range(df_sp)
print(b)
print(b[2])
scale_factor <- diff(a)/diff(b)
print(scale_factor)


for (i in 1:length(df_dow)){
  df_sp[i] <- ((df_sp[i] - b[1]) * scale_factor) + a[1]
}
trans <- ~ ((. - a[1]) / scale_factor) + b[1]
print(df_sp)

ggplot(df_sp_main) +
  geom_line(aes_string(limited_date, df_vacc_daily), col = "blue") + 
  geom_line(aes_string(limited_date, df_sp), col='red') + 
  scale_y_continuous(name = "Percentage of U.S. Population Fully Vaccinated", 
                     sec.axis = sec_axis(trans=trans, name="S&P 500"))+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

# NASDAQ scaled data graph 
a <- range(df_vacc_daily)
print(a)
print(a[2])
b <- range(df_nasdaq)
print(b)
print(b[2])
scale_factor <- diff(a)/diff(b)
print(scale_factor)


for (i in 1:length(df_dow)){
  df_nasdaq[i] <- ((df_nasdaq[i] - b[1]) * scale_factor) + a[1]
}
trans <- ~ ((. - a[1]) / scale_factor) + b[1]
print(df_nasdaq)

ggplot(df_nasdaq_main) +
  geom_line(aes_string(limited_date, df_vacc_daily), col = "blue") + 
  geom_line(aes_string(limited_date, df_nasdaq), col='red') + 
  scale_y_continuous(name = "Percentage of U.S. Population Fully Vaccinated", 
                     sec.axis = sec_axis(trans=trans, name="NASDAQ Composite"))+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 



coeff <- 12

ggplot(df, aes(x=date)) +
  
  geom_line( aes(y=vacc_daily), color = "blue") + 
  geom_line( aes(y=dow/((b[2]-b[1]*scale_factor) +a[1])), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases of COVID-19",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ((. - a[1]) / scale_factor) + b[1], name="Dow Jones") 
  ) +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("")

ggplot(df_a, aes(x=limited_date)) +
  
  geom_line( aes(y=df_vacc_daily), color = "blue") + 
  geom_line( aes(y=df_dow/((b[2]-b[1]*scale_factor) +a[1])), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Percentage of U.S. Population Fully Vaccinated",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ((. - a[1]) / scale_factor) + b[1], name="Dow Jones")
    
  ) +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

ggplot(df_a) +
  geom_line(aes_string(limited_date, df_vacc_daily), col = "blue") + 
  geom_line(aes_string(limited_date, df_dow), col='red') + 
  scale_y_continuous(name = "Percentage of U.S. Population Fully Vaccinated", 
                     sec.axis = sec_axis(trans=trans, name="Dow Jones"))+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

coeff = 100000
ggplot(df_a, aes(x=limited_date)) +
  
  geom_line( aes(y=df_vacc_daily), color = "blue") + 
  geom_line( aes(y=df_dow/coeff), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Percentage of U.S. Population Fully Vaccinated",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Dow Jones")
    
  ) +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("") 

coeff = 10000
ggplot(df_sp_main, aes(x=limited_date)) +
  
  geom_line( aes(y=df_vacc_daily), color = "blue") + 
  geom_line( aes(y=df_sp/coeff), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Percentage of U.S. Population Fully Vaccinated",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="S&P 500") 
    
  )+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("")

coeff = 50000
ggplot(df_nasdaq_main, aes(x=limited_date)) +
  
  geom_line( aes(y=df_vacc_daily), color = "blue") + 
  geom_line( aes(y=df_nasdaq/coeff), color = "red") + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Percentage of U.S. Population Fully Vaccinated",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="NASDAQ") 
    
  )+
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) + xlab("")

