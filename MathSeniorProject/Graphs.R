library("readxl")
library("ggplot2")
df <- read_excel("C:\\Users\\marys\\OneDrive\\Documents\\MathSeniorProject\\Data.xlsx")
# print(data)

#date = df[,1]
# print(date)
print(df)



ggplot(data = df, aes(x = date, y = daily_cases)) + geom_line()
print(df)
plot(x=df$Date, y = df$`Covid-19 Daily Increase in Cases`)

coeff <-10

ggplot(data, aes(x=df$Date)) + 
  geom_line( aes(y = df$`Covid-19 Daily Increase in Cases`), color = "#69b3a2") + 
  geom_line(aes(y = df$`S&P 500`), color = "red") + 
  scale_y_continuous(name = "Covid Cases", sec.axis = sec_axis(~.*coeff, name = "SP500"))

ggplot(data, aes(x=df$Date)) + 
  geom_line( aes(y = df$`USA Fully Vaccinated % of Population`), color = "#69b3a2") + 
  geom_line(aes(y = df$`S&P 500`), color = "red") + 
  scale_y_continuous(name = "Covid Cases", sec.axis = sec_axis(~.*coeff, name = "SP500"))
