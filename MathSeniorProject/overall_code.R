setwd("/Users/msun1/OneDrive - Samford University/Samford University/SU Teaching/MA495 Senior Project/2022 Spring/Project_Mary Brown/Data")

# importing the required library
# library(glmnet)
library(tidyverse)
library(readxl)
library(rio)
library(dygraphs)
library(xts)
library(GGally)

########read multisheets in an exel#####
path = "/Users/msun1/OneDrive - Samford University/Samford University/SU Teaching/MA495 Senior Project/2022 Spring/Project_Mary Brown/Data/Data_5_1.xlsx"

# reading data from all sheets
data_all = import_list(path)

### print data
# print (data)
summary(data_all)
str(data_all)
summary(data_all$Main)
n = dim(data_all$Main)[1]
p = dim(data_all$Main)[2]

data.all = data_all$Main
data.all.num = as.data.frame(cbind(data.all[,1:3], sapply(data.all[,4:p], as.numeric)))
index.na = which(is.na(data.all.num$`Change Dow Jones`)) 

# index.remove = c(index.lk, index.na)
data.num = data.all.num[-index.na,]
str(data.num)

marker.date = 439 ###2020-3-2
marker.date.vac = 652 ###2021-1-4
end.date = 903

########plot################

##########covid and dowjones######
data_graph = subset(data.num, select = c(Date, `Covid-19 Daily Increase`, `Dow Jones`))
don_DJ=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(don_DJ)

#########all three markets####
data_graph = subset(data.num, select = c(Date, `Dow Jones`, `S&P 500`, NASDAQ))
don_3stocks=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(don_3stocks)


###########Technology###########

data_graph = subset(data.num, select = c(Date, MSFT, CRM, AAPL))
don_industry=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(don_industry)

data_graph = subset(data.num, select = c(Date, `Change MSFT`, `Change CRM`, `Change AAPL`))
don_industry_change=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(don_industry_change)

data_graph = subset(data.num, select = c(Date, MSFT, CRM, AAPL,`Change MSFT`, `Change CRM`, `Change AAPL`))
don_industry_all=xts( x = data_graph[,-1], order.by=data_graph$Date)
dygraph(don_industry_all)


#####plot BOEN###########
data_graph = subset(data.num, select = c(Date, BA, `Change BA`))
don_BA_change=xts( x=data_graph[,-1], order.by=data_graph$Date)
dygraph(don_BA_change)


#############t-test########

########boeing######
before.data = data.num$BA[1:(marker.date-1)]
after.data = data.num$BA[marker.date:end.data]

t.test(before.data, after.data, alternative = "two.sided", var.equal = FALSE)

before.data = data.num$`Change BA`[1:(marker.date-1)]
after.data = data.num$`Change BA`[marker.date:end.data]

t.test(before.data, after.data, alternative = "less", var.equal = FALSE)



##########modeling#######

####correlation analysis###
cor(data.num$`Dow Jones`, data.num$`S&P 500`)
cor(data.num$`Dow Jones`, data.num$NASDAQ)
cor(data.num$`S&P 500`, data.num$NASDAQ)
# cor(data.num$`Dow Jones`, data.num$`Covid-19 Daily Increase`)
# cor(data.num$`S&P 500`, data.num$`Covid-19 Daily Increase`)
# cor(data.num$NASDAQ, data.num$`Covid-19 Daily Increase`)


###DOW JONES #####
data.DJ= subset(data.num, select = c(`Dow Jones`, `Covid-19 Daily Increase`, `% Fully Vaccinated (USA)`))

data.DJ.model = data.DJ[marker.date.vac:end.date,]

ggpairs(data.DJ.model)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data.DJ.model, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data.DJ.model,columns = 1:3, lower = list(continuous = my_fn))
g

###S&P 500 #####
data.SP= subset(data.num, select = c(`S&P 500`, `Covid-19 Daily Increase`, `% Fully Vaccinated (USA)`))

data.SP.model = data.SP[marker.date.vac:end.date,]

ggpairs(data.SP.model)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data.SP.model, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data.SP.model,columns = 1:3, lower = list(continuous = my_fn))
g

###NASDAQ #####
data.NS= subset(data.num, select = c(NASDAQ, `Covid-19 Daily Increase`, `% Fully Vaccinated (USA)`))

data.NS.model = data.NS[marker.date.vac:end.date,]

ggpairs(data.NS.model)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data.NS.model, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data.NS.model,columns = 1:3, lower = list(continuous = my_fn))
g


###All three markets #####
data.all3= subset(data.num, select = c(`Dow Jones`,`S&P 500`, NASDAQ, `Covid-19 Daily Increase`, `% Fully Vaccinated (USA)`))

data.all3.model = data.all3[marker.date.vac:end.date,]

ggpairs(data.all3.model)


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data.all3.model, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(data.all3.model,columns = 1:5, lower = list(continuous = my_fn))
g






# 
# ##linear regression model
# ######standardize the data
# ct_data.DJ = as.data.frame(scale(data.DJ.model, center = TRUE, scale = FALSE))
# 
# ggpairs(sd_data.DJ)
# 
# # plot(density((data.num$`Dow Jones`)))
# 
# 
# 
# glm.fit = glm(`Dow Jones` ~ `Covid-19 Daily Increase` + `% Fully Vaccinated (USA)`, data = ct_data.DJ, family = gaussian )
# summary(glm.fit)
# 
# # n = dim(data.DJ.model)[1]
# # test.size = 50
# # train.size = n - test.size
# # glm.fit = glm(`Dow Jones` ~ `Covid-19 Daily Increase` + `% Fully Vaccinated (USA)`, data = ct_data.DJ[1:train.size,], family = gaussian )
# # summary(glm.fit)
# # glm.pred =  predict(glm.fit,newdata = ct_data.DJ[(train.size+1):n,])
# # glm.pred
# #  pred.error = mean(abs((as.numeric(glm.pred) - ct_data.DJ[(train.size+1):n,1])/ct_data.DJ[(train.size+1):n,1]))
# # pred.error


