setwd("C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject")

# importing the required library
# library(glmnet)
library(tidyverse)
library(readxl)
library(rio)
library(dygraphs)
library(xts)
library(GGally)

########read multisheets in an exel#####
path = "C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx"

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
date.st.start = 614 ###2020-2-3
date.st.end = 671 ### 1010-3-27

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

#####plot AAPL###########
data_graph = subset(data.num, select = c(Date, AAPL, `Change AAPL`))
don_AAPL_change=xts( x=data_graph[,-1], order.by=data_graph$Date)
dygraph(don_AAPL_change)


#############t-test########

############short-term t-test########
########dow jones######
before.data = data.num$`Dow Jones`[1:(date.st.start-1)]
after.data = data.num$`Dow Jones`[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)


########sp 500######
before.data = data.num$`S&P 500`[1:(date.st.start-1)]
after.data = data.num$`S&P 500`[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########nasdaq######
before.data = data.num$NASDAQ[1:(date.st.start-1)]
after.data = data.num$NASDAQ[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########MSFT######
before.data = data.num$MSFT[1:(date.st.start-1)]
after.data = data.num$MSFT[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########CRM######
before.data = data.num$CRM[1:(date.st.start-1)]
after.data = data.num$CRM[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AAPL######
before.data = data.num$AAPL[1:(date.st.start-1)]
after.data = data.num$AAPL[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########UNH######
before.data = data.num$UNH[1:(date.st.start-1)]
after.data = data.num$UNH[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AMGN######
before.data = data.num$AMGN[1:(date.st.start-1)]
after.data = data.num$AMGN[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)


########JNJ######
before.data = data.num$JNJ[1:(date.st.start-1)]
after.data = data.num$JNJ[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########GS######
before.data = data.num$GS[1:(date.st.start-1)]
after.data = data.num$GS[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########V######
before.data = data.num$`Change V`[1:(date.st.start-1)]
after.data = data.num$`Change V`[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AXP######
before.data = data.num$AXP[1:(date.st.start-1)]
after.data = data.num$AXP[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########BA######
before.data = data.num$BA[1:(date.st.start-1)]
after.data = data.num$BA[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)


########CAT######
before.data = data.num$CAT[1:(date.st.start-1)]
after.data = data.num$CAT[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########HON######
before.data = data.num$HON[1:(date.st.start-1)]
after.data = data.num$HON[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########HD######
before.data = data.num$HD[1:(date.st.start-1)]
after.data = data.num$HD[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########MCD######
before.data = data.num$MCD[1:(date.st.start-1)]
after.data = data.num$MCD[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########DIS######
before.data = data.num$DIS[1:(date.st.start-1)]
after.data = data.num$DIS[date.st.start:date.st.end]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########normal t-test###########
########dow jones######
before.data = data.num$`Dow Jones`[1:(marker.date-1)]
after.data = data.num$`Dow Jones`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change Dow Jones`[1:(marker.date-1)]
after.data = data.num$`Change Dow Jones`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########SP 500######
before.data = data.num$`S&P 500`[1:(marker.date-1)]
after.data = data.num$`S&P 500`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change S&P 500`[1:(marker.date-1)]
after.data = data.num$`Change S&P 500`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########NASDAQ######
before.data = data.num$NASDAQ[1:(marker.date-1)]
after.data = data.num$NASDAQ[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change NASDAQ`[1:(marker.date-1)]
after.data = data.num$`Change NASDAQ`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AAPL######
before.data = data.num$AAPL[1:(marker.date-1)]
after.data = data.num$AAPL[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change AAPL`[1:(marker.date-1)]
after.data = data.num$`Change AAPL`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########MSFT######
before.data = data.num$MSFT[1:(marker.date-1)]
after.data = data.num$MSFT[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change MSFT`[1:(marker.date-1)]
after.data = data.num$`Change MSFT`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########CRM######
before.data = data.num$CRM[1:(marker.date-1)]
after.data = data.num$CRM[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change CRM`[1:(marker.date-1)]
after.data = data.num$`Change CRM`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########UNH######
before.data = data.num$UNH[1:(marker.date-1)]
after.data = data.num$UNH[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change UNH`[1:(marker.date-1)]
after.data = data.num$`Change UNH`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AMGN######
before.data = data.num$AMGN[1:(marker.date-1)]
after.data = data.num$AMGN[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change AMGN`[1:(marker.date-1)]
after.data = data.num$`Change AMGN`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########JNJ######
before.data = data.num$JNJ[1:(marker.date-1)]
after.data = data.num$JNJ[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change JNJ`[1:(marker.date-1)]
after.data = data.num$`Change JNJ`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)


########GS######
before.data = data.num$GS[1:(marker.date-1)]
after.data = data.num$GS[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change GS`[1:(marker.date-1)]
after.data = data.num$`Change GS`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########V######
before.data = data.num$V[1:(marker.date-1)]
after.data = data.num$V[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change V`[1:(marker.date-1)]
after.data = data.num$`Change V`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########AXP######
before.data = data.num$AXP[1:(marker.date-1)]
after.data = data.num$AXP[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change AXP`[1:(marker.date-1)]
after.data = data.num$`Change AXP`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########BA######
before.data = data.num$BA[1:(marker.date-1)]
after.data = data.num$BA[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change BA`[1:(marker.date-1)]
after.data = data.num$`Change BA`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)


########CAT######
before.data = data.num$CAT[1:(marker.date-1)]
after.data = data.num$CAT[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change CAT`[1:(marker.date-1)]
after.data = data.num$`Change CAT`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########HON######
before.data = data.num$HON[1:(marker.date-1)]
after.data = data.num$HON[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change HON`[1:(marker.date-1)]
after.data = data.num$`Change HON`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########HD######
before.data = data.num$HD[1:(marker.date-1)]
after.data = data.num$HD[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change HD`[1:(marker.date-1)]
after.data = data.num$`Change HD`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########MCD######
before.data = data.num$MCD[1:(marker.date-1)]
after.data = data.num$MCD[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change MCD`[1:(marker.date-1)]
after.data = data.num$`Change MCD`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########DIS######
before.data = data.num$DIS[1:(marker.date-1)]
after.data = data.num$DIS[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

before.data = data.num$`Change DIS`[1:(marker.date-1)]
after.data = data.num$`Change DIS`[marker.date:end.date]

t.test(before.data, after.data, alternative = "greater", var.equal = FALSE)

########Vaccination Hypothesis Testing######

########dow jones######
before.data = data.num$`Change Dow Jones`[1:(marker.date.vac-1)]
after.data = data.num$`Change Dow Jones`[marker.date.vac:end.date]

t.test(before.data, after.data, alternative = "less", var.equal = FALSE)


########sp 500#####
before.data = data.num$`Change S&P 500`[1:(marker.date.vac-1)]
after.data = data.num$`Change S&P 500`[marker.date.vac:end.date]

t.test(before.data, after.data, alternative = "less", var.equal = FALSE)

########dow jones######
before.data = data.num$`Change NASDAQ`[1:(marker.date.vac-1)]
after.data = data.num$`Change NASDAQ`[marker.date.vac:end.date]

t.test(before.data, after.data, alternative = "less", var.equal = FALSE)


####################################################################
######all t-test p-value####
nt = 30
index.start = 4
index.end = 38
p_all = rep(0,18)

for (i in seq(index.start, index.end, 2) ){
  t_bf = data.num[(marker.date-nt+1):marker.date, i]
  t_af = data.num[(marker.date+1):(marker.date+nt), i]
  t_test = t.test(t_bf, t_af, alternative = "greater", var.equal = FALSE)
  p = t_test$p.value
  p_all[i/2 - 1] = p
  print(paste("The p-value of t-test for", names(data.num)[i], "in",  nt,"days before and after COVID break out is", p))
}

p_all
plot(p_all)

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
    geom_point(size = 0.3) + 
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


