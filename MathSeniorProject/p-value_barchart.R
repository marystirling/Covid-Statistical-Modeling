library(readxl)
library(ggplot2)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx', 'Hypothesis Testing')
print(all)

stock_name <- all$`Index/Stock`
p_value <- all$`p-value (LT)`
df <- data.frame(stock_name, p_value)
#stock_name = factor(stock_name, levels = c("Dow Jones", "S&P 500", "NASDAQ", "MSFT", "CRM", "AAPL", "UNH", "AMGN", "JNJ", "GS", "V", "AXP", "BA", "CAT", "HON", "HD", "MCD", "DIS"), ordered = TRUE)

ggplot(df, aes(stock_name, all$`p-value (LT)`)) + 
  geom_bar(stat = "identity", fill = "light blue") + coord_flip() + 
  theme(axis.text.x = element_text(color="black",
                                   size=14, angle=0),
        axis.text.y = element_text(color="black",
                                   size=14, angle=0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + scale_x_discrete(limits = stock_name) 

rev_stock_name <- all$`Reverse name`
rev_p_value <- all$`reverse p-value`
rev_df <- data.frame(rev_stock_name, rev_p_value)
#stock_name = factor(stock_name, levels = c("Dow Jones", "S&P 500", "NASDAQ", "MSFT", "CRM", "AAPL", "UNH", "AMGN", "JNJ", "GS", "V", "AXP", "BA", "CAT", "HON", "HD", "MCD", "DIS"), ordered = TRUE)

ggplot(rev_df, aes(rev_stock_name, rev_p_value)) + 
  geom_bar(stat = "identity", fill = "royalblue4") + coord_flip() + 
  theme(axis.text.x = element_text(color="black",
                                   size=14, angle=0),
        axis.text.y = element_text(color="black",
                                   size=14, angle=0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + scale_x_discrete(limits = rev_stock_name) 

coord_flip() + stock_name = as.list(all$`Index/Stock`)

p_value = as.numeric(all$`p-value`)

barplot(height=all$`p-value`, names = all$`Index/Stock`, 
        col=rgb(0.8,0.1,0.1,0.6),
        space=c(1.2, 0.2, 0.2),
        main="p-values", 
        ylim=c(0,1),
        las = 2,
        #horiz = TRUE
)

ggplot(df, aes(x = stock_name, y = p_value))

df$all..p.value. = as.numeric(df$all..p.value.)
options(repr.plot.width=8, repr.plot.height=3)
ggplot(data, aes(x = stock_name, y = p_value, main="Car Distribution")) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_discrete(name="Average Trip Duration (in seconds)") +
  scale_x_discrete(name="Start Station") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))

ggplot(data, aes(x = stock_name, y = p_value)) + geom_point() + scale_y_continuous(limits = c(0, 1))

