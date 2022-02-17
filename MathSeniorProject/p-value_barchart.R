library(readxl)
library(ggplot2)
all = read_excel('C:\\Users\\marys\\OneDrive\\Documents\\Covid-Statistical-Modeling\\MathSeniorProject\\Data.xlsx', 'Hypothesis Testing')
print(all)

stock_name = as.list(all$`Index/Stock`)
p_value = as.numeric(all$`p-value`)

barplot(height=all$`p-value`, names = all$`Index/Stock`, 
        col=rgb(0.8,0.1,0.1,0.6),
        space=c(1.2, 0.2, 0.2),
        main="p-values", 
        ylim=c(0,1),
        las = 2,
        horiz = TRUE
)


options(repr.plot.width=8, repr.plot.height=3)
ggplot(y, aes(x = stock_name, y = p_value, main="Car Distribution")) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Average Trip Duration (in seconds)") +
  scale_x_discrete(name="Start Station") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))
