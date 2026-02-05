install.packages("gapminder")
library(gapminder)
write.csv(
  gapminder,
  "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/data/gapminder.csv",
  row.names = FALSE
)
library(ggplot2)
gapminder <- read.csv("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/data/gapminder.csv")
head(gapminder)
str(gapminder)
countries <- c("Spain", "France", "Germany", "United Kingdom")
df <- gapminder[gapminder$country %in% countries, ]
ggplot(df, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Life expectancy",
       title = "Life expectancy over time") +
  theme_minimal()
ggsave("assignment1/ass1_plot.png", width = 7, height = 5)
1