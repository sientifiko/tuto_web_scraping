
library(ggplot2)
library(dplyr)
library(ggthemes)



df_news <- read.csv("df_news.csv")

df_news <- df_news[,-1]

colnames(df_news) <- c("link", "tag","date")

df_news$guaido <-  df_news$tag["Guaidó" %in% df_news$tag]


df_news$guaido <- str_count(df_news$tag, pattern = "Guaidó")

tbl <- df_news %>% 
  select(date, guaido) %>%
  group_by(date) %>%
  summarize(count = sum(guaido, na.rm = T))

tbl <- tbl[-1,]

tbl$date <- as.Date(tbl$date, format="%d-%m-%Y")



ggplot(tbl, aes(date, count)) +
  geom_line(size=2) +
  labs(y="atención golpe Guaidó", x="") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 30)) 
