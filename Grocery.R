library(dplyr)
library(ggplot2)

options(scipen=999)

grocery <- read.csv("~/Desktop/Groceries_dataset.csv")

grocery$Member_number <- as.character(grocery$Member_number)

grocery$itemDescription <- as.character(grocery$itemDescription)

## extracting weekday from date
grocery$Day <- weekdays(as.Date(grocery$Date, "%d-%m-%Y"))

class(grocery)

str(grocery)

#####################################################

## top 10 items by total sales

top_10_sales_pct <- grocery %>% 
  add_count(itemDescription) %>% 
  group_by(itemDescription) %>% 
  summarise(total_sales = sum(n)) %>% 
  top_n(10, total_sales) %>% 
  arrange(desc(total_sales))


## top 10 items by sale(pie chart)
pie_chart_1 <- ggplot(top_10_sales_pct, aes(x="", y=total_sales, fill=itemDescription))+
  geom_bar(width = 1, stat = "identity", color="white")+ coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(total_sales / sum(total_sales)*100, 1), '%')),
            position = position_stack(vjust = 0.5)) + theme_void() + 
  ggtitle('Top 10 percent of sales by itemDesciption')

pie_chart_1

#####################################################

## Total sales by weekday

total_by_day <- grocery %>% 
  add_count(itemDescription) %>% 
  group_by(Day) %>% 
  summarise(total_sales = sum(n)) %>% 
  arrange(desc(total_sales))

## total sales by weekday(bar_chart)
bar_chart_1 <- ggplot(total_by_day, aes(reorder(Day, -total_sales), total_sales)) +
  geom_bar(stat = 'identity', aes(fill = Day), position = 'dodge') +
  xlab('Day') +
  ylab('Total sales by Weekday') +
  ggtitle('Total sales by Weekday')

bar_chart_1

#####################################################


## Items with highest sales by weekday('Whole Milk' is item with highest sales everyday)

items_sales_weekday <- grocery %>% 
  add_count(itemDescription) %>%
  group_by(Day, itemDescription) %>% 
  summarise(total_sales = sum(n)) %>% 
  top_n(1, total_sales) %>% 
  arrange(desc(total_sales))


## Items with highest sales by weekday (pie chart)
pie_chart_2 <- ggplot(items_sales_weekday, aes(x="", y=total_sales, fill=Day))+
  geom_bar(width = 1, stat = "identity", color="white")+ coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(total_sales / sum(total_sales)*100, 1), '%')),
            position = position_stack(vjust = 0.5)) + theme_void() + 
  ggtitle('Items with highest sales by weekday')

pie_chart_2

########################################################

## Top 3 items sales by weekday
top_3_by_weekday <- grocery %>% 
  add_count(itemDescription) %>%
  group_by(Day, itemDescription) %>% 
  summarise(total_sales = sum(n)) %>% 
  top_n(3, total_sales) %>% 
  arrange(Day, desc(total_sales))

## Top 3 items sales by weekday(Bar chart)
bar_chart_2 <- ggplot(top_3_by_weekday, aes(reorder(Day, -total_sales), total_sales)) +
  geom_bar(stat = 'identity', aes(fill = itemDescription), position = 'dodge') +
  xlab('Weekday') +
  ylab('Total sales by Weekday') +
  ggtitle('Top 3 items sales by Weekday')

bar_chart_2
