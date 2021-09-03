setwd("C:/R_Aug/Case_study3_Visualization")

Sales_data <- read.csv("SalesData.csv")
View(Sales_data)


# 1. Compare Sales by region for 2016 with 2015 using bar chart

require(dplyr)
sales_by_region2015 <- Sales_data%>% group_by(Region) %>% summarise(sales = sum(Sales2015)) 
View(sales_by_region2015)
sales_by_region2015$YEAR <- "TotalSales15"

sales_by_region2016 <- Sales_data%>% group_by(Region) %>% summarise(sales = sum(Sales2016))
sales_by_region2016$YEAR <- "TotalSales16"

sale_by_region <- rbind(sales_by_region2015, sales_by_region2016)

require(ggplot2)
require(plotly)

ggplot2::ggplot(data = sale_by_region) + aes(x = Region, y= sales, fill = YEAR) + geom_bar(stat = "identity", position = "dodge", color = "black") + ggtitle("Sales by Region for 2016 with 2015")



# 2. Pie charts for sales for each region in 2016 

PIE <- Sales_data %>% dplyr::group_by(Region) %>% dplyr::summarise(Total_Sale =round(sum(Sales2016)*100/sum(Sales_data$Sales2016),2))


pie(PIE$Total_Sale, labels =paste0(PIE$Region,PIE$Total_Sale ), main ="SALE of each Region2016")



# 3. Compare sales of 2015 and 2016 with Region and Tiers


sales3_1 <-   Sales_data %>% dplyr::group_by(Region,Tier) %>% dplyr::summarise(SALE = sum(Sales2015))
sales3_1$YEAR<- "YEAR15"

sales3_2 <-   Sales_data %>% dplyr::group_by(Region,Tier) %>% dplyr::summarise(SALE = sum(Sales2016))
sales3_2$YEAR<- "YEAR16"
sales3 <-rbind(sales3_1,sales3_2)

ggplot2::ggplot(data =sales3 ) + aes(x =Tier, y =SALE, fill =YEAR ) + geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~Region)+ theme(legend.title = element_blank())+labs(title = "Sales of 2015 & 2016 Region & Tiers")



# 4. In East region, which state registered a decline in 2016 as compared to 2015? 


sales4 <- Sales_data[Sales_data$Region =="East",]
sales4_1 <- sales4 %>% dplyr::group_by(State) %>% dplyr::summarise(SALE = sum(Sales2016))
sales4_1$YEAR<- "YEAR16"

sales4_2 <- sales4 %>% dplyr::group_by(State) %>% dplyr::summarise(SALE = sum(Sales2015))
sales4_2$YEAR<- "YEAR15"
sales412 <- rbind(sales4_1, sales4_2)
ggplot2::ggplot(data =sales412 ) + aes(x =State, y =SALE, fill =YEAR ) + geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.title = element_blank())+labs(title = "sales of 2015 and 2016" )



# 5. In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015? 


sales5<- Sales_data[Sales_data$Tier=="High",]
sales5_1 <- sales5 %>% dplyr::group_by(Division) %>% dplyr::summarise(UNITSOLD = sum(Units2015))
sales5_1$YEARLY<- "YEAR15"
sales5_2 <- sales5 %>% dplyr::group_by(Division) %>% dplyr::summarise(UNITSOLD = sum(Units2016))
sales5_2$YEARLY<- "YEAR16"
sales512 <-rbind(sales5_1,sales5_2)
ggplot2::ggplot(data =sales512) + aes(x=Division, y =UNITSOLD, fill = YEARLY ) +  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.title = element_blank())+labs(title = "Number of units sold in 2016 & 2015" )



# 6. Create a new column Qtr - 
# Jan - Mar : Q1 
# Apr - Jun : Q2 
# Jul - Sep : Q3 
# Oct - Dec : Q4 



sales6Q1 <- Sales_data[(Sales_data$Month == "Jan") | (Sales_data$Month == "Feb") | (Sales_data$Month =="Mar"),]
sales6Q1$QUARTER <- "Q1"
sales6Q2 <- Sales_data[(Sales_data$Month == "Apr") | (Sales_data$Month == "May") | (Sales_data$Month =="Jun"),]
sales6Q2$QUARTER <- "Q2"
sales6Q3 <- Sales_data[(Sales_data$Month == "Jul") | (Sales_data$Month == "Aug") | (Sales_data$Month =="Sep"),]
sales6Q3$QUARTER <- "Q3"
sales6Q4 <- Sales_data[(Sales_data$Month == "Oct") | (Sales_data$Month == "Nov") | (Sales_data$Month =="Dec"),]
sales6Q4$QUARTER <- "Q4"
sales6_Qtr <- rbind(sales6Q1,sales6Q2,sales6Q3,sales6Q4)
head(sales6_Qtr,10)




# 7. Compare Qtr wise sales in 2015 and 2016 in a bar plot 


sales7 <-sales6_Qtr
sales7_1 <- sales7 %>% dplyr::group_by(QUARTER) %>% dplyr::summarise(SALE = sum(Sales2015))
sales7_1$YEAR <- "YEAR2015"
sales7_2 <- sales7 %>% dplyr::group_by(QUARTER) %>% dplyr::summarise(SALE = sum(Sales2016))
sales7_2$YEAR <- "YEAR2016"
sales7A <- rbind(sales7_1,sales7_2)
ggplot2::ggplot(data =sales7A ) + aes(x =QUARTER, y =SALE, fill =YEAR) +geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.title = element_blank())+labs(title = "Qtr sales in 2015 & 2016" )





# 8. Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in a pie chart. 
# (Draw 4 pie charts representing a Quarter for each Tier)


### Group the data by Tier and Quarter 


sales8_1 <- sales7 %>% dplyr::group_by(Tier, QUARTER) %>% dplyr::summarise(Sale =round(sum(Sales2015)*100/sum(Sales_data$Sales2015),2))
sales8A <- sales8_1[sales8_1$Tier =="High", ]
pie(sales8A$Sale,labels =paste0(sales8A$QUARTER,sales8A$Sale), main ="PIECHARTOFSALE201   5 TierHigh")
sales8B <- sales8_1[sales8_1$Tier =="Low", ]
pie(sales8B$Sale,labels =paste0(sales8B$QUARTER,sales8B$Sale), main ="PIECHARTOFSALE2015 TierLow")
sales8C <- sales8_1[sales8_1$Tier =="Med", ]
pie(sales8C$Sale,labels =paste0(sales8C$QUARTER,sales8C$Sale), main ="PIECHARTOFSALE2015 TierMed")
sales8D <- sales8_1[sales8_1$Tier =="Out", ]
pie(sales8D$Sale,labels =paste0(sales8D$QUARTER,sales8D$Sale), main ="PIECHARTOFSALE2015 TierOut")