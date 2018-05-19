
#This is a sample of my previous work experience when it comes to analysis.
#Here I was tasked with finding the cost of shipping for each particalar shipping destination
#Given the sales Q1 and Freight Q1 2017 information. I went ahead and found how much each case costed to ship
#Freelance as a quantitative financial analyst but my skills can be applied to data science as well.
sku <- read.csv('sku.info.csv') #created a data frame for sku
sales <- read.csv('Sales Q1 2017.csv') #created a data frame for sales of Q1 2017
freight <- read.csv('Freight Q1 2017.csv')#created a data frame for freights of Q1 2017
library(data.table) #Install data table package to use setnames to change column names for 
#much easier merging later on in the data.
library(tidyverse)#to use some functions
#Setting the names for Sales. Renaming the columns names so it would be much clearer to understand for me.
#This can easily be scaled if data comes from the same source. Only need to change the names.
library(dplyr)
library(ggrepel)
setnames(sales, old = c('Party.Name', 'Segment1', 'Ship.City', 'Ship.State', 'Ship.Postal.Code', 'Shipping.Method', 'Cases.SUM', 'X24.PBE.Cases'), 
         new=c('customer_name','sku','ship_city','ship_state','zip_code','shipping_method','cases_sold', 'package_24_shipment'))
#Changing the columns names of the freight columns to have them match the sales names
#This can easily be scaled if data comes from the same source. Only need to change the names.
setnames(freight, old=c('Destination.Customer','Sku','City','State','Zip.Code','Total.Spend', 'LOADTYPE', 'Shipment.24.Pk.Eq'), new=c('customer_name','sku','ship_city','ship_state','zip_code','total_spend', 'shipping_method', 'package_24_shipment'))                                                                                                                                      

#Setting names for the sku csv
setnames(sku, old=c('Item.Number', 'X24.Equivalent', 'Cases.per.Truck'), new = c('sku', 'X24', 'cases_per_truck'))

#Selecting the information that we need from the freight to create our spreadsheet.
#converted the data frame into a tibble which is much easier to work with in Rstudio
#This can easily be scaled because if the names have been changed previously then this will follow.
freight_cost <- select(freight,
                       customer_name,
                       sku,
                       ship_city,
                       ship_state,
                       zip_code,
                       shipping_method,
                       package_24_shipment,
                       total_spend)

#Needed to make this column into a character string since it had numbers R was interpretting this as
#factor values
freight_cost$ship_city <- as.character(freight_cost$ship_city)

#Here we are going to be doing some data cleaning so that both the freight data and sales data match up.
#We want to have nice data so we know that the city names are the same and everything is correctly marked.
freight_cost[freight_cost$ship_city == 'TLAJOMULCO DE ZUNIGA SAMS-6238', 'ship_city'] <- 'TLAJOMULCO DE ZUNIGA'
freight_cost[freight_cost$ship_city == 'CHALCO SAMS-7505', 'ship_city'] <- "CHALCO"
freight_cost[freight_cost$ship_city == 'CUAUTITLAN IZCALLI SAMS-6388', 'ship_city'] = "CUAUTITLAN IZCALLI"
freight_cost[freight_cost$ship_city == 'MONTERREY SAMS-4995', 'ship_city'] <- 'MONTERREY'
freight_cost[freight_cost$ship_city == 'CULIACAN SAMS-4971', 'ship_city'] <- 'CULIACAN'
freight_cost[freight_cost$ship_city == 'VILLAHERMOSA TAB. SAMS 6550', 'ship_city'] <- 'VILLAHERMOSA TABASCO'
freight_cost[freight_cost$ship_city == 'TLAJOMULCO DE ZUNIGA WM-7460', 'ship_city'] <- 'TLAJOMULCO DE ZUNIGA'
freight_cost[freight_cost$ship_city == 'TLAJOMULCO DE ZUNIGA WM-7493', 'ship_city'] <- 'TLAJOMULCO DE ZUNIGA'
freight_cost[freight_cost$ship_city == 'CHALCO WM-7459', 'ship_city'] <- 'CHALCO'
freight_cost[freight_cost$ship_city == 'CHALCO WM-7471', 'ship_city'] <- 'CHALCO'
freight_cost[freight_cost$ship_city == 'CUAUTITLAN IZCALLI WM-7457', 'ship_city'] <- 'CUAUTITLAN IZCALLI'
freight_cost[freight_cost$ship_city == 'CUAUTITLAN IZCALLI WM-7464', 'ship_city'] <- 'CUAUTITLAN IZCALLI'
freight_cost[freight_cost$ship_city == 'CUAUTITLAN IZCALLI WM-7482', 'ship_city'] <- 'CUAUTITLAN IZCALLI'
freight_cost[freight_cost$ship_city == 'CUAUTITLAN IZCALLI WM-7494', 'ship_city'] <- 'CUAUTITLAN IZCALLI'
freight_cost[freight_cost$ship_city == 'MONTERREY WM-7461', 'ship_city'] <- 'MONTERREY'
freight_cost[freight_cost$ship_city == 'MONTERREY WM-7490', 'ship_city'] <- 'MONTERREY'
freight_cost[freight_cost$ship_city == 'CULIACAN WM-7455', 'ship_city'] <- 'CULIACAN'
freight_cost[freight_cost$ship_city == 'CULIACAN WM-7487', 'ship_city'] <- 'CULIACAN'
freight_cost[freight_cost$ship_city == 'VILLAHERMOSA TAB. WM-7468', 'ship_city'] <- 'VILLAHERMOSA TABASCO'
freight_cost[freight_cost$ship_city == 'VILLAHERMOSA TABASCO WM 7453', 'ship_city'] <- 'VILLAHERMOSA TABASCO'

#We only want to deal in cases where the client did not pickup the items
freight_cost <- filter(freight_cost, shipping_method != 'CustomerPickup')
#Here using the select function to gather all the necessary elements from the sales
#in order to help calculate out cost. Made this data frame into a tibble so much easier to 
#work with in the console.
#This can easily be scaled because if the names have been changed previously then this will follow.
sales_cost <- as_tibble(select(sales,
                               customer_name,
                               sku,
                               ship_city,
                               ship_state,
                               zip_code,
                               shipping_method,
                               package_24_shipment,
                               cases_sold))

sales_cost$ship_city <- as.character(sales_cost$ship_city)
sales_cost$shipping_method <- as.character(sales_cost$shipping_method)
#We know that when the delivery method says "paid" that means that the product has been shipping for delivery
#So we can go ahead and change up the information to reflect that
sales_cost[sales_cost$shipping_method == "DELIVERY", 'shipping_method'] <- 'Delivery'
sales_cost[sales_cost$shipping_method == 'Paid', 'shipping_method'] <- 'Delivery'

#Want to clean up the data so that all the information matches as best as possible
sales_cost[sales_cost$ship_city == "Ramos Arizpe", 'ship_city'] <- 'RAMOS ARIZPE'
sales_cost[sales_cost$ship_state == 'mx', 'ship_state'] <- 'MX'
#Now lets filter out any cases that the client pickup up the cases of water since we only want to focus on delivery
sales_cost <- filter(sales_cost, shipping_method != 'PICKUP')


#Some of the values inside the sales has negative values for cases sold but in real life there
#is not a possibility of selling negative cases. So my intuition for this is to work only in 
#the world where all cases are sold and there is no negative values. Went ahead and set these
#negative values to positive.
sales_cost$cases_sold <- abs(sales_cost$cases_sold)

#grouping the vallues so we can have the total amount spend at each location 
freight_group <- freight_cost %>%
  group_by(customer_name, sku, ship_city, ship_state, zip_code, shipping_method) %>%
  summarise(package_24_shipment = sum(package_24_shipment), total_spend = sum(total_spend)) %>%
  ungroup()

#grouping the values of the sales data so we can sum the total cases shipped out to each location
sales_group <- sales_cost %>%
  group_by(customer_name, sku, ship_city, ship_state, zip_code, shipping_method) %>%
  summarise(package_24_shipment = sum(package_24_shipment), cases_sold = sum(cases_sold)) %>%
  ungroup()

#Calculating the cost of each item to be shipped. So by grouping by the sku item number we can come to a 
#nice grouping of what the total cost of shipping that number was and how many cases were shipped with that
#item number

sku_sold <- select(sales_group, sku, cases_sold) %>%
  group_by(sku) %>%
  summarise(cases_sold = sum(cases_sold)) %>%
  ungroup()
sku_freight <- select(freight_group, sku, total_spend) %>%
  group_by(sku) %>%
  summarise(total_spend = sum(total_spend)) %>%
  ungroup()

#merging the sku data so we can find the avg cost of shipping the item 
sku_total <- merge(sku_sold, sku_freight, by =c('sku')) %>%
  mutate(avg_spend = total_spend/cases_sold)
sku_total <- select(sku_total, sku, avg_spend)
#now we add this data to our sales information
sales_final <- merge(sales_group, sku_total, by =c('sku')) %>%
  ungroup()

#making some values into factor so work better with graphing
sales_final$zip_code <- as.character(sales_final$zip_code)
sales_final$shipping_method <- as.factor(sales_final$shipping_method)
sales_final$ship_city <- as.factor(sales_final$ship_city)
sales_final$zip_code <- as.factor(sales_final$zip_code)

write_excel_csv(sales_final,'cost_total_for_delivery.csv')


#Creating the map for mexico
world.map <- map_data("world")
mexico <- subset(world.map, region == "Mexico")

#Here I create a new data frame so its easier to get the values of the long and lat
select_stuff <- select(sales_final,
                       ship_state,
                       ship_city,
                       zip_code,
                       cases_sold) %>%
  group_by(ship_city, ship_state, zip_code) %>%
  summarise(avg_case = mean(cases_sold)) %>%
  ungroup()


#here creating an empty data frame so that I can add in the long and lat of the information
select_stuff['long'] <- NA
select_stuff['lat'] <- NA
select_stuff$long <- as.double(c('-98.85', '-104.0023', '-100.1471', '-99.1783', '-99.1783', '-99.1972',
                                 '-107.4631', '-103.3167', '-100.3122', '-103.6167', '-100.2138', '-99.4389',
                                 '-92.5124', '-99.1833', '-103.3167', '-103.1833', '-99.4919', '-92.8138',
                                 '-93.4', '-92.8'))
select_stuff$lat <- as.double(c('19.25', '19.1116', '25.7636', '19.6997', '19.6997', '19.5936',
                                '24.7733', '20.5667', '25.8002', '25.6333', '25.7070', '24.9761',
                                '15.9266', '19.7456', '20.4333', '20.5167', '19.6651', '17.9278',
                                '17.8667', '17.5458'))

#Now I want to add this information to my original sales final graph so let me get the information correct
#first going to keep only the important information
select_imp <- select(select_stuff,
                     ship_city,
                     zip_code,
                     long,
                     lat)
sales_final2 <- merge(sales_final, select_imp, by = c('ship_city', 'zip_code')) 


#here is the final mexico graph 
mexico_graph <- ggplot(mexico, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "white",
               color = "gray40", size = .2) +
  geom_point(data = sales_final2, aes(x = long, y = lat, size = cases_sold, color = ship_city))+
  scale_size_continuous(range = c(8,15))+
  labs(color = 'City of Destination', size = "Average cost of shipment", title ='Comparing the average cost of shipping to each city in Mexico')



#This is a regression line between avg spend, cases_sold, and 24 packs
ggplot(sales_final, aes(x = avg_spend, y = cases_sold))+
  geom_point(aes(size = package_24_shipment), alpha = 1/3)+
  geom_smooth(se = FALSE)

#histogram showing how much is sold in each city
ggplot(sales_final, aes(x = cases_sold, fill = ship_city))+
  geom_histogram()+
  labs(x = 'Cases Sold', fill = "City")

#Boxplot of avg spend
ggplot(sales_final, aes(x = reorder(ship_city, avg_spend, median), y = avg_spend))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x = "City", y = "Shipment Cost per Case")

#here creating a subset 
cities <- select(sales_final, ship_city, cases_sold) %>%
  group_by(ship_city) %>%
  summarise(total_sold = sum(cases_sold)) %>%
  ungroup()


## Creating a pie chart for the percentage of cases sold to each respective city
#This is a function that takes in a columns and calculates the weight of each value in the column  
proprtion <- function(x, removeNA=TRUE){
  nc <- length(x) #length of the column
  prop <- numeric(nc) #We need to initialize this vector. We know this will be numeric so we add that now.
  for (i in 1:nc){
    prop[i] <- x[i] / sum(x)
  }
  prop
}
prop <- select(cities, ship_city, total_sold)
prop['prop'] <- NULL
prop$prop <- proprtion(cities$total_sold)
prop$prop <- formatC(prop$prop, digits = 5, format = "f")
prop$prop <- as.double(prop$prop)

library(plotly) #for pie chart
pie <- plot_ly(prop, labels = prop$ship_city, values = prop$prop, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = 'Percentage of Cases Sold to each Shipping City',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 



##conver to a time series
sales2 <- ts(abs(sales$cases_sold), frequency = 365)
plot.ts(sales2, xlim = c(8, 10))


# selecting data and cases sold
sales_data <- select(sales,
                     Trx.Date,
                     cases_sold)
sales_data$cases_sold <- abs(sales_data$cases_sold)
#Now changing the data to reflect a time format that is recognizable by R
sales_data$Trx.Date <- as.Date(sales_data$Trx.Date, format = '%m/%d/%Y')
#Making the data into a table so that we can work with it nicely 
#Then sum the column
DT<- data.table(sales_data)
DT <- DT[,sum(cases_sold),by = Trx.Date]


#Revenue forcasting



#Fit the model and get predictions
cases_sold.mean <- HoltWinters(DT$V1,
                               alpha = .2, #comment to estimate
                               beta = .1, #Comment to estimate
                               gamma = FALSE)

#beta = FALSE and gamma = FALSE give EWMA
# gamma = FALSE gives double exponential smoothing

cases_sold.pred <- predict(cases_sold.mean,
                           n.ahead = 30, #how many spaces in the future you want to look
                           prediction.interval = TRUE)

cases_sold.pred
#plot data
plot.ts(DT$V1, ylab = 'Cases Sold', xlim = c(10,170), ylim = c(0, 200000))
lines(cases_sold.mean$fitted[,1], col = 'green') #the fitted is for all the fittted values, the 1 is for the first column
lines(cases_sold.pred[,1], col='blue') # the fit columns
lines(cases_sold.pred[,2], col = 'red') #upper column
lines(cases_sold.pred[,3], col = 'red') #lower columns

((cases_sold.pred[30] / cases_sold.pred[1]) - 1)*100


#forecasting
library(forecast)
sensor <- ts(DT$V1)
fit <- auto.arima(sensor)
fcast <- forecast(fit, h = 30)
plot(fcast)
plot(forecast(auto.arima(ts(DT$V1, frequency = 365)), h = 365))