#CHEONG ZI YOU	TP063836
#FIONA LIOW ZI LIN	TP060687
#LIM CHEN YI	TP066187
#TAN SEE YING	TP061971

#Install Packages
install.packages("plyr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("crayon")
install.packages("reshape2")
install.packages("plyr")
install.packages("devtools")
install.packages("scatterplot3d")
install.packages("rgl")
install.packages("ggridges")
install.packages("plotrix")
install.packages("RColorBrewer")
install.packages("plotly")

library(crayon)
library(tidyr)
library(ggplot2)
library(crayon)
library(dplyr)
library(plyr)
library(scales)
library(plotrix)
library(tidyverse)
library(hrbrthemes)
library(ggridges)
library(plotrix)
library(scatterplot3d)
library(ddplot)
library(RColorBrewer)
library(plotly)
require(rgl)



#--------Data Import----------------
rent_data <- read.csv("C:\\Users\\chyil\\Downloads\\university\\R programing\\R assignment new\\House_Rent_Dataset.csv")
dim(rent_data)
View(rent_data)



#--------Data Cleaning--------------
#searching any duplication data inside of dataset
sum(duplicated(rent_data)==TRUE)
sum(duplicated(rent_data)==FALSE)

#search missing value
sum(is.na(rent_data))
mean(is.na(rent_data))

#--------Pre-Processing--------------
#check data type of each column
str(rent_data)

#convert date & Split it
rent_data$Posted.On <- as.Date(rent_data$Posted.On,"%m/%d/%Y")

rent_data <- mutate(rent_data, Year = as.numeric(format(rent_data$Posted.On, format = "%Y")),
               Month = as.numeric(format(rent_data$Posted.On, format = "%m")),
               Day = as.numeric(format(rent_data$Posted.On, format = "%d")))

View(rent_data)

#Split floor
splitted_floor = data.frame(do.call("rbind", strsplit(rent_data$Floor, split = " out of ")))
names(splitted_floor) = c("House_Floor", "Total_Floor")
str(splitted_floor)
categories <- unique(splitted_floor$House_Floor) 
categories

splitted_floor$House_Floor[splitted_floor$House_Floor == "Upper Basement"] <- "0.75"
splitted_floor$House_Floor[splitted_floor$House_Floor == "Lower Basement"] <- "0.25"
splitted_floor$House_Floor[splitted_floor$House_Floor == "Ground"] <- "0"

#convert floor to numeric
splitted_floor$House_Floor = as.numeric(splitted_floor$House_Floor)
splitted_floor$Total_Floor = as.numeric(splitted_floor$Total_Floor)

splitted_floor
rent_data<-cbind(rent_data, splitted_floor)

#rename header
names(rent_data) = c("Date","BHK","Rent_Price","Size","Floor","Area_Type",
                             "Locality","City","Furnishing_Status","Tenant","Bathroom","Point_of_Contact",
                             "Year","Month","Day","House_Floor","Total_Floor")


View(rent_data)

#convert to factor
str(rent_data)
rent_data$Area_Type = as.factor(rent_data$Area_Type)
rent_data$Locality = as.factor(rent_data$Locality)
rent_data$City = as.factor(rent_data$City)
rent_data$Furnishing_Status = as.factor(rent_data$Furnishing_Status)
rent_data$Tenant = as.factor(rent_data$Tenant)
rent_data$Point_of_Contact = as.factor(rent_data$Point_of_Contact)
str(rent_data)
View(rent_data)

#check data type
data_type = data.frame(Data_Type = sapply(rent_data, class))
data_type

#Display
options(scipen=999)
options(show.signif.stars=FALSE)

nlevels(factor(rent_data$House_Floor))
unique(rent_data$House_Floor)


#--------Data Exploration--------------

#1.Number of cities
city_num = nlevels(factor(rent_data$City))
paste("Total Number of cities :", city_num)
unique(rent_data$City)

#convert cities to a list
city_col = duplicated(as.list(rent_data$City))

#Remove duplicated city data and display as table
city = as.data.frame(rent_data$City[!city_col])
names(city)=c("City")
View(city)


#2.Total area localities
locality_num = nlevels(factor(rent_data$Locality))
paste("Total Area Locality: ", locality_num)
unique(rent_data$Locality)

#convert area localities to a list
locality_col = duplicated(as.list(rent_data$Locality))

#Remove duplicated area localities data and display as table
locality = as.data.frame(rent_data$Locality[!locality_col])
names(locality) = c("Area Locality")
View(locality)


#3.Types of area
area_num = nlevels(factor(rent_data$Area_Type))
paste("Total Area Types: ", area_num)
unique(rent_data$Area_Type)

#convert area types to a list
area_col = duplicated(as.list(rent_data$Area_Type))

#Remove duplicated area types data and display as table
area = as.data.frame(rent_data$Area_Type[!area_col])
names(area) = c("Area Type")
View(area)


#4.Types of furnishing status
furnish_num = nlevels(factor(rent_data$Furnishing_Status))
paste("Total Types of Furnishing Status: ", furnish_num)
unique(rent_data$Furnishing_Status)

#Convert to list
furnish_col = duplicated(as.list(rent_data$Furnishing_Status))

#Remove duplicated column
furnish = as.data.frame(rent_data$Furnishing_Status[!furnish_col])
names(furnish) = c("Furnishing Status")
View(furnish)


#5.Types of tenant preferred by the landlord
tenant_num = nlevels(factor(rent_data$Tenant))
paste("Total Types of Tenant Preferred: ", tenant_num)
unique(rent_data$Tenant)

#Convert to list
tenant_col = duplicated(as.list(rent_data$Tenant))

#Remove duplicated column
tenant = as.data.frame(rent_data$Tenant[!tenant_col])
names(tenant) = c("Tenant Preferred")
View(tenant)


#6.Types of point of contact
contact_num = nlevels(factor(rent_data$Point_of_Contact))
paste("Total Types of Point of Contact: ", contact_num)
unique(rent_data$Point_of_Contact)

#Convert to list
contact_col = duplicated(as.list(rent_data$Point_of_Contact))

#Remove duplicated column
contact = as.data.frame(rent_data$Point_of_Contact[!contact_col])
names(contact) = c("Point of contact")
View(contact)


#7. House Size Range
house_size_range <- rent_data %>%
  mutate(SIZE_RANGE = cut(Size,seq(0,8000,2000))) %>%
  group_by(SIZE_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()
house_size_range$SIZE_RANGE <- c("0-2000", "2001-4000", "4001-6000", "6001-8000")
house_size_range


#8. Types of BHK combination
bhk_num = nlevels(factor(rent_data$BHK))
paste("Total Types of BHK combination: ", bhk_num)
unique(rent_data$BHK)

#Convert to list
bhk_col = duplicated(as.list(rent_data$BHK))

#Remove duplicated column
bhk = as.data.frame(rent_data$BHK[!bhk_col])
names(bhk) = c("BHK")
View(bhk)


#9.Building's floor for rent
floor_available_num = nlevels(factor(rent_data$House_Floor))
paste("Total distinct Building's floor for rent: ", floor_available_num)
unique(rent_data$House_Floor)

#Convert to list
floor_available_col = duplicated(as.list(rent_data$House_Floor))

#Remove duplicated column
floor_available = as.data.frame(rent_data$House_Floor[!floor_available_col])
names(floor_available) = c("Building's floor for rent")
View(floor_available)


#10.Number of bathroom
bathroom_num = nlevels(factor(rent_data$Bathroom))
paste("Total Types of Bathroom number: ", bathroom_num)
unique(rent_data$Bathroom)

#Convert to list
bathroom_col = duplicated(as.list(rent_data$Bathroom))

#Remove duplicated column
bathroom = as.data.frame(rent_data$Bathroom[!bathroom_col])
names(bathroom) = c("Bathroom number")
View(bathroom)


#11.Rental price range
rental_range_1200_3500000 <- rent_data %>%
  mutate(RENTAL_RANGE = cut(Rent_Price,
                            seq(1200,3500000,50000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()

rental_range_1200_3500000$RENTAL_RANGE <- c("1000-51000", "51000-101000", "101000-151000", 
                                            "151000-201000", "201000-251000", "251000-301000",
                                            "301000-351000","351000-401000","401000-451000",
                                            "451000-501000", "501000-551000","551000-601000",
                                            "601000-651000","651000-701000","801000-851000",
                                            "951000-1000000","1150000-1200000", "1200000-3500000")
rental_range_1200_3500000

#Deeper analysis in the price range 1000 to 10000
rental_range_1000_10000 <- rent_data %>%
  mutate(RENTAL_RANGE = cut(Rent_Price,
                            seq(1000,10000,1000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()
rental_range_1000_10000 <-rental_range_1000_10000 %>% drop_na()

rental_range_1000_10000$RENTAL_RANGE <- c("1000-2000", "2000-3000", "3000-4000", 
                                          "4000-5000","5000-6000","6000-7000",
                                          "7000-8000","8000-9000","9000-10000")
rental_range_1000_10000








#------------------------------------------------------------------------------------------------------------------------------------------
#--------Question--------------
#Question 1: What is the cheapest day to rent a house?
#Analysis 1.1: The average rent price on each month
#Analysis 1.1.1: The quantity of house rent on each month
data_month = rent_data %>% group_by(Month) %>% dplyr::summarise(Quantity = n())
data_month
rent_data$Month
ggplot(data_month, aes(x=Month, y=Quantity)) +  geom_line(col= "blue") +  
  geom_point(col = "red", stat = "identity") +  
  geom_text(aes(label = Quantity), hjust = 0.5,  vjust = -1) +
  theme(panel.background = element_rect(fill = "lavender")) + 
  ggtitle("The quantity of house rent on each month")

#Analysis 1.1.2: Average rent on each month
avg_rent_month = aggregate(rent_data$Rent_Price, list(rent_data$Month), FUN=mean)
names(avg_rent_month) = c("Month_AVG", "Average_Rent_In_Month")

ggplot(avg_rent_month, aes(x=Month_AVG, y=Average_Rent_In_Month)) + 
  geom_line( color="#69b3a2", size=1.1, alpha=0.9) + geom_point() +
  geom_text(aes(label = round(Average_Rent_In_Month, digits = 1)), hjust = 0.5,  vjust = -1, col = 'Purple') +
  labs(title="Average Rent on Each Month", x="Month", y="Average Rent") +
  theme(panel.background = element_rect(fill = "lightblue"))

#gitter plot
g_means <- ddply(rent_data, .(Month), summarise, meann = mean(Rent_Price))
ggplot(rent_data, aes(Month, Rent_Price)) +
  geom_point(color = "lightblue") +
  geom_jitter(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="purple", fill="purple") +
  geom_text(data = g_means, aes(x = Month, y = meann, 
                                label = paste("Average:",round(meann, digits = 1))), vjust = 1) +
  labs(title="Rent on Each Month", x="Month", y="Rent") +
  theme(panel.background = element_rect(fill = "LAVENDER"))

#Analysis 1.2: The quantity of house rent post on each day
#Analysis 1.2.1: Dates collected in April
April_data = rent_data %>% 
            filter(Month == 4) %>% group_by(Day) %>%  dplyr::summarise(Quantity = n())

#Bar
ggplot(April_data, aes(x=Day, y = Quantity, fill=Quantity)) + 
  geom_bar( stat = "Identity") +
  geom_text(aes(label = Quantity), hjust = -0.3) +
  scale_x_continuous(breaks = seq(from = 1, to = 30, by = 1)) +
  ggtitle("Dates collected in April") +
  coord_flip()

#Pie - percent
piepercent<- round(100*April_data$Quantity/sum(April_data$Quantity), 1)

pie(April_data$Quantity, labels = paste0(piepercent, "%"), main = "Dates collected in April",
    col = rainbow(length(April_data$Quantity)))
    
legend("topright", as.character(April_data$Day), cex = 0.48, 
       fill = rainbow(length(April_data$Quantity)), title = "Day") 



#Analysis 1.2.2: Dates collected in July
July_data = rent_data %>% 
  filter(Month == 7) %>% group_by(Day) %>%  dplyr::summarise(Quantity = n())

#bar
ggplot(July_data, aes(x=Day, y = Quantity, fill=Quantity)) + 
  geom_bar( stat = "Identity") +
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(label = Quantity), vjust = -0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 30, by = 1)) +
  ggtitle("Dates collected in July") 

#Pie
piepercent<- round(100*July_data$Quantity/sum(July_data$Quantity), 1)

bmp("plot5.bmp", height=600, width=800, pointsize = 15)
pie3D(July_data$Quantity,labels = paste0(piepercent, "%"),radius = 1, 
      explode = 0.1, main = "Dates collected in July")
par(xpd=TRUE)
legend(title = "Day", 1,0.7,legend=July_data$Day,cex=0.7,yjust=0.2, xjust = -0.1,
       fill = rainbow(length(July_data$Quantity))) 

dev.off()

#Analysis 1.2.3: Total Quantity of the houses rent post on each date
date_num <- rent_data %>%
  group_by(Date) %>%
  dplyr::summarise(Quantity = n())

ggplot(date_num, aes(x=Date, y=Quantity)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_x_date(date_breaks = "3 day", date_labels = "%d %b") +
  scale_y_continuous(breaks = seq(from = 0, to = 350,  by = 50)) +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 6)) +
  ggtitle("The quantity of House Rent Post on Each Day")


date_num_arr <- arrange(date_num, desc(Quantity))
View(date_num_arr)
date_num_arr_acc <- arrange(date_num, Quantity)
View(date_num_arr_acc)


#The data is collected from 13 April until 11 July
  
#Analysis 1.3: Find the first, middle and last rent of the month price
#So we divided the day to 1-10, 11-20, 21-30, find the avarage rent price
#first got salary, many ppl can rent stg
#Quantity
data_values_range = rent_data %>%                  
  mutate(ranges = cut(rent_data$Day,
                      c(0, 10, 20, 31))) %>% 
  group_by(ranges) %>% 
  dplyr::summarise(sums = sum(Rent_Price), Quantity = n()) %>%
  as.data.frame()

ggplot(data_values_range, aes(x=ranges, y=Quantity)) +
  geom_line(aes(group=1), col= "blue") +
  geom_point(col = "Green", stat = "identity") +
  geom_text(aes(label= Quantity, vjust = -1)) +
  labs(title = "Quantity Of Each Range", x = "Ranges of Days", y = "Quantity") +
  theme_ipsum() + 
  theme(axis.title.y = element_text(angle=0, vjust = 0.5)) + 
  theme(axis.title.x = element_text(angle=0, hjust = 0.5))

#Price
data_values_range = rent_data %>%                  
  mutate(ranges = cut(rent_data$Day,
                      c(0, 10, 20, 31))) %>% 
  group_by(ranges) %>% 
  dplyr::summarise(sums = sum(Rent_Price), means = mean(Rent_Price)) %>%
  as.data.frame()
data_values_range

ggplot(data_values_range, aes(x=ranges, y=means)) +
  geom_line(aes(group=1), col= "blue") +
  geom_point(col = "purple", stat = "identity") +
  geom_text(aes(label= round(means, digits = 2), vjust = -1)) +
  labs(title = "Average Amount of Each Range", x = "Ranges of Days", y = "Average Amount") +
  theme_ipsum() + 
  theme(axis.title.y = element_text(angle=0, vjust = 0.5)) + 
  theme(axis.title.x = element_text(angle=0, hjust = 0.5))


#Price - only May, June
data_values_range2 = rent_data %>%                  
  mutate(ranges = cut(rent_data$Day,
                      c(0, 10, 20, 31))) %>%
  filter(Month == 5 | Month == 6) %>%
  group_by(ranges) %>% 
  dplyr::summarise(sums = sum(Rent_Price), means = mean(Rent_Price)) %>%
  as.data.frame()
data_values_range2

ggplot(data_values_range2, aes(x=ranges, y=means)) +
  geom_line(aes(group=1), col= "blue") +
  geom_point(col = "purple", stat = "identity") +
  geom_text(aes(label= round(means, digits = 2), vjust = -1)) +
  labs(title = "Mean Amount of Each Range (May & June)", x = "Ranges of Days", y = "Average Amount") +
  theme_ipsum() + 
  theme(axis.title.y = element_text(angle=0, vjust = 0.5)) + 
  theme(axis.title.x = element_text(angle=0, hjust = 0.5))

#only May, June (Quantity)
data_values_range2 = rent_data %>%                  
  mutate(ranges = cut(rent_data$Day,
                      c(0, 10, 20, 31))) %>%
  filter(Month == 5 | Month == 6) %>%
  group_by(ranges) %>% 
  dplyr::summarise(Quantity = n()) %>%
  as.data.frame()
data_values_range2

ggplot(data_values_range2, aes(x=ranges, y=Quantity)) +
  geom_line(aes(group=1), col= "blue") +
  geom_point(col = "Green", stat = "identity") +
  geom_text(aes(label= Quantity, vjust = -1)) +
  labs(title = "Quantity of Each Range (May & June)", x = "Ranges of Days", y = "Quantity") +
  theme_ipsum() + 
  theme(axis.title.y = element_text(angle=0, vjust = 0.5)) + 
  theme(axis.title.x = element_text(angle=0, hjust = 0.5))

#Analysis 1.4: Find the cheapest date
cheapest_date <- rent_data %>%
  group_by(Date) %>%
  dplyr::summarise(AVG = format(round(mean(Rent_Price),1), nsmall = 1))
cheapest_date <- arrange(cheapest_date, AVG)
View(cheapest_date)

highest_date <- rent_data %>%
  group_by(Date) %>%
  dplyr::summarise(AVG = format(round(mean(Rent_Price),1), nsmall = 1))
highest_date <- arrange(highest_date, desc(AVG))
View(highest_date)


#Question 2: How does family background (tenants) affect their rent?

#Analysis 2.1: Which type of tenants is the most?
tenants_type = rent_data %>% group_by(Tenant) %>% dplyr::summarise(Quantity = n())
tenants_type

#Number
ggplot(tenants_type) +
  aes(x = Tenant, y = Quantity) +
  geom_col(aes(fill = Tenant)) +
  geom_text(aes(label = Quantity), vjust = -0.5) +
  labs(
    x = "Tenant",
    y = "Count",
    title = "The quantity of Each Tenant"
  ) +
  scale_fill_brewer(palette="Set2")

#Percentages (Donut chart)
piepercent_num_tenants <- data.frame(Tenant = tenants_type$Tenant,Quantity = tenants_type$Quantity ) 
piepercent_num_tenants$fraction <- round(100*tenants_type$Quantity/sum(tenants_type$Quantity), 1)

piepercent_num_tenants$ymax <- cumsum(piepercent_num_tenants$fraction)
piepercent_num_tenants$ymin <- c(0, head(piepercent_num_tenants$ymax, n=-1))
piepercent_num_tenants$labelPosition <- (piepercent_num_tenants$ymax + piepercent_num_tenants$ymin) / 2
piepercent_num_tenants$label <- paste0(piepercent_num_tenants$Tenant, 
                                       "\n Quantity: ", piepercent_num_tenants$Quantity,
                                       "\n Percent: ", paste(piepercent_num_tenants$fraction, "%"))
ggplot(piepercent_num_tenants, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tenant)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "lavender"))+
  ggtitle("The quantity and percentages of each tenant") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))


#Analysis 2.2: Find the relationship between Rent Price and tenants
avg_rent_tenant = aggregate(rent_data$Rent_Price, list(rent_data$Tenant), FUN=mean)
names(avg_rent_tenant) = c("Tenant", "Average_Rent")

ggplot(avg_rent_tenant, aes(x=Tenant, y=Average_Rent)) +
  geom_line( aes(group=1), color="#6698FF", size=1.1, alpha=0.9) +
  geom_point(col = "#F67280") +
  geom_text(aes(label = round(Average_Rent, digits = 2)), 
            hjust = 0.5,  vjust = -1, col = 'Black', size = 4) +
  labs(title="Average Rent for Each Type of Tenant", x="Tenant", y="Average Rent") +
  theme_ipsum() +
  theme(panel.background = element_rect(fill = "lavender")) +
  theme(axis.title.y = element_text(size = 15, angle=90, hjust = 0.5)) + 
  theme(axis.title.x = element_text(size = 15, angle=0, hjust = 0.5)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))


#-----------House Condition-----------
#Analysis 2.3: The preferred Floor of different tenants
#Available Floor
p2_meds <- ddply(rent_data, .(Tenant), summarise, med = median(House_Floor))
p2_means <- ddply(rent_data, .(Tenant), summarise, meann = mean(House_Floor))

ggplot(rent_data, aes(x=Tenant, y=House_Floor, fill=Tenant)) + 
  geom_violin()+ 
  labs(title = "The relationship between Size and Tenants", y = "Available Floor")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="orange", fill="orange") +
  scale_x_discrete(breaks = seq(from = 0, to = 80, by = 1)) +
  geom_text(data = p2_meds, aes(x = Tenant, y = med, label =  paste("Median:",med)), 
            size = 5, vjust = 1.5) +
  geom_text(data = p2_means, aes(x = Tenant, y = meann, label = paste("Mean:",round(meann, digits = 1))), 
            size = 5, vjust = -1.3) + 
  theme(plot.background = element_rect(fill = "#D6F1F9" ))+
  theme(panel.background = element_rect(fill = "#D6F9E6"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set3")


#Analysis 2.4: The relationship between Size and tenants
p_meds <- ddply(rent_data, .(Tenant), summarise, med = median(Size))
p_means <- ddply(rent_data, .(Tenant), summarise, meann = mean(Size))

ggplot(data = rent_data, aes(x = Tenant , y = Size, fill = Tenant))+
  geom_boxplot()+ 
  labs(title = "The relationship between Size and Tenants")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="orange", fill="orange") +
  geom_text(data = p_meds, aes(x = Tenant, y = med, label =  paste("Median:",med)), 
            size = 5, vjust = 1.5) +
  geom_text(data = p_means, aes(x = Tenant, y = meann, label = paste("Mean:",round(meann, digits = 1))), 
            size = 5, vjust = -1.3) +
  theme(legend.position="none") + 
  theme(plot.background = element_rect(fill = "#EBB9FF" ))+
  theme(panel.background = element_rect(fill = "#F7FC8C"))+
  theme(plot.title = element_text(hjust = 0.5))

#Density
ggplot(rent_data, aes(Size)) +
  geom_density(aes(fill = Tenant)) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_ipsum() +
  labs(title = "The relationship between Size and Tenants",
       x = "Size", 
       y ="Density")


#Fiona Liow Zi Lin
#TP060687
#Question 3: How does the structure of the house affect the house rent?

#Analysis 3-1: Find the relationship between bathroom number and the number of rent.
#Bar chart representation
bath_num <- rent_data %>%
  group_by(Bathroom) %>%
  dplyr::summarise(Rent = n())
bath_num

ggplot(bath_num, aes(x = Bathroom, y = Rent)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(bath_num$Bathroom))) +
  ggtitle("The relationship between bathroom number and number of rent") + 
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(Bathroom, label =Rent), position = position_dodge(width = 0.1))


#Pie chart representation
library(ggplot2)

df = data.frame(x <- c(bath_num$Rent),
                Bathroom_Number <- c('1','2','3','4','5','6','7','10'))

ggplot(df, aes(x="", y=x, fill=Bathroom_Number)) +geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void()+
  ggtitle("The relationship between bathroom number and number of rent")


#3D pie chart representation
#calculate data in percentage
lab <- paste0(round(bath_num$Rent/sum(bath_num$Rent) * 100, 2), "%")
#Bathroom number labeling
pie3D(x,labels=Bathroom_Number,
      main="The relationship between bathroom number and number of rent ",
      border = "white",theta = 1.5, radius = 1.2)
#Percentage labeling
pie3D(x,labels=lab,
      main="The relationship between bathroom number and number of rent ",
      border = "white",theta = 1.5, radius = 1.2)


#Analysis 3-2: Find the relationship between bathroom number and tenant preferred.
#Count plot representation
ggplot(rent_data, aes(x = Bathroom, y = Tenant)) + 
  geom_count() +
  labs(title="The co-variation between bathroom number and tenant preferred", x="Bathroom Number", y="Tenant Preferred")

#Bar chart representation
#facet
ggplot(rent_data, aes(x=Bathroom)) + 
  geom_bar() +
  facet_wrap(~Tenant)


#Analysis 3-3: Find the relationship between bathroom number and renting price.
#Preparing data
avg_rent_bath=aggregate(rent_data$Rent_Price, list(rent_data$Bathroom), FUN=mean) 
col_names<-list('Bathroom.Number','average.Rental')
colnames(avg_rent_bath) <- c(col_names)

#Box plot representation
ggplot(rent_data, aes(group=Bathroom,x = Bathroom, y = Rent_Price)) + 
  geom_boxplot() +
  labs(title="The relationship between bathroom number and average rental price", x="Bathroom number", y="Average rental")

#Tree map representation
library(devtools)
install_github("wilkox/treemapify")
library(treemapify)

ggplot(avg_rent_bath, aes(area = average.Rental, fill = Bathroom.Number, label =Bathroom.Number)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)+
  labs(title = "Average Rental Price according to Bathroom Number")

#Analysis 3-4: Find the relationship between BHK combination and the number of rent.
#Bar chart representation
BHK_num <- rent_data %>%
  group_by(BHK) %>%
  dplyr::summarise(Rent = n())
BHK_num

ggplot(BHK_num, aes(x = BHK, y = Rent)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(BHK_num$BHK))) +
  ggtitle("The relationship between BHK combination and number of rent") + 
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(BHK, label =Rent), position = position_dodge(width = 0.1))


#Pie chart representation
library(ggplot2)

df = data.frame(x <- c(BHK_num$Rent),
                BHK_Number <- c('1','2','3','4','5','6'))

ggplot(df, aes(x="", y=x, fill=BHK_Number)) +geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void()+
  ggtitle("The relationship between BHK combination and number of rent")


#3D pie chart representation
#calculate data in percentage
lab <- paste0(round(BHK_num$Rent/sum(BHK_num$Rent) * 100, 2), "%")
#Bathroom number labeling
pie3D(x,labels=BHK_Number,
      main="The relationship between BHK combination and number of rent ",
      border = "white",theta = 1.5, radius = 1.2)

#Percentage labeling
pie3D(x,labels=lab,
      main="The relationship between BHK combination and number of rent ",
      border = "white",theta = 1.5, radius = 1.2)

#Analysis 3-5: Find the relationship between BHK combination and tenant preferred.
#Count plot representation
ggplot(rent_data, aes(x = BHK, y = Tenant)) + 
  geom_count() +
  labs(title="The co-variation between BHK combination and tenant preferred", x="BHK", y="Tenant Preferred")

#Bar chart representation
#facet
ggplot(rent_data, aes(x=BHK)) + 
  geom_bar() +
  facet_wrap(~Tenant)


#Analysis 3-6: Find the relationship between BHK combination and renting price.
#Preparing data
avg_rent_BHK=aggregate(rent_data$Rent_Price, list(rent_data$BHK), FUN=mean) 
col_names<-list('BHK.combination','average.Rental')
colnames(avg_rent_BHK) <- c(col_names)

#Box plot representation
ggplot(rent_data, aes(group=BHK,x = BHK, y = Rent_Price)) + 
  geom_boxplot() +
  labs(title="The relationshiip between BHK combination and average rental price", x="BHK", y="Average rental")

#Tree map representation
library(devtools)
install_github("wilkox/treemapify")
library(treemapify)

ggplot(avg_rent_BHK, aes(area = average.Rental, fill = BHK.combination, label =BHK.combination)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)+
  labs(title = "Average Rental Price according to BHK combination")

#Analysis 3-7: Find the relationship between Size and the Rent price.
ggplot(rent_data, aes(Size, Rent_Price)) +
  geom_point(color = "darkgreen") +
  geom_jitter(color = "pink") +
  geom_smooth(method = "gam", se= TRUE) +
  labs(title = "The Relationship between Size and the Rent Price",
       y = "Rent Price",
       x = "Size")

#Question 4:How does The Location affects The House Rent?
#Analysis 4.1: Relationship between Rent and City
RelationshipRentCity=rent_data %>%                  
  group_by(City) %>%
  dplyr::summarise(Rent_Price) %>%
  as.data.frame()
RelationshipRentCity

library(ggplot2)
#Line Graph in Point
ggplot(RelationshipRentCity) +
  aes(x = City, y = Rent_Price, fill = City, colour = City) +
  geom_line()+
  geom_point(shape = "circle", 
             size = 1.5) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(y = "Rent Price", 
       title = "The Relationship between City and Rent") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18L, 
                                  face = "bold"))

#Analysis 4.2: Find The Average House Rent in Each City.
AverangeRentCity=rent_data%>%
  group_by(City)%>%
  dplyr::summarise(AVGHouseRent=mean(Rent_Price))%>%
  as.data.frame()
View(AverangeRentCity)

#Box Plot
p_means <- ddply(rent_data, .(City), summarise, meann = mean(Rent_Price))

ggplot(rent_data, aes(x=City,y=Rent_Price, fill=City))+
  scale_fill_brewer(palette="Pastel1")+
  geom_boxplot(aes(color=City))+  
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.9, color="black", fill="red") +
  geom_text(data = p_means, aes(x = City, y = meann, label = paste("Mean:",round(meann, digits = 1))),
            size = 3.5, vjust=1.2, hjust=-0.1) +
  coord_flip()+
  labs(title="The Average House Rent in Each City")


#Analysis 4.3: What are The Total Rent Fees of House in Each City?
#Total rent of house
TotalRentCity=rent_data %>%                  
  group_by(City) %>%
  dplyr::summarise(Sums = sum(Rent_Price)) %>%
  as.data.frame()
TotalRentCity = arrange(TotalRentCity, desc(Sums))
View(TotalRentCity)

library(ggplot2)
#Point Graph
ggplot(TotalRentCity) +
  aes(x = City, y = Sums, fill = Sums) +
  geom_point(shape = "circle filled", size = 1.5, 
             colour = "#11462A") +
  scale_fill_gradient(low = "#391343", high = "#F756D1") +
  labs(x = "City", y = "Rental", 
       title = "Total Rent of City") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18L, face = "bold", 
  hjust = 0.5), axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, 
  face = "bold"))

#Analysis 4.4: The Numbers of House Available for Rent in Each City?
HouseCity=rent_data%>%
  group_by(City)%>%
  dplyr::summarise(Quantity = n ())%>%
  as.data.frame()
HouseCity = arrange(HouseCity, desc(Quantity))
View(HouseCity)

library(ggplot2)
#Bar Plot
ggplot(HouseCity) +
  aes(x = City, fill = City, weight = Quantity) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(x = "City", y = "Quantity of House", title = "The Number of House Available") +
  theme_gray() +
  theme(legend.position = "left", plot.title = element_text(size = 18L, face = "bold", hjust = 0.5), 
  axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, 
  face = "bold"))



#Analysis 4.5: What is The Number of Houses by Area Locality in Each City?
mixdata=rbind.data.frame(LocalityKolkata1,LocalityBangalore1,LocalityChennai1,LocalityDelhi1,LocalityHyderabad1,LocalityMumbai1)
mixdata
#Line with Point Graph
install.packages("hrbrthemes")
library(hrbrthemes)
Analysis5.6graph= ggplot(mixdata,aes(Quantity,COUNT))+
  geom_line(color="lightblue",size=0.5,alpha=0.9)+
  geom_point(shape=21, color="black", fill="#69b3a2", size=1)+
  theme_ipsum() +
  facet_wrap(~ City)+
  ggtitle("Number of Houses by Area Locality in Each City")
Analysis5.6graph=ggplotly(Analysis5.6graph)
Analysis5.6graph

#4.5.1 Locality of Kolkata
LocalityKolkata=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Kolkata"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityKolkata


LocalityKolkata1=LocalityKolkata %>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityKolkata1)

#4.5.2 Locality of Bangalore
LocalityBangalore=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Bangalore"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityBangalore

LocalityBangalore1=LocalityBangalore%>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityBangalore1)

#4.5.3 Locality of Delhi
LocalityDelhi=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Delhi"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityDelhi

LocalityDelhi1=LocalityDelhi%>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityDelhi1)

#4.5.4 Locality of Chennai
LocalityChennai=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Chennai"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityChennai

LocalityChennai1=LocalityChennai%>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityChennai1)

#4.5.5 Locality of Hyderabad
LocalityHyderabad=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Hyderabad"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityHyderabad

LocalityHyderabad1=LocalityHyderabad%>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityHyderabad1)

#4.5.6 Locality of Mumbai
LocalityMumbai=rent_data%>%
  select(Locality,City)%>%
  group_by(Locality,City)%>%
  filter(City %in% c("Mumbai"))%>%
  dplyr::summarise(Quantity = n())%>%
  as.data.frame()
LocalityMumbai

LocalityMumbai1=LocalityMumbai%>%
  group_by(Quantity,City)%>%
  dplyr::summarise(COUNT = n())%>%
  as.data.frame()
View(LocalityMumbai1)

#Analysis 4.6: What is the highest house rent located in Mumbai?
MumbaiRent=rent_data%>%
  filter(City %in% c("Mumbai"))%>%
  group_by(Locality)%>%
  dplyr::summarise(HighPrice=mean(Rent_Price))%>%
  top_n(10)
MumbaiRent
MumbaiRent=MumbaiRent[order(MumbaiRent$HighPrice,decreasing=TRUE),]
View(MumbaiRent)



library(ggplot2)

#Point Graph
ggplot(MumbaiRent) +
 aes(x = HighPrice, y = Locality, fill = Locality, colour = Locality) +
 geom_point(shape = "triangle square", 
 size = 1.5) +
 scale_fill_hue(direction = 1) +
 scale_color_hue(direction = 1) +
 labs(y = "Rent Price", 
 subtitle = "Highest Rent from Top10 Localities in Mumbai") +
 theme_gray() +
 theme(plot.subtitle = element_text(size = 18L, 
 face = "bold"))

#Analysis 4.7: What is The Range of House Rent?
#Range of rental Data
range(rent_data$Rent_Price)
RentalRange_1200_3500000 <- rent_data %>%
  mutate(RentalRange = cut(rent_data$Rent_Price,
                           seq(1200,3500000,100000)))%>%
  group_by(RentalRange) %>%
  dplyr::summarise(Quantity = n())
RentalRange_1200_3500000$RentalRange <- c("1000-101000","101000-201000","201000-301000",
                                          "301000-401000","401000-501000","501000-601000",
                                          "601000-701000","801000-901000","901000-1001000",
                                          "1101000-1201000","1201000-3500000")
RentalRange_1200_3500000
RentalRange_1200_3500000$RentalRange

#Bar Plot
par(mar = c(6,6,6,6))
ggplot(RentalRange_1200_3500000,aes(x = RentalRange, y = Quantity, fill=RentalRange)) +
  geom_bar(stat = "identity", width = 0.5, color = "Black")+
  ggtitle("The Range of House Rent (Range Between 1000 - 3500000)")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))+
  geom_text(aes(RentalRange, label = Quantity), vjust = -1, position = position_dodge(width = 0.1))



#Analysis 4.8: What is the Top 10 house rent available according to the locality?
HouseRentLocality=rent_data%>%
  group_by(Locality)%>%
  dplyr::summarise(LocalityPrice= mean(Rent_Price))%>%
  top_n(10)
HouseRentLocality
HouseRentLocality=HouseRentLocality[order(HouseRentLocality$LocalityPrice,decreasing=TRUE),]
View(HouseRentLocality)

options(scipen=999)
#Generate Lollipop Graph (HouseRentLocality) 
Lollipopgrap=ggplot(HouseRentLocality, aes(x=Locality, y=LocalityPrice)) +
  geom_point(size=7, color="blue", fill=alpha("purple", 0.3), alpha=0.7, shape=21, stroke=2) + 
  geom_segment( aes(x=Locality, xend=Locality, y=0, yend=LocalityPrice), lwd=1.5, color="black")+
  theme_light()+ theme(panel.grid.major.x = element_blank(),
                       panel.border = element_blank(), axis.ticks.x = element_blank(),
                       plot.title = element_text(hjust=0.5, face="bold", size=16))+
  geom_text(aes(label = LocalityPrice), color = "black", size = 4,
            position=position_dodge(1), vjust=-0.75, hjust=1.3)+coord_flip()+
  ggtitle("Top 10 House Rent Based on Locality")

Lollipopgrap

#Analysis 4.9: What is the Top 10 total number of houses available for rent by area locality?
HouseLocality=rent_data%>%
  group_by(Locality)%>%
  dplyr::summarise(TotalNumberHouseLocality=n())%>%
  top_n(10)
HouseLocality
HouseLocality=HouseLocality[order(HouseLocality$TotalNumberHouseLocality,decreasing=TRUE),]
View(HouseLocality)

#Generate Pie Chart (Analysis 4.9)
stritoint=strtoi(HouseLocality$TotalNumberHouseLocality)
stritoint
pie_lable=paste0(round(100*stritoint/sum(stritoint),2),"% - ",HouseLocality$TotalNumberHouseLocality, " - ",HouseLocality$Locality)
pie3D(HouseLocality$TotalNumberHouseLocality, border="white", col=hcl.colors(length(HouseLocality$Locality),"Spectral"),labelcex = 0.7,radius = 0.8,
      labels=pie_lable,theta = 1.25,
      main="The Top 10 Total Number of Houses Available for Rent(Locality) ")


#Question 5: How does the house floor affect the house rent?
#Analysis 5.1 The total number of floor available?
HouseTotalFloor=rent_data%>%
  group_by(House_Floor)%>%
  dplyr::summarise(Quantity=n())
View(HouseTotalFloor)


library(ggplot2)
#Area Graph
ggplot(HouseTotalFloor) +
  aes(x = House_Floor, y = Quantity) +
  geom_area(fill="purple",alpha=0.5,color=1,lwd=0.5,linetype=1) +
  scale_fill_viridis_c(option = "inferno", 
                       direction = 1) +
  labs(x = "House Floor", title = "The Total Number of House Floor") +
  theme_minimal()


#Analysis 5.2 the Average of floor rent of floor available?
AVGRentTotalFloor=rent_data%>%
  group_by(House_Floor)%>%
  dplyr::summarise(AVG=mean(Rent_Price))
AVGRentTotalFloor$AVG=as.integer(AVGRentTotalFloor$AVG)
summary(AVGRentTotalFloor)

library(ggplot2)
#line graph
ggplot(AVGRentTotalFloor) +
  aes(x = House_Floor, y = AVG, colour = House_Floor) +
  geom_line() +
  scale_color_viridis_c(option = "plasma", 
                        direction = 1) +
  labs(x = "House Floor", y = "Average", title = "The Average of Floor Rent") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18L, face = "bold", hjust = 0.5))


#------------------------------------------------------------question 6--------------------------------------------------------------------
# Question 6: How does the furnishing status affect house renting?
# Analysis 6-1: The total number for each type of furnishing status 

qauntity_each_furnish_type <- rent_data %>% group_by(Furnishing_Status) %>% dplyr::summarise(Quantity = n())
qauntity_each_furnish_type
  
  # bar chart
ggplot(qauntity_each_furnish_type, aes(x = Furnishing_Status, y = Quantity)) +
  geom_bar(stat = "identity", width = 0.7, color = "Black", fill = heat.colors(length(qauntity_each_furnish_type$Furnishing_Status))) + 
  ggtitle("The Quantity for each type of Furnishing Status") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  geom_text(aes(Furnishing_Status, label = Quantity), cex = 5, vjust = 1.5)

  # pie chart
color <- brewer.pal(length(qauntity_each_furnish_type$Quantity), "Set2")

pie(qauntity_each_furnish_type$Quantity, 
    labels = paste0(qauntity_each_furnish_type$Furnishing_Status, 
                    round(100 * qauntity_each_furnish_type$Quantity/sum(qauntity_each_furnish_type$Quantity),2), "%"), col = color, density = 50, 
    explode = 0.1, main = "The Quantity for each type of Furnishing Status")


#Analysis 6-2: Relationship between rent price and furnishing status

  # average price for each furnishing type
avg_price_furnish <- rent_data %>% group_by(Furnishing_Status) %>% dplyr::summarise(mean_furnish_status = mean(Rent_Price)) %>%
  as.data.frame()

fp_result <- ddply(rent_data, .(Furnishing_Status), summarise, fpmean = mean(Rent_Price))
fp_result

  # boxplot chart
ggplot(rent_data, aes(x = Furnishing_Status, y = Rent_Price, fill = Furnishing_Status)) + 
  scale_fill_brewer(palette = "Pastel2") +
  geom_boxplot(aes(color = Furnishing_Status)) + 
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 1.9, color = "black", fill = "pink") +
  geom_text(data = fp_result, aes(x = Furnishing_Status, y = fpmean, label = paste("Mean is", round(fpmean, digits = 2))),
            size = 3, vjust = 2, hjust = -0.4) + coord_flip() +
  labs(title = "Average Price of each Furnishing Type")


# Analysis 6-3: The total number of each type of tenant

tenant_type <- rent_data %>% group_by(Tenant) %>% dplyr::summarise(Quantity = n())
tenant_type

  # bar + pie chart
num_tenant_pie <- ggplot(tenant_type) +
  aes(x = Tenant, fill = Tenant, weight = Quantity) +
  geom_bar() +
  scale_fill_brewer(palette = "PRGn", direction = 1) +
  labs(
    x = "Tenant",
    y = "Quantity",
    title = "Total Number of each type of Tenant"
  ) +
  theme_minimal()

num_tenant_pie + coord_polar()


# Analysis 6-4: What is the relationship between tenant and furnishing status?

  # bar chart
ggplot(rent_data, aes(`Furnishing_Status`)) + geom_bar(color = "Black", fill = "Purple") + 
  facet_wrap(~`Tenant`) + 
  labs(title = "Quantity of furnishing status based on each individual tenant types", y = "Amount of Tenant") 



# Analysis 6-4-1: Quantity of each type of furnishing status based on Bachelors

count_barchelor <- rent_data %>% group_by(Furnishing_Status)%>% filter(Tenant == "Bachelors")
View(count_barchelor)

pre_barchelor <- count_barchelor %>% group_by(Furnishing_Status) %>% dplyr::summarise(Quantity = n() )
pre_barchelor

  # bar chart
ggplot(pre_barchelor) +
  aes(
    x = Furnishing_Status,
    y = Quantity,
    fill = Furnishing_Status
  ) +
  geom_col() +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    x = "Furnishing Status",
    y = "Quantity",
    title = "Furnishing Status based on Bacrchelor"
  ) + geom_text(aes(Furnishing_Status, label = Quantity), cex = 5, hjust = 1, vjust = 0) +
  coord_flip() +
  theme_bw()



# Analysis 6-4-2: Quantity of each type of furnishing status based on Family
count_family <- rent_data %>% group_by(Furnishing_Status)%>% filter(Tenant == "Family")
View(count_family)

pre_family <- count_family %>% group_by(Furnishing_Status) %>% dplyr::summarise(Quantity = n() )
pre_family

  # bar chart
ggplot(pre_family) +
  aes(
    x = Furnishing_Status,
    y = Quantity,
    fill = Furnishing_Status
  ) +
  geom_col() +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  labs(
    x = "Furnishing Status",
    y = "Quantity",
    title = "Furnishing Status based on Family"
  ) + geom_text(aes(Furnishing_Status, label = Quantity), cex = 5, hjust = 1, vjust = 0) +
  coord_flip() +
  theme_minimal()



# Analysis 6-4-3: Quantity of each type of furnishing status based on Bachelors/Family
count_bf <- rent_data %>% group_by(Furnishing_Status)%>% filter(Tenant == "Bachelors/Family")
View(count_bf)

pre_bf <- count_bf %>% group_by(Furnishing_Status) %>% dplyr::summarise(Quantity = n() )
pre_bf

  # bar chart
ggplot(pre_bf) +
  aes(
    x = Furnishing_Status,
    y = Quantity,
    fill = Furnishing_Status
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(Furnished = "#D3D3D3",
               `Semi-Furnished` = "#CA4778",
               Unfurnished = "#38796E")
  ) +
  labs(
    x = "Furnishing Status",
    y = "Quantity",
    title = "Furnishing status based on Bachelors/Family"
  ) + geom_text(aes(Furnishing_Status, label = Quantity), cex = 5, hjust = 1, vjust = 0) +
  coord_flip() +
  theme_minimal()


#------------------------------------------------------------question 7--------------------------------------------------------------------
# Question 7: How does the point of contact effect on house renting?
# Analysis 7-1: The total number for each type of point of contact

qauntity_each_point <- rent_data %>% group_by(Point_of_Contact) %>% dplyr::summarise(Quantity = n())
qauntity_each_point

  # point chart
ggplot(qauntity_each_point) +
  aes(
    x = Quantity,
    y = Point_of_Contact,
    fill = Point_of_Contact
  ) + geom_point(shape = "circle filled", size = 1.5, colour = "#112446") +
  scale_fill_manual(
    values = c(`Contact Agent` = "#B3E2CD",
               `Contact Builder` = "#f756D1",
               `Contact Owner` = "SKYBLUE")) +labs(
                 x = "Quantity",
                 y = "Point of OCntact",
                 title = "The Quantity of each Point of Contact") + theme_minimal()


  # pie chart
color <- brewer.pal(length(qauntity_each_point$Quantity), "Set2")

pie(qauntity_each_point$Quantity, 
    labels = paste0(qauntity_each_point$Point_of_Contact, 
                    round(100 * qauntity_each_point$Quantity/sum(qauntity_each_point$Quantity),2), "%"), 
    col = color, density = 50, 
    explode = 0.1, main = "The Quantity of each Point of Contact")


# Analysis 7-2: The average price of rent in each point of contact

  # Calculate the average price for each point of contact
avg_price_contact <- rent_data %>% group_by(Point_of_Contact) %>% dplyr::summarise(mean_furnish_status = mean(Rent_Price)) %>%
  as.data.frame()

cp_result <- ddply(rent_data, .(Point_of_Contact), summarise, cpmean = mean(Rent_Price))
cp_result

  # line chart
ggplot(cp_result, aes(x= Point_of_Contact, y = cpmean)) + 
  geom_line(aes(group = 1), color = "Black", linewidth = 1, alpha = 0.5) + geom_point(col = "Red") +
  geom_text(aes(label = round(cpmean, digits = 2), hjust = 0.5, vjust = -0.5)) + 
  labs(title = "Average Price of Each Type of Point of Contact", x = "Point of Contact", y = "Average Price") + 
  theme(panel.background = element_rect(fill = "lightblue"))



# Analysis 7-3: What is the relationship between tenant and point of contact?

  # bar chart
ggplot(rent_data, aes(`Point_of_Contact`)) + geom_bar(color = "grey", fill = "darkgrey") + 
  facet_wrap(~`Tenant`) + 
  labs(title = "Quantity of point of contact based on each individual tenant types", y = "Amount of Tenant") 



# Analysis 7-3-1: Quantity of each type of point of contact based on Bachelors

count_barchelor <- rent_data %>% group_by(Point_of_Contact)%>% filter(Tenant == "Bachelors")
View(count_barchelor)

pre_barchelor <- count_barchelor %>% group_by(Point_of_Contact) %>% dplyr::summarise(Quantity = n() )
pre_barchelor

  # bar chart
ggplot(pre_barchelor) +
  aes(
    x = Point_of_Contact,
    y = Quantity,
    fill = Point_of_Contact
  ) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Point of Contact",
    y = "Quantity",
    title = "Quantity of each type of point of contact based on Bachelors"
  ) + 
  theme_bw() + geom_text(aes(Point_of_Contact, label = Quantity), cex = 5, vjust = 1.5)



# Analysis 7-3-2: Quantity of each type of point of contact based on Family

count_family <- rent_data %>% group_by(Point_of_Contact)%>% filter(Tenant == "Family")
View(count_family)

pre_family <- count_family %>% group_by(Point_of_Contact) %>% dplyr::summarise(Quantity = n() )
pre_family

  # lollipop chart
ggplot(pre_family, aes(x= Point_of_Contact, y = Quantity)) +
  geom_point(size = 6, color = "Grey", fill = alpha("skyblue", 0.3), alpha = 0.7, shape = 21, stroke = 2) + 
  geom_segment(aes(x = Point_of_Contact, xend = Point_of_Contact, y = 0, yend = Quantity), lwd = 1.5, color = "black") +
  theme_minimal() + theme(panel.grid.major.x = element_blank(), panel.border = element_blank(), 
                          axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  geom_text(aes(label = Quantity), color = "black", size = 4, position = position_dodge(1), vjust = -1, hjust = 0.5) +
  ggtitle("Quantity of each type of point of contact based on Family")



# Analysis 7-3-3: Quantity of each type of point of contact based on Bachelors/Family 
count_bf <- rent_data %>% group_by(Point_of_Contact)%>% filter(Tenant == "Bachelors/Family")
View(count_bf)

pre_bf <- count_bf %>% group_by(Point_of_Contact) %>% dplyr::summarise(Quantity = n() )
pre_bf

  # bar chart
ggplot(pre_bf) +
  aes(
    x = Point_of_Contact,
    y = Quantity,
    fill = Point_of_Contact
  ) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Point of Contact",
    y = "Quantity",
    title = "Quantity of each type of point of contact based on Bachelors/Family "
  ) +
  theme_minimal() + theme_bw() + geom_text(aes(Point_of_Contact, label = Quantity), cex = 5, vjust = 1)

  # pie chart
color <- brewer.pal(length(pre_bf$Quantity), "Set3")

pie3D(pre_bf$Quantity, 
      labels = paste0(pre_bf$Point_of_Contact, 
                      round(100 * pre_bf$Quantity/sum(pre_bf$Quantity),2), "%"), col = color, 
      main = "The Quantity of each Point of Contact")






























