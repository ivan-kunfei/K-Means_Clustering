
install.packages('scatterpot3d')


library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(dply)
library(labeling)
library(tidyr)

NYCdf <- NYC_HISTORICAL  %>%
  left_join(NEIGHBORHOOD[,1:2], by='NEIGHBORHOOD_ID')  %>%
  left_join(BUILDING_CLASS[,-2], by=c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID')) %>%
  select(NEIGHBORHOOD_NAME,SALE_DATE,SALE_PRICE,GROSS_SQUARE_FEET,RESIDENTIAL_UNITS,COMMERCIAL_UNITS,TYPE) %>%
  filter(SALE_PRICE>100,GROSS_SQUARE_FEET>100) %>%
  mutate(Year = year(SALE_DATE)) 

# Question 1-a
NYC.Bedford_Stuyvesant <- filter(NYCdf,NEIGHBORHOOD_NAME=='BEDFORD STUYVESANT',Year>2008) %>%
  group_by(Year)
Bedford_Stuyvesant.TotalSales.ByYear <- summarise(NYC.Bedford_Stuyvesant, TotalSales = n())
view(Bedford_Stuyvesant.TotalSales.ByYear)
ggplot() + geom_line(data = Bedford_Stuyvesant.TotalSales.ByYear, size=1, aes(x=Year,y=TotalSales, group=1,color='blue'))+ scale_color_discrete(name='Bedford_Stuyvesant',label='Sales')


Bedford_Stuyvesant.TotalSales <- summarise(Bedford_Stuyvesant.TotalSales.ByYear,TotalSales=sum(TotalSales))

# Question 1-b
# Mean sales and gross square feet since 2009 (Bedford_Stuyvesant)
Bedford_Stuyvesant.SalesSqFt <- filter(NYCdf,NEIGHBORHOOD_NAME=='BEDFORD STUYVESANT',Year>2008, TYPE=='RESIDENTIAL') 
Bedford_Stuyvesant.MeanSalesSqFt <- group_by(Bedford_Stuyvesant.SalesSqFt,Year) %>%
summarise(MeanSalePrice = mean(SALE_PRICE), MeanGrossFeet = mean(GROSS_SQUARE_FEET))
view(Bedford_Stuyvesant.MeanSalesSqFt)
ggplot() + geom_line(data = Bedford_Stuyvesant.MeanSalesSqFt, size=1, aes(x=Year,y=MeanSalePrice, group=1,color='blue'))+ scale_color_discrete(name='Bedford_Stuyvesant',label='MeanSalePrice')
ggplot() + geom_line(data = Bedford_Stuyvesant.MeanSalesSqFt, size=1, aes(x=Year,y=MeanGrossFeet, group=1,color='blue'))+ scale_color_discrete(name='Bedford_Stuyvesant',label='MeanGrossFeet')
Bedford_Stuyvesant.TotalMeanSalesSqFt <- summarise(Bedford_Stuyvesant.MeanSalesSqFt,MeanSalePrice = mean(MeanSalePrice), MeanGrossFeet = mean(MeanGrossFeet))


# Question 1-c
# five number summary for sales and gross square feet
summary(Bedford_Stuyvesant.MeanSalesSqFt)

# Question 1-d
# proportion of Residential, commercial, mixed and other sales
Bedford_Stuyvesant.Proportion <-   filter(NYCdf,NEIGHBORHOOD_NAME=='BEDFORD STUYVESANT',Year>2008) %>%
  group_by(TYPE) %>%
  summarise(TotalUnits=n()) %>%
  drop_na() %>%
  mutate(proportion=TotalUnits/sum(TotalUnits))
view(Bedford_Stuyvesant.Proportion)

a <- Bedford_Stuyvesant.Proportion$proportion
b <- Bedford_Stuyvesant.Proportion$TYPE
pie(x=a, labels=b)

# Question 1-e
# StandardDeviation for Sales Prices in Residential properties for each year
SdSalePrices.Residential.ByYear <- filter(NYCdf,NEIGHBORHOOD_NAME=='BEDFORD STUYVESANT',Year>2008, TYPE=='RESIDENTIAL') %>%
  group_by(Year) %>%
  select(SALE_PRICE, TYPE, RESIDENTIAL_UNITS) %>%
  summarise(sdSalePrice=sd(SALE_PRICE,na.rm=T)) 
ggplot() + geom_line(data=SdSalePrices.Residential.ByYear, size=1, aes(x=Year,y=sdSalePrice, group=1,color='blue'))+ scale_color_discrete(name='Bedford_Stuyvesant',label='StandardDeviation for Sales Prices')


# Question 1-f
# Correlation between sale price and grpss square feet in residential units
cor(Bedford_Stuyvesant.SalesSqFt[c(3,4)])
ggplot()+ geom_point(data=Bedford_Stuyvesant.SalesSqFt,aes(y=Bedford_Stuyvesant.SalesSqFt$SALE_PRICE,x=Bedford_Stuyvesant.SalesSqFt$GROSS_SQUARE_FEET))

# Q2: Perform K-means clustering, comparing your neighborhood to other neighborhoods
# Compare K-means cluster for Median Sales for Residential properties of all neighborhoods
KPI <- filter(NYCdf, TYPE=='RESIDENTIAL',Year>2008) %>%
  select(NEIGHBORHOOD_NAME,SALE_PRICE, GROSS_SQUARE_FEET) %>%
  group_by(NEIGHBORHOOD_NAME) %>%
  summarise(PricePerFeet = sum(SALE_PRICE)/sum(GROSS_SQUARE_FEET), MedSales=median(SALE_PRICE),sdSalePrice=sd(SALE_PRICE)) %>%
  drop_na()
view(KPI)


# identify the best number of clusters
# iterate from 1-15, the total squared errors is less, the chossen number is batter
zscore <- select(KPI,'PricePerFeet','MedSales','sdSalePrice')
zscore <- scale(zscore)

k.max <- 15
# tot.withinss = total squared errors
wss <- sapply(1:k.max, function(k){kmeans(zscore,k)$tot.withinss})

plot(1:k.max,wss,type='b',pch=19,frame=FALSE,
     xlab='Number of clusters k',
     ylab='Total within-clusters sum of squares')




clustering <- kmeans(zscore, centers=3)

zscore <- cbind(zscore, clustering$cluster)
zscore <- cbind(KPI[c(1)],zscore)

library(scatterplot3d)




scatterplot3d(x=zscore$sdSalePrice,y=zscore$MedSales,z=zscore$PricePerFeet,color=zscore$V4)

# Question 3  t-test
y1 = filter(NYCdf,NEIGHBORHOOD_NAME=='BEDFORD STUYVESANT',Year>2008) %>%
  select(SALE_PRICE,GROSS_SQUARE_FEET) %>%
  mutate (AverageResidentialCosts = SALE_PRICE/GROSS_SQUARE_FEET) %>%
  select(AverageResidentialCosts)
 
y2 = filter(NYCdf,NEIGHBORHOOD_NAME=='CROWN HEIGHTS',Year>2008) %>%
  select(SALE_PRICE,GROSS_SQUARE_FEET) %>%
  mutate (AverageResidentialCosts = SALE_PRICE/GROSS_SQUARE_FEET) %>%
  select(AverageResidentialCosts)
t.test(y1,y2)
