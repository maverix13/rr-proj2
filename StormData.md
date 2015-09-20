# Analysis of severe weather events on public health and economic consequences across United States
Maverix13  
September 13, 2015  

## Synopsis
This report explores  the NOAA Storm Database and answer some basic questions about severe weather events. 
The analysis covers:

* Across United States, events that are most harmful to public health.
* Across United States, events that have greatest economic consequences.

Results <% plug here%>

## Data Processing

Data is downloaded, if required. Code checks if data already exists. Data is then loaded into R. 
Note that a zip file is directly read using read.csv. There is no need to unzip it.

```r
#install.packages("devtools")
#devtools::install_github("renkun-ken/formattable")
library(knitr)
library(dplyr)
library(ggplot2)
library(formattable)
```

```r
if(!file.exists("data")){
    dir.create("data")
}
if(!file.exists("data/stormData.csv.bz2")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data/stormData.csv.bz2", method = "curl")
    
}

stormData <- read.csv("data/stormData.csv.bz2")
dim(stormData)
```

```
## [1] 902297     37
```

```r
head(stormData[,2:28])
```

```
##             BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE  EVTYPE
## 1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL TORNADO
## 2  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL TORNADO
## 3  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL TORNADO
## 4   6/8/1951 0:00:00     0900       CST     89    MADISON    AL TORNADO
## 5 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL TORNADO
## 6 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL TORNADO
##   BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN
## 1         0                                               0         NA
## 2         0                                               0         NA
## 3         0                                               0         NA
## 4         0                                               0         NA
## 5         0                                               0         NA
## 6         0                                               0         NA
##   END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES INJURIES
## 1         0                      14.0   100 3   0          0       15
## 2         0                       2.0   150 2   0          0        0
## 3         0                       0.1   123 2   0          0        2
## 4         0                       0.0   100 2   0          0        2
## 5         0                       0.0   150 2   0          0        2
## 6         0                       1.5   177 2   0          0        6
##   PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1    25.0          K       0           
## 2     2.5          K       0           
## 3    25.0          K       0           
## 4     2.5          K       0           
## 5     2.5          K       0           
## 6     2.5          K       0
```

### Preprocessing Data
As the histogram below shows, in the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. 

```r
stormData <- mutate(stormData, YEAR = as.numeric(format(as.Date(BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y")))
g <- ggplot(stormData, aes(YEAR, fill = ..count.., removePanelGrid=TRUE,removePanelBorder=TRUE)) +
  geom_histogram(binwidth=5) + 
  ylab("Number of Events") +
  scale_fill_gradient("# Events", low = "black", high = "gray") +
  ggtitle("Histogram: Events By Year")
print(g)
```

![](StormData_files/figure-html/unnamed-chunk-4-1.png) 

Hence for this report, data after 1996 is considered for analysis.


```r
stormData <- stormData[stormData$YEAR >= 1996,]
dim(stormData)
```

```
## [1] 653530     38
```

Following columns are of relevance for the current analysis

Column Name | Description
------------|------------
EVTYPE      | Type of storm event. Take note that similar storm events can be listed using different wording e.g. “coastal flood” and “coastal flooding.” Take note of this if you want to run a query grouping by event type. 
FATALITIES  | Number directly killed
INJURIES    | Number directly injured
PROPDMG		  |	Property damage in whole numbers and hundredths
PROPDMGEXP	|	A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)
CROPDMG		  |	Crop damage in whole numbers and hundredths
CROPDMGEXP	|	A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)

This above table is constructed based on information from http://ire.org/nicar/database-library/databases/storm-events/

Further reduction of stormData based on columns and row values


```r
## Just keeping relevant columns
relevantColumns <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
stormData <- stormData[,relevantColumns]
## Keep rows having any data
stormData <- subset(stormData, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
```

#### Mapping of Events to Group
Many events can be grouped together based on similarity. The categorization for this report is based on NOAA 2009 Annual Summaries (http://www.ncdc.noaa.gov/oa/climate/sd/annsum2009.pdf).


```r
stormData$EVGROUP <- "Others"
stormData[grepl("tornado|thunderstorm|hail|funnel|tstm|lig(.*?)ing", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Convection"
stormData[grepl("temperature|cold|cool|heat|hot|fire|dry|warm|wet|freez", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Extreme Temperature"
stormData[grepl("flood|rain|mud(.*?)slid", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Flood"
stormData[grepl("marine|sea|tide|coast|tsunami|current|surf|wave", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Marine"
stormData[grepl("tropic|cyclone|hurricane", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Cyclone"
stormData[grepl("avalanche|snow|blizzard|winter|wintry|ice", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Winter"
stormData[grepl("wind|dust", 
                       stormData$EVTYPE, ignore.case = TRUE), "EVGROUP"] <- "Wind"

stormData$EVGROUP <- as.factor(stormData$EVGROUP)
```

### Analysis on public health
Analysis of fatalities and injuries caused by a given storm event group. 


```r
pubHealthData <- stormData %>% 
    group_by(EVGROUP) %>% 
    summarize(fatalities = sum(FATALITIES), injuries = sum(INJURIES))
kable(pubHealthData)
```



EVGROUP                fatalities   injuries
--------------------  -----------  ---------
Convection                   2170      25533
Cyclone                       182       1661
Extreme Temperature          2267       9202
Flood                        1404       8670
Marine                        751        989
Others                        160       1302
Wind                         1034       7127
Winter                        764       3491

### Analysis on Economic Impact
Analysis of property and crop damage caused by a given event group. 

```r
unique(stormData$PROPDMGEXP)
```

```
## [1] K   M B
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(stormData$CROPDMGEXP)
```

```
## [1] K   M B
## Levels:  ? 0 2 B k K m M
```
Unique values for both property and crop damage exponents is ['K', 'M', 'B']. These values are mulitpliers as mentioned above in column descriptions.


```r
economicData <- stormData %>% 
    mutate(PROPDMGVAL = PROPDMG * (
    ifelse(PROPDMGEXP == "K", 1000,
    ifelse(PROPDMGEXP == "M", 1000000,
    ifelse(PROPDMGEXP == "B", 1000000000,
                              1  )))))  %>% 
    mutate(CROPDMGVAL = CROPDMG * (
    ifelse(CROPDMGEXP == "K", 1000,
    ifelse(CROPDMGEXP == "M", 1000000,
    ifelse(CROPDMGEXP == "B", 1000000000,
                              1  ))))) %>%
    group_by(EVGROUP) %>%
    summarize(propertyDamage = sum(PROPDMGVAL), cropDamage = sum(CROPDMGVAL))

formattable(economicData, list(
    propertyDamage=currency,
    cropDamage=currency 
))
```


|             EVGROUP|      propertyDamage|         cropDamage|
|-------------------:|-------------------:|------------------:|
|          Convection|  $39,955,370,310.00|  $2,787,145,900.00|
|             Cyclone|  $88,762,871,560.00|  $6,026,993,800.00|
| Extreme Temperature|   $7,812,842,200.00|  $3,530,582,630.00|
|               Flood| $159,904,859,000.00|  $7,067,994,900.00|
|              Marine|   $5,295,157,060.00|     $42,522,500.00|
|              Others|  $45,253,732,050.00| $13,439,935,500.00|
|                Wind|  $13,369,105,350.00|  $1,736,767,400.00|
|              Winter|   $6,413,677,850.00|    $120,786,100.00|

## Results

```r
fData <- pubHealthData %>%
    select(EVGROUP, fatalities) %>%
    mutate(
        cumFatal = cumsum(fatalities),
        cumPerc = cumFatal/sum(fatalities)
    )
#mtcars3$car <-factor(mtcars2$car, levels=mtcars2[order(mtcars$mpg), "car"])
fData$EVGROUP <- factor(pubHealthData$EVGROUP, levels=pubHealthData[order(desc(pubHealthData$fatalities)), "EVGROUP"])
kable(fData)
```



EVGROUP    fatalities   cumFatal     cumPerc
--------  -----------  ---------  ----------
NA               2170       2170   0.2485112
NA                182       2352   0.2693541
NA               2267       4619   0.5289739
NA               1404       6023   0.6897618
NA                751       6774   0.7757673
NA                160       6934   0.7940907
NA               1034       7968   0.9125057
NA                764       8732   1.0000000

```r
fatalitiesPlot <- ggplot(fData, aes(y=fatalities, x=EVGROUP)) +
    geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(fatalitiesPlot)
```

![](StormData_files/figure-html/unnamed-chunk-11-1.png) 

