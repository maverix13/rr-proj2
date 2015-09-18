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
library(dplyr)
library(ggplot2)
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
g <- ggplot(stormData, aes(YEAR, fill = ..count.., removePanelGrid=TRUE,removePanelBorder=TRUE,)) +
  geom_histogram(binwidth=5) + 
  ylab("Number of Events") +
  scale_fill_gradient("# Events", low = "black", high = "gray") +
  ggtitle("Histogram: Events By Year")
print(g)
```

![](StormData_files/figure-html/unnamed-chunk-4-1.png) 

Hence for this report, data after 1990 is considered for analysis.


```r
stormData <- stormData[stormData$YEAR >= 1990,]
dim(stormData)
```

```
## [1] 751740     38
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

