---
title: "US_NOAA_Storm_Analysis"
author: "Jordan Woloschuk"
date: "7/11/2019"
output: 
        html_document:
                keep_md: true
---





# US NOAA Storm Database Analysis on the Cost and Magnitude of Recent Storms
### Reporducible Research: Peer Assessment #2

## Assignment Overview

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

## 1) Synopsos

TK


The US National Oceanic and Atmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States between 1950 and 2011. This database includes when and where these weather events occur, as well as estimates of any fatalities, injuries, and property damage. In this analysis we seek to describe the type of major storm or weather event that have caused the greatest human health and economic cost in the United States. For the analysis, we have focused our attention weather events after January 1, 2000 given the greater completeness of the dataset.

Our overall hypothesis is that XXXXXX 

From the NOAA storm dataset, we found that XXXX 


## 2) Data Processing

### 2.1) Loading Libraries

Load necessary libraries for data analysis and developing results


```r
library(ggplot2)
library(dplyr)
# R.utils for bunzip2. It is faster to unzip the .csv.bz2 first instead of directly
# using read.csv(). This was tested by using system.time() for both methods.
library(R.utils) 


# Want to remove scientific notation
options(scipen=999)
```

### 2.2) Loading the Data


```r
# Using Cache = TRUE due to time it takes to read the CSV file

# Set working directory, save the zip file URL, the zip file name and CSV name

setwd("~/GitHub/Reporducible_Reseearch-Course_Project_2")

zipURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

rawzipfile <- "repdata_data_StormData.csv.bz2"

csvfile <- "repdata_data_StormData.csv"

# Checks if the CSV file exists. If not, checks to see if the
# zip file is in the directory, if not, then downloads the zip file. The zip file
# is then unzipped

if (!file.exists(csvfile)) {
        
        if (!file.exists(rawzipfile)) {
                download.file(zipURL, rawzipfile, method = "curl")
                bunzip2(rawzipfile, csvfile, remove = FALSE, overwrite = TRUE)
          }
  }

raw_data <- read.csv(csvfile)
```

### 2.2) Modifying the Data and select timeframe

Only want to focus on a select number of key columns for this analysis:   
    1. BGN_DATE: Begining Date  
    2. STATE: State  
    3. EVTYPE: Weather event type  
    4. FATALITIES: Number of fatalitities  
    5. INJURIES: Number of injuries  
    6. PROPDMG: Property damage  
    7. PROPDMGEXP: Property damage exponent  
    8. CROPDMG: Crop damage  
    9. CROPDMGEXP Crop damage exponent  




```r
selected_vars <- c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES", "INJURIES",
                   "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm_data <- raw_data[,selected_vars]

# Convert to date class
storm_data$BGN_DATE <- as.Date(storm_data$BGN_DATE, "%m/%d/%Y")


# Calculate the number of weather events before 2000/01/01
before_2000 <- sum(storm_data$BGN_DATE < as.Date("2000-01-01"))

after_2000 <- sum(storm_data$BGN_DATE >= as.Date("2000-01-01"))

before_2000_percent <- paste(round(100 * before_2000 / nrow(storm_data), 2), "%", sep = " ") 

# Calculate the Min and Max date, and the number of years between them and 2000/01/01

min_date <- min(storm_data$BGN_DATE)
max_date <- max(storm_data$BGN_DATE)

after_date <- max_date - as.Date("2000-01-01")
before_date <- as.Date("2000-01-01") - min_date


# Create new storm data subset 
storm_subset <- subset(storm_data, BGN_DATE > as.Date("2000-01-01"))
```

We will also limit the analysis for storms that have occured since only 379134 instances occur before January 1, 2000.  
This represents only 42.02 % of the total dataset occuring over a period of 18260 days.   
In contrast, 523163 weather instance occur after January 1, 2000 over a period of 4351 days.    

However, if we examine the storm EVTYPE dataset, we can see that there are a total of 985 unique weather related classifications. The following 


```r
head(storm_data$EVTYPE)
```

```
## [1] TORNADO TORNADO TORNADO TORNADO TORNADO TORNADO
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

### 2.3) Develop new weather event classification groupings

A large number of these weather events are related and could be consolidated for improved interpretation of the data. In the following steps we will develop 8 new simplified weather classifications. 



```r
# Create a new column that will have the 8 condensed weather event type classifications
storm_subset$EVGROUP <- NULL

# 1) Lightning Storm and Percipitation
storm_subset[grepl("tstm|thunderstorm|lightning|precipitation|rain|hail|drizzle|sleet|
                 wet|percip|burst|depression|fog|wall cloud|sleet|ABNORMALLY WET|EXTREMELY WET",
                 storm_subset$EVTYPE,ignore.case = TRUE),
             "EVGROUP"] <- "Lightning and Percipitation"
# 2) Ice and Snow
storm_subset[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|
                 blizzard|chill|freezing|avalanche|glaze|sleet|extreme cold
                 |avalanche|BLIZZARD", storm_subset$EVTYPE,ignore.case = TRUE),
           "EVGROUP"] <- "Ice and Snow"

# 3) Flooding
storm_subset[grepl("flood|surf|blow-out|swells|fld|dam break", storm_subset$EVTYPE,
                 ignore.case = TRUE), "EVGROUP"] <- "Flooding"
# 4) Hurricane and Stormy Seas
storm_subset[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning|
                   hurricane|typhoon|HURRICANE|SEICHE", storm_subset$EVTYPE,
                   ignore.case = TRUE),"EVGROUP"] <- "Hurricane and Stormy Seas"

# 5) Landslides
storm_subset[grepl("slide|erosion|slump", storm_subset$EVTYPE, ignore.case = TRUE),
           "EVGROUP"] <- "Landslides"

# 6) Heatwave and Drought
storm_subset[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|
                 record temperature|record high|dust|saharan", storm_subset$EVTYPE,
                 ignore.case = TRUE), "EVGROUP"] <- "Heatwave and Drought"


# 7) Tornado and Wind
storm_subset[grepl("wind|wnd|storm|tornado|spout|funnel|whirlwind", storm_subset$EVTYPE,
                 ignore.case = TRUE), "EVGROUP"] <- "Tornado and Wind"

# 8) Fire and Volcanos
storm_subset[grepl("fire|smoke|volcanic|RED FLAG CRITERIA", storm_subset$EVTYPE,
                 ignore.case = TRUE), "EVGROUP"] <- "Fire and Volcanos"

# Categorize these new 8 groupings as factors
storm_subset$EVGROUP <- as.factor(storm_subset$EVGROUP)

# Remove the remaining 6 NULL observations 
# 5 are classified as "OTHER" weather events
# 1 is classified as "NORTHERN LIGHTS"

storm_subset <- storm_subset[!is.na(storm_subset$EVGROUP),]
```

### 2.4) Calculate absolute property damage figures

Next we need to convert the CROPDMNGEXP and PROPDMGEXP character values into exponential power of 10 values


```r
# Function to convert character values into exponential power of 10 values
Power10 <- function(x){
  if(is.numeric(x)) {x <- x}
  else if(grepl("h", x, ignore.case=TRUE)) {x <- 2} # change h to 2
  else if(grepl("k", x, ignore.case=TRUE)) {x <- 3} # change k to 3
  else if(grepl("m", x, ignore.case=TRUE)) {x <- 6} # change m to 6
  else if(grepl("b", x, ignore.case=TRUE)) {x <- 9} # change b to 9
  else if(x == "" || x == " ") {x <- 0} # change blank to 0
  else {x <- NA} # other symbols like ? are changed to NA
  x
}
```

With this Power10 function, we still need a function to apply this exponential power to the CROPDMG and PROPDMG damage values.


```r
ApplyP10 <- function(num, exp) {
  
  P10 <- Power10(exp)
  
  if(is.numeric(num)){num <- num * (10 ^ P10)}
  if(!is.numeric(num)){num <- 0}
  
  num
  
}
```

Now with the Power10 and the ApplyP10 functions, we can calculate the total property and crop damage


```r
# Property damage calculation
storm_subset$Total_PROPDMG <- mapply(ApplyP10, storm_subset$PROPDMG,storm_subset$PROPDMGEXP)

# Crop damage calculation
storm_subset$Total_CROPDMG <- mapply(ApplyP10, storm_subset$CROPDMG,storm_subset$CROPDMGEXP)

# Total property and crop damage
storm_subset$Total_DMG <- storm_subset$Total_PROPDMG + storm_subset$Total_CROPDMG
```

## 3) Results 



TBD

(States the type of event that causes the greatest harm to human health)
(states the type of even that causes the greatest economic consequences)


