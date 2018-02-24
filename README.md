Storadata.Rmd
---
title: "Storm Data Analysis"
author: "Aysegul Sonmez"
date: "February 23, 2018"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##----------------------------------------------------------------------------------------
##The storm data is loaded into a data frame for analysis.

```{r}
setwd("C:/Users/aysegul/Desktop/coursera/reproducibleresearch/w4")
stormdata  <- read.csv(bzfile("StormData.csv.bz2"),header=TRUE)
```

##Cleaning EVTYPE a quick cleaning is possible with str_trim() and toupper()and year column formatted.  

```{r}
library(plyr)
library(stringr)
stormdata_cleaned <- mutate(stormdata,EVTYPE=toupper(str_trim(EVTYPE)), YEAR=format(strptime(BGN_DATE,format="%m/%d/%Y %T"),format="%Y"))
```

##Question1:Across the United States, which types of events (as indicated in the EVTYPE variable) are mostharmful with respect to population health?  
\newline
\newline

  
```{r}

library(plyr)
evtype_total_casualties <- ddply(stormdata_cleaned,.(EVTYPE),
                                 summarize,
                                 totalFatalities=sum(FATALITIES),
                                 totalInjuries=sum(INJURIES),
                                 totalCasualties=sum(FATALITIES+INJURIES))

print(evtype_total_casualties[1:10,])

casualties_sorted <- evtype_total_casualties[order(evtype_total_casualties[,"totalCasualties"],
                                                   decreasing=TRUE),]
print(casualties_sorted[1:10,])

```


```{r,echo=TRUE}

library(ggplot2)

g <- ggplot(casualties_sorted[1:10,], aes(y=totalCasualties, x=reorder(EVTYPE, -totalCasualties)))

g <- g + geom_bar(fill="red4",stat="identity")
g <- g + ggtitle("Top 10 Events with Highest total fatalities") + labs(x="EVENT TYPE", y="Total fatalities")
g <- g + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
print(g)
```

  
##Question2: Across the United States, which types of events have the greatest economic consequences?  



###TotalCasualities by Evtype and Year  
\newline


```{r}  

evtype_yearly_total_casualties <- ddply(stormdata_cleaned,.(EVTYPE,YEAR),
                           summarize,
                           totalFatalities=sum(FATALITIES),
                           totalInjuries=sum(INJURIES),
                           totalCasualties=sum(FATALITIES+INJURIES))

tornado_casualties <- evtype_yearly_total_casualties[evtype_yearly_total_casualties$EVTYPE == "TORNADO",]


```

```{r}

    evtype_yearly_total_casualties[1:10,]

```

###Damage to Property and Crops Property and crop damage is noted in the data along with a multiplier (K or M or B )  

\newline





```{r} 

stormdata_damages <- mutate(stormdata_cleaned,PropDmg = PROPDMG * ifelse(PROPDMGEXP == "K",1000,ifelse(PROPDMGEXP=="M",1000000,ifelse(PROPDMGEXP=="B",1000000000,1))),CropDmg = CROPDMG * ifelse(CROPDMGEXP == "K",1000,ifelse(CROPDMGEXP=="M",1000000,ifelse(CROPDMGEXP =="B",1000000000,1))))


```


###To determine the most damaging types of events, we sum up property and crop damage.
\newline


```{r} 

evtype_total_damages <- ddply(stormdata_damages,.(EVTYPE),summarize,
                              totalPropDmg=sum(PropDmg),
                              totalCropDmg=sum(CropDmg),
                              totalDmg = sum(PropDmg,CropDmg))
damages_sorted <- evtype_total_damages[order(evtype_total_damages[,"totalDmg"],
                                             decreasing=TRUE),]
print(damages_sorted[1:10,])

```



```{r,echo=TRUE}

library(ggplot2)

g2 <- ggplot(damages_sorted[1:10,], aes(y=totalDmg, x=reorder(EVTYPE, -totalDmg)))
g2 <- g2 + geom_bar(fill="red4",stat="identity")
g2 <- g2 + ggtitle("Top 10 Events with Highest Damages") + labs(x="Event Type", y="Total Damages")
g2 <- g2 + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

print(g2)

```

