download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "StromData.csv.bz2")
stromData=read.csv("StromData.csv.bz2")

fatal=aggregate(stromData$FATALITIES~stromData$EVTYPE,activity,sum)
names(fatal) = c('evt','fatalities')
top10fatal = fatal[order(fatal$fatalities,decreasing = TRUE),][1:10,]
top10fatal$evt = factor(top10fatal$evt,levels=top10fatal$evt)

library(ggplot2)
ggplot(top10fatal,aes(x=evt,y=fatalities)) + geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) +
    xlab("Weather Event Type") +
    ylab("Number of fatalities") +
    ggtitle("Number of fatalities by Top 10 Weather Events")


injuries=aggregate(stromData$INJURIES~stromData$EVTYPE,activity,sum)
names(injuries) = c('evt','injuries')
top10injuries = injuries[order(injuries$injuries,decreasing = TRUE),][1:10,]
top10injuries$evt = factor(top10injuries$evt,levels=top10injuries$evt)


ggplot(top10injuries,aes(x=evt,y=injuries)) + geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) +
    xlab("Weather Event Type") +
    ylab("Number of injuries") +
    ggtitle("Number of injuries by Top 10 Weather Events")

stromData$propDamage = ifelse(stromData$PROPDMGEXP=="H",stromData$PROPDMG * 10^2,
                              ifelse(stromData$PROPDMGEXP=="K",stromData$PROPDMG * 10^3,
                                     ifelse(stromData$PROPDMGEXP=="M",stromData$PROPDMG * 10^6,
                                            ifelse(stromData$PROPDMGEXP=="B",stromData$PROPDMG * 10^9,stromData$PROPDMG
                                            )
                                     )
                              )
)
stromData$cropDamage = ifelse(stromData$CROPDMGEXP=="H",stromData$CROPDMG * 10^2,
                              ifelse(stromData$CROPDMGEXP=="K",stromData$CROPDMG * 10^3,
                                     ifelse(stromData$CROPDMGEXP=="M",stromData$CROPDMG * 10^6,
                                            ifelse(stromData$CROPDMGEXP=="B",stromData$CROPDMG * 10^9,stromData$CROPDMG
                                            )
                                     )
                              )
)


damages=aggregate(stromData$propDamage+stromData$cropDamage~stromData$EVTYPE,activity,sum)
top10damages=damages[order(damages$damages,decreasing = TRUE),][1:10,]
top10damages$evt=factor(top10damages$evt,levels = top10damages$evt)
names(top10damages)=c("evt","damages")

ggplot(top10damages,aes(x=evt,y=damages)) + geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) +
    xlab("Weather Event Type") +
    ylab("Damages in US dollars") +
    ggtitle("Property & Crop damages by Top 10 Weather Events")

