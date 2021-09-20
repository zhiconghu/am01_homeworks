library(ggplot2)
library(lubridate)

#Load data and select my forces
load("stopSearch_assignment.rda")
stopSearchmyForces <- stopSearch[which(stopSearch$Force=="gwent"|
                                         stopSearch$Force=="metropolitan"|
                                         stopSearch$Force=="south-yorkshire"),]



#Combing demographic data
stopnsearchdata <- read.csv("stop-and-search-data.csv")
stopnsearchdata <- stopnsearchdata[which(stopnsearchdata$Geography=="Gwent"|
                                           stopnsearchdata$Geography=="Metropolitan Police"|
                                           stopnsearchdata$Geography=="South Yorkshire"),]

#Select time from year 2017 till 2020 (Note that financial years starts at 6th April each year)
unique(stopnsearchdata$Time)
stopnsearchdata <- stopnsearchdata[which(stopnsearchdata$Time=="2017/18"|
                                           stopnsearchdata$Time=="2018/19"|
                                           stopnsearchdata$Time=="2019/20"),]

#Check type of type of classification of ethnic group used
unique(stopSearchmyForces$`Officer-defined ethnicity`)
stopnsearchdata <- stopnsearchdata[which(stopnsearchdata$Ethnicity_type=="ONS 2001 5+1"|
                                           stopnsearchdata$Ethnicity_type=="ONS 2011 5+1"|
                                           stopnsearchdata$Ethnicity_type=="All"),]

#Selected columns that are meaning, the columns with demographic information
stopnsearchdata <- subset(stopnsearchdata, select = c(Time,Ethnicity,Population.by.ethnicity,Geography,
                                                      Rate.per.1.000.population.by.ethnicity,
                                                      Number.of.stop.and.searches))

#Data cleaning on our main data
stopSearchmyForces$`Self-defined ethnicity`[stopSearchmyForces$`Self-defined ethnicity` == ""] <- NA
stopSearchmyForces$`Officer-defined ethnicity`[stopSearchmyForces$`Officer-defined ethnicity` == ""] <- NA
stopSearchmyForces$`Object of search`[stopSearchmyForces$`Object of search` == ""] <- NA
stopSearchmyForces$`Outcome`[stopSearchmyForces$`Outcome` == ""] <- NA
stopSearchmyForces$`Legislation`[stopSearchmyForces$`Legislation` == ""] <- NA

#Split data into three Forces to see if there are significant missing data in any forces
stopSearchgwent <- stopSearchmyForces[which(stopSearchmyForces$Force=="gwent"),]
stopSearchmetropolitan <- stopSearchmyForces[which(stopSearchmyForces$Force=="metropolitan"),]
stopSearchsouthyorkshire <- stopSearchmyForces[which(stopSearchmyForces$Force=="south-yorkshire"),]

sum(is.na(stopSearchgwent$`Officer-defined ethnicity`))
sum(is.na(stopSearchmetropolitan$`Officer-defined ethnicity`))
sum(is.na(stopSearchsouthyorkshire$`Officer-defined ethnicity`))

#Looking into metropolitan
stopSearchmetropolitan <- stopSearchmetropolitan[!is.na(stopSearchmetropolitan$`Officer-defined ethnicity`),]
ggplot(data = stopSearchmetropolitan, aes(x=factor(`Officer-defined ethnicity`), fill=Legislation)) +
  geom_bar(stat = "count") +
  xlab("Officer defined ethnicity")

stopnsearchdata.metropolitan <- stopnsearchdata[which(stopnsearchdata$Geography=="Metropolitan Police"),]
stopnsearchdata.metropolitan <- stopnsearchdata.metropolitan[which(stopnsearchdata.metropolitan$Ethnicity=="Asian"|
                                                        stopnsearchdata.metropolitan$Ethnicity=="Black"|
                                                        stopnsearchdata.metropolitan$Ethnicity=="Other"|
                                                        stopnsearchdata.metropolitan$Ethnicity=="White"),]
stopnsearchdata.metropolitan.1920 <- stopnsearchdata.metropolitan[which(stopnsearchdata.metropolitan$Time=="2019/20"),]
ggplot(data = stopnsearchdata.metropolitan.1920, 
       aes(x=factor(Ethnicity), y=Population.by.ethnicity)) +
  geom_bar(stat = "identity", fill="orange") +
  xlab("Ethnicity")

ggplot(data = stopnsearchdata.metropolitan[which(stopnsearchdata.metropolitan$Time=="2019/20"),], 
       aes(x=factor(Ethnicity), y=Number.of.stop.and.searches)) +
  geom_bar(stat = "identity", fill="orange") +
  xlab("Ethnicity")








#Actual Report
#First change date variable
#Gwent from 2017.10.01 - 2020.09.30
#South-Yorkshire from 2017.10.01 - 2020.09.30
#Metropolitan from 2017.09.30 - 2020.09.30











