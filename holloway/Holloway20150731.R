Holloway paper 
=============================================  

# Needed packages
install.packages("choroplethrMaps")
install.packages("rmarkdown")
install.packages("choroplethr")
install.packages("reshape")
install.packages("reshape2")
install.packages("plyr")
install.packages("sm")
install.packages("foreign")
install.packages("car")
install.packages("gplots")

# Running packages
library("rmarkdown")
library("choroplethrMaps")
library("choroplethr")
library("reshape")
library("reshape2")
library("plyr")
library("sm")
library("gplots")
library("foreign")
library("car")

# Get data
dataset=read.csv("dataset.csv")

#convert to numeric
dataset$arrival=as.numeric(as.character(dataset$arrival))
dataset$hollow=as.numeric(as.character(dataset$hollow))
dataset$aruba=as.numeric(as.character(dataset$aruba))
dataset$distance=as.numeric(as.character(dataset$distance))
dataset$nights=as.numeric(as.character(dataset$nights))
dataset$X1.time=as.numeric(as.character(dataset$X1.time))
dataset$X2.to.5.visits=as.numeric(as.character(dataset$X2.to.5.visits))
dataset$X6.or.more.visits=as.numeric(as.character(dataset$X6.or.more.visits))
dataset$Repeaters=as.numeric(as.character(dataset$Repeaters))

# reshape dataset to match choropleth
melt(dataset, id=c("nr","year","month","date","statenum","state"), measured=c("arrival","hollow","aruba","nights","distance","X1.time","X2.to.5.visits","X6.or.more.visits","Repeaters","check","check5","check10"), na.rm=TRUE)

#mean total arrival
summary_total=ddply(dataset,c("state"), summarise, arrival=mean(arrival))

#mean pre holloway
dataset_pre=subset(dataset, pre==1)
summary_pre=ddply(dataset_pre,c("state"),summarise, arrival=mean(arrival))

#mean post holloway
dataset_post=subset(dataset, pre==0)
summary_post=ddply(dataset_post,c("state"),summarise, arrival=mean(arrival))

#mean total arrival first timers
summary_total_first=ddply(dataset,c("state"), summarise, arrival=mean(na.omit(X1.time)))

#mean pre holloway first timers
dataset_pre_first=subset(dataset, pre==1)
summary_pre_first=ddply(dataset_pre,c("state"),summarise, arrival=mean(na.omit(X1.time)))

#mean post holloway first timers
dataset_post_first=subset(dataset, pre==0)
summary_post_first=ddply(dataset_post,c("state"),summarise, arrival=mean(na.omit(X1.time)))

#mean total arrival 2-5 timers
summary_total_two=ddply(dataset,c("state"), summarise, arrival=mean(na.omit(X2.to.5.visits)))

#mean pre holloway 2-5 timers
dataset_pre_two=subset(dataset, pre==1)
summary_pre_two=ddply(dataset_pre,c("state"),summarise, arrival=mean(na.omit(X2.to.5.visits)))

#mean post holloway 2-5 timers
dataset_post_two=subset(dataset, pre==0)
summary_post_two=ddply(dataset_post,c("state"),summarise, arrival=mean(na.omit(X2.to.5.visits)))

#mean total arrival 6 timers
summary_total_six=ddply(dataset,c("state"), summarise, arrival=mean(na.omit(X6.or.more.visits)))

#mean pre holloway 6 timers
dataset_pre_six=subset(dataset, pre==1)
summary_pre_six=ddply(dataset_pre,c("state"),summarise, arrival=mean(na.omit(X6.or.more.visits)))

#mean post holloway 6 timers
dataset_post_six=subset(dataset, pre==0)
summary_post_six=ddply(dataset_post,c("state"),summarise, arrival=mean(na.omit(X6.or.more.visits)))



#rename variable for chorpleth to read
names(summary_total)[1]="region"
names(summary_total)[2]="value"
names(summary_pre)[1]="region"
names(summary_pre)[2]="value"
names(summary_post)[1]="region"
names(summary_post)[2]="value"

names(summary_total_first)[1]="region"
names(summary_total_first)[2]="value"
names(summary_pre_first)[1]="region"
names(summary_pre_first)[2]="value"
names(summary_post_first)[1]="region"
names(summary_post_first)[2]="value"

names(summary_total_two)[1]="region"
names(summary_total_two)[2]="value"
names(summary_pre_two)[1]="region"
names(summary_pre_two)[2]="value"
names(summary_post_two)[1]="region"
names(summary_post_two)[2]="value"

names(summary_total_six)[1]="region"
names(summary_total_six)[2]="value"
names(summary_pre_six)[1]="region"
names(summary_pre_six)[2]="value"
names(summary_post_six)[1]="region"
names(summary_post_six)[2]="value"

#mean pre holloway repeaters
summary_pre_repeat=ddply(dataset_post,c("state"),summarise, arrival=summary_pre_two$value+summary_pre_six$value)
#mean post holloway repeaters
summary_post_repeat=ddply(dataset_post,c("state"),summarise, arrival=summary_post_two$value+summary_post_six$value)


#difference calculation {gemiddeld bezoek 1st time(periode NA) minus gemiddeld bezoek 1st time (periode VOOR)} – {gemiddeld bezoek> 1st time ( periode NA)  minus gemiddeld bezoek>1st time (periode VOOR)}
difference=data.frame((summary_post_first$value-summary_pre_first$value)-((summary_post_two$value+summary_post_six$value)-(summary_pre_two$value+summary_pre_six$value)))
difference=data.frame(difference,summary_total$region)

names(difference)[1]="value"
names(difference)[2]="region"


#Descriptive statistics
plotmeans(X1.time ~ year, main="Mean first visit to Aruba", data=dataset) # plotmeans draw a 95% confidence interval around the means
plotmeans(X2.to.5.visits ~ year, main="2-5 prior visits to Aruba", data=dataset) # plotmeans draw a 95% confidence interval around the means
plotmeans(X6.or.more.visits ~ year, main="Mean 6 or more prior visits to Aruba", data=dataset) # plotmeans draw a 95% confidence interval around the means



#choropleth
state_choropleth_total=state_choropleth(summary_total, title="Average monthly arrivals to Aruba (2004-2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_total
state_choropleth_pre=state_choropleth(summary_pre, title="Average monthly arrivals to Aruba prior to Holloway incident (January 2004-May 2005)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_pre
state_choropleth_post=state_choropleth(summary_post, title="Average monthly arrivals to Aruba post Holloway incident (May 2005-December 2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_post

state_choropleth_total_first=state_choropleth(summary_total_first, title="Average monthly first time arrivals to Aruba (2004-2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_total_first
state_choropleth_pre_first=state_choropleth(summary_pre_first, title="Average monthly first time arrivals to Aruba prior to Holloway incident (January 2004-May 2005)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_pre_first
state_choropleth_post_first=state_choropleth(summary_post_first, title="Average monthly arrivals to Aruba post Holloway incident (May 2005-December 2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_post_first

state_choropleth_total_two=state_choropleth(summary_total_two, title="Average monthly arrivals 2-5 time visitors to Aruba (2004-2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_total_two
state_choropleth_pre_two=state_choropleth(summary_pre_two, title="Average monthly arrivals 2-5 time visitors to Aruba prior to Holloway incident (January 2004-May 2005)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_pre_two
state_choropleth_post_two=state_choropleth(summary_post_two, title="Average monthly arrivals 2-5 time visitors to Aruba post Holloway incident (May 2005-December 2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_post_two

state_choropleth_total_six=state_choropleth(summary_total_six, title="Average monthly arrivals 6 or more visits to Aruba (2004-2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_total_six
state_choropleth_pre_six=state_choropleth(summary_pre_six, title="Average monthly arrivals 6 or more visits to Aruba prior to Holloway incident (January 2004-May 2005)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_pre_six
state_choropleth_post_six=state_choropleth(summary_post_six, title="Average monthly arrivals 6 or more visits to Aruba post Holloway incident (May 2005-December 2008)", legend="Average number of visitors per state",num_colors=9)
state_choropleth_post_six

#difference choropleth
state_choropleth_post_difference=state_choropleth(difference, title="Difference calculation visitors to Aruba,pre and post Holloway incident, by number of visits (May 2005-December 2008)", legend="Difference",num_colors=9)
state_choropleth_post_difference

#binding repeat data for Kernel
summary_pre_repeat$prepost="pre"
summary_post_repeat$prepost="post"
repeat_kernel <- rbind(summary_pre_repeat, summary_post_repeat)

#create value labels
arrival.f <-factor(repeat_kernel$prepost, levels=c("pre","post"))
labels=c("repeaters pre Holloway", "repeaters post Holloway")

# plot densities 
sm.density.compare(repeat_kernel$arrival, arrival.f, xlab="average arrivals")
title(main="Kernel distribution repeat visitors pre and post Holloway")

# add legend via mouse click
colfill<-c(2:(2+length(levels(arrival.f)))) 
legend(locator(1), levels(arrival.f), fill=colfill)

#binding first timers data for Kernel
summary_pre_first$prepost="pre"
summary_post_first$prepost="post"
first_kernel <- rbind(summary_pre_first, summary_post_first)

#create value labels
arrival_first.f <-factor(first_kernel$prepost, levels=c("pre","post"))
labels=c("first pre Holloway", "first post Holloway")

# plot densities 
sm.density.compare(first_kernel$value, arrival_first.f, xlab="average arrivals")
title(main="Kernel distribution first time visitors pre and post Holloway")

# add legend via mouse click
colfill<-c(2:(2+length(levels(arrival_first.f)))) 
legend(locator(1), levels(arrival_first.f), fill=colfill)

