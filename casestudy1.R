beers = read.csv(file="~/Downloads/CaseStudy_2_2_2_2/Beers.csv", header = TRUE)
head(beers)
str(beers)

breweries = read.csv(file="~/Downloads/CaseStudy_2_2_2_2/Breweries.csv", header = TRUE)
head(breweries)
str(breweries)

#How many breweries are present in each state?

library(data.table) ## >= v1.9.6
setDT(breweries)[, .(count = uniqueN(Brewery_id)), by = State]


#Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.

#need to make sure the brewery ID has the same name for merge function to work
names(breweries)[1] = "Brewery_id"
names(breweries)
head(breweries)
str(breweries)

#merge dataframes by brewery id
beer_brews = merge(beers, breweries, by = "Brewery_id", all = FALSE)

#print last 6 and first 6
head(beer_brews, 6)
tail(beer_brews, 6)

#Report the number of NA's in each column.

colSums(is.na(beer_brews))

#Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
#need to ignore nas for median

beer_brews_na =na.omit(beer_brews)

df1 = setDT(beer_brews_na)[,list(Mean=mean(ABV), Max=max(ABV), Min=min(ABV), Median=as.numeric(median(ABV)), Std=sd(ABV)), by=State]

#check
AK = beer_brews_na[beer_brews_na$State == " AK",]
median(AK$ABV)

df = setDT(beer_brews_na)[,list(Mean=mean(IBU), Max=max(IBU), Min=min(IBU), Median=as.numeric(median(IBU)), Std=sd(IBU)), by=State]


library(ggplot2)
ggplot(data=df, aes(x=State, y=Median, fill=State)) +
  geom_bar(stat="identity", show.legend=FALSE) + labs(x = "State", y = "Median of IBU", 
                                                      title = "Median IBU per state") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=df1, aes(x=State, y=Median, fill=State)) +
  geom_bar(stat="identity", show.legend=FALSE) + labs(x = "State", y = "Median of ABV", 
                                                      title = "Median ABV per state") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
beer_brews_na[beer_brews_na$IBU == max(beer_brews_na$IBU),]
#Oregon!

beer_brews_na[beer_brews_na$ABV == max(beer_brews_na$ABV),]
#kentuckyy

#Summary statistics for the ABV variable
setDT(beer_brews_na)[,list(Mean=mean(ABV), Max=max(ABV), Min=min(ABV), Median=as.numeric(median(ABV)), Std=sd(ABV))]

#check
mean(beer_brews_na$ABV)

#Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.

ggplot(beer_brews_na, aes(x=ABV, y=IBU)) + 
  labs(x = "ABV", y = "IBU", title = "Scatterplot of bitterness versus alcoholic content") + 
  geom_point(shape=1) +    
  geom_smooth(method=lm)

