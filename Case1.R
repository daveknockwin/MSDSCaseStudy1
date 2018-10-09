install.packages("ggplot2")

#import breweries file
Breweries <- read.csv("Breweries.csv")
#inspecting the dataset
str(Breweries)
#sort the data by state
library(dplyr)
breweriesOrderedByState <- arrange(Breweries, desc(Breweries$State))
# use table function to get a count of breweries
breweriesbystate <- table(Breweries$State)
# change table to dataframe
breweriesbystate <- as.data.frame(breweriesbystate)
str(breweriesbystate)
# assign names to columns
names(breweriesbystate) <- c("State","Total Breweries")
print(breweriesbystate)

#load the beers file
Beers <- read.csv("Beers.csv")
# inspect dataset
str(Beers)
names(Breweries) 
names(Beers) 
#rename columns
names(Breweries) <- c("Brewery_id","Brewery_Name", "City", "State" )
names(Beers) <- c("Beer_Name","Beer_ID","ABV","IBU","Brewery_id","Style","Ounces") 
#common variable data is Brewery_id
breweriesandbeers <- merge(Breweries,Beers, by=c("Brewery_id"), all=TRUE)
str(breweriesandbeers) # 2410 10
head(breweriesandbeers)
tail(breweriesandbeers)
write.csv(breweriesandbeers,file ="breweriesandbeers.csv", row.names = FALSE, quote=FALSE)

#variable NA_count will represent the number of NA's in each column
NA_count <- colSums(is.na(breweriesandbeers))
NA_count

#Lets move NAs from the ABV column and then check max ABV using function order (we want to see the whole line which contains this element)
#Variable ABV_NOna will represent data set without NAs elements in the column ABV and ABV_NOna_ordered will represent ordered data set by column ABV without NAs elements. 
ABV_NOna <- subset(x=breweriesandbeers, !is.na(breweriesandbeers$ABV))
ABV_NOna_ordered <- ABV_NOna[order(ABV_NOna$ABV),]
tail(ABV_NOna_ordered,1)
#Lets move NAs from the IBU column and then check max IBU using function order (we want to see the whole line which contains this element)
#Variable IBU_NOna will represent data set without NAs elements in the column IBU
IBU_NOna <- subset(x=breweriesandbeers, !is.na(breweriesandbeers$IBU))
IBU_NOna_ordered <- IBU_NOna[order(IBU_NOna$IBU),]
tail(IBU_NOna_ordered,1)
#In this way we gave the answer on the question 5 and still keeping all NAs in final dataset "breweriesandbeers" in case we will need them.

library(ggplot2)
med.abv <- tapply(ABV_NOna$ABV, ABV_NOna$State, median)
med.abv
med.ibu <- tapply(IBU_NOna$IBU, IBU_NOna$State, median)
med.ibu

median_abv <- as.data.frame.table(med.abv)
median_ibu <- as.data.frame.table(med.ibu)
colnames(median_abv)[1] <- "States"
colnames(median_ibu)[1] <- "States"

data <- merge(median_abv, median_ibu, by="States", all = TRUE)
colnames(data)[3] <- "IBU"
colnames(data)[2] <- "ABV"

mytheme <- theme(plot.title = element_text(face = "bold.italic",
                                           size= 14, color = "black"), axis.text.x = element_text(size = 5, face ="bold.italic"), axis.text.y = element_text(face = "bold.italic"))
ggplot(data = data, aes(x = States, y = IBU, fill = ABV)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of IBU & ABV by State") +
  mytheme

summary(breweriesandbeers$ABV)

#create a subset for the variables ABV and IBU as they are our variables fo interest
bitter_vs_alc <- subset(Beers, subset = is.na(IBU) == FALSE, select = c(ABV, IBU))

head(bitter_vs_alc)

#Alcoholic Content vs Bitterness
ggplot(data=bitter_vs_alc, aes(x=ABV, y=IBU)) +
  geom_point(pch = 20, color = "brown", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Alcoholic Content vs Bitterness",
       x="Alcohol by Volume", y="International Bitterness Units")

cor.test(breweriesandbeers$ABV,breweriesandbeers$IBU)


