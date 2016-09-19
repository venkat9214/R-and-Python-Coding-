## Very good!

#Question #1:
#For the first two questions, we study the data mpg in the ggplot2 package.
#(a) Load ggplot2 package first and then type data(mpg). Quickly go through the dataset
#and the help file.


install.package("dplyr")
library(dplyr)
data(mpg)

#(b) Obtain a subset of data including: year, cyl, cty, hwy, and renames these variables as
#V1, V2, V3, V4.

names(mpg)[4] <- ('V1')
names(mpg)[5] <- ('V2')
names(mpg)[8] <- ('V3')
names(mpg)[9] <- ('V4') ### >>>>>>> You might want to check "select" in dplyr

#(c) In mpg data, obtain the average of city miles per gallon and highway miles per gallon
#for different numbers of cylinders. (Hint: the mean function calculates the average of a
#vector.)

x <- mean(mpg$V3)
y <- mean(mpg$V4)

#(d) For manufacturer, identify the car(s) that have the largest city miles per gallon.




lc1 <- select(mpg, manufacturer, model, V3)
lc2 <- group_by(lc1, manufacturer, model)
lc3 <- filter(group_by(lc2,manufacturer, model), V3 == max(V3))
lc4 <- arrange(lc3, manufacturer, desc(V3))
lc5 <- filter(group_by(lc4,manufacturer), V3 == max(V3))

#Question #2:
#We want to know the relationship between three variables: engine displacement, city
#miles per gallon, and highway miles per gallon.
#1. Create a new variable ratioHVE showing the ratio between highway miles per
#gallon and engine displacement.


HVE <- transmute(mpg, ratioHVE = (mpg$displ / mpg$V4))

#2. Create new variables rationCVE showing the ratio between city miles per gallon
#and engine displacement

CVE <- transmute(mpg, ratioCVE = (mpg$displ / mpg$V3))

#3. Obtain the average ratioHVE and ratioCVE by different years and manufacturers.


MHVE <- merge(mpg, HVE)
MHVE1 <- select(MHVE, manufacturer, V1, ratioHVE)
avgHVE <- summarise(group_by(MHVE1, manufacturer, V1), total = mean(MHVE1$ratioHVE))

MCVE <- merge(mpg,CVE)
MCVE1 <- select(MCVE, manufacturer, V1, ratioCVE)
avgCVE <- summarise(group_by(MCVE1, manufacturer, V1), total = mean(MCVE1$ratioCVE))


#4. Find the biggest ratioHVE by different years and drv.


g <- summarize(MHVE1,MaxHVE=max(ratioHVE))


#Question #3:
#For this question, you are going to explore an online dataset and try to answer the
#questions. You can find the dataset here:
#https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t
#1. What are the mean and median beginning and ending salaries for each agency?
#Note that salaries can be annual, hourly, or daily. You need to convert all of them
#to annual.


a <- filter(jobs, jobs$Salary.Frequency == 'Annual')
a <- select(a, 1:10,12:26)

b <- filter(jobs, jobs$Salary.Frequency == 'Hourly')

c <- filter(jobs, jobs$Salary.Frequency == 'Daily')

d <- mutate(b, Salary.Type1 = 2080 * b$Salary.Range.From, Salary.Type2 = 2080 * b$Salary.Range.To)

e <- mutate(c, Salary.Type1 = 364 * c$Salary.Range.From, Salary.Type2 = 364 * c$Salary.Range.To)

d <- select(d, 1:8, 12:28)
e <- select(e, 1:8, 12:28)

names(d)[24] <- ('Salary.Range.From')
names(d)[25] <- ('Salary.Range.To')

names(e)[24] <- ('Salary.Range.From')
names(e)[25] <- ('Salary.Range.To')

x <- rbind.data.frame(a, d, e)
y <- select(x, 2, 9, 10)

q <- group_by(y, Agency)
w <- summarise(q, AvgStart=mean(Salary.Range.From), AvgEnd=mean(Salary.Range.To), MedianStart=median(Salary.Range.From), MedianEnd=median(Salary.Range.To))


#2. Which agency has the highest average starting salary?

e <- filter(w, AvgStart == max(AvgStart))

#3. Does the type of posting (internal or external) have a big impact on the average
#salary range?

pos <- group_by(x, Posting.Type)
posi <- summarise(pos, AvgStart=mean(Salary.Range.From), AvgEnd=mean(Salary.Range.To), MedianStart=median(Salary.Range.From), MedianEnd=median(Salary.Range.To))

# There is no big impact on average salary range based on the type of posting

#4. Rank the levels according to the average salary range for each level.


nyjobs<-mutate(jobs,range=Salary.Range.To-Salary.Range.From)
level<-group_by(nyjobs,Level)
sal_level<-summarize(level,AvgRange=mean(range))
arrange(sal_level,AvgRange)

#5. Find out the range of money currently being spent by each agency for new hires.
#Note that the X..Of.Positions column shows how many positions are available.

new<-mutate(new,NewHireSpendingRange=X..Of.Positions*range)
p<-group_by(new,Agency)
rom <-summarize(p,TotalNewHireSpendingRange=sum(NewHireSpendingRange))


#6. What civil service title has the largest salary range?

jobs[which(jobs$range==max(jobs$range)),]$Civil.Service.Title

## jobs %>% group_by(Civil.Service,Title) %>%
##              summarise(max = max(range)) %>%
##              arrange(desc(max))
