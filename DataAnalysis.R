# import the libraries that we will use 
# ggplot for drawing pictures plyr for dealing with datas and grid for ranging figures
install.packages("ggplot2")
install.packages("gridExtra")

library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

# set workspace -- write your own here
setwd("E:\\2018-2019_1\\R\\ComptRendu")

# open files of csv -> dataframe
Berkeley_2013 <- read.csv(file = "berkeley-2013.csv",header = TRUE,sep = ',')
Berkeley_2014 <- read.csv(file = "berkeley-2014.csv",header = TRUE,sep = ',')
Berkeley_2015 <- read.csv(file = "berkeley-2015.csv",header = TRUE,sep = ',')
Berkeley_2016 <- read.csv(file = "berkeley-2016.csv",header = TRUE,sep = ',')
Berkeley_2017 <- read.csv(file = "berkeley-2017.csv",header = TRUE,sep = ',')

# a little discription of dataframes
head(Berkeley_2013)
head(Berkeley_2014)
head(Berkeley_2015)
head(Berkeley_2016)
head(Berkeley_2017)

# have a look at how many people for each job
# these two functions do the same things
table(Berkeley_2017$Job.Title)
count(Berkeley_2017,vars="Job.Title")

# ----------------------------------------------------------

# First figure: General overview
# show the average salary of each job for these five yeas

# use ddply to regroup datas
# we will present it in k$
average_2013 <- ddply(Berkeley_2013,.(Job.Title),summarise,average_salary=mean(Total.Pay)/1000)
average_2014 <- ddply(Berkeley_2014,.(Job.Title),summarise,average_salary=mean(Total.Pay)/1000)
average_2015 <- ddply(Berkeley_2015,.(Job.Title),summarise,average_salary=mean(Total.Pay)/1000)
average_2016 <- ddply(Berkeley_2016,.(Job.Title),summarise,average_salary=mean(Total.Pay)/1000)
average_2017 <- ddply(Berkeley_2017,.(Job.Title),summarise,average_salary=mean(Total.Pay)/1000)

# draw the five figures for these five years
g1_13 <- ggplot(average_2013,aes(Job.Title,average_salary))+geom_point()+ggtitle("Average Total Salary per Job in 2013")+labs(x="Jobs", y="Average Salary (k$)")+theme(axis.text.x = element_blank())
g1_14 <- ggplot(average_2014,aes(Job.Title,average_salary))+geom_point()+ggtitle("Average Total Salary per Job in 2014")+labs(x="Jobs", y="Average Salary (k$)")+theme(axis.text.x = element_blank())
g1_15 <- ggplot(average_2015,aes(Job.Title,average_salary))+geom_point()+ggtitle("Average Total Salary per Job in 2015")+labs(x="Jobs", y="Average Salary (k$)")+theme(axis.text.x = element_blank())
g1_16 <- ggplot(average_2016,aes(Job.Title,average_salary))+geom_point()+ggtitle("Average Total Salary per Job in 2016")+labs(x="Jobs", y="Average Salary (k$)")+theme(axis.text.x = element_blank())
g1_17 <- ggplot(average_2017,aes(Job.Title,average_salary))+geom_point()+ggtitle("Average Total Salary per Job in 2017")+labs(x="Jobs", y="Average Salary (k$)")+theme(axis.text.x = element_blank())

# ... plot a line for mean.
#to know the average salary per job. We can see how many jobs are under and below
g1_13 <- g1_13 + geom_hline(yintercept=mean(average_2013$average_salary), color='steelblue')
g1_14 <- g1_14 + geom_hline(yintercept=mean(average_2014$average_salary),color='steelblue')
g1_15 <- g1_15 + geom_hline(yintercept=mean(average_2015$average_salary), color='steelblue')
g1_16 <- g1_16 + geom_hline(yintercept=mean(average_2016$average_salary),color='steelblue')
g1_17 <- g1_17 + geom_hline(yintercept=mean(average_2017$average_salary),color='steelblue')

# draw them in the same figure to compare more clearly
grid.arrange(g1_13,g1_14,g1_15,g1_16,g1_17,nrow=2)

# ----------------------------------------------------------
# Second figure
# point picture is so large, we want to range them and compare diffrent ranges for each year
# with this figure of different years, we can analyse the economie trend clearly

# In this dataframe, some job have only one observation, they are not credible
# From now on, we will only focus on the cols who have more than 10 observations

# inner function to calculate observation numbers, average total pay and average of overtime pay 
cal_number_average <- function(df){
  return (data.frame(number = nrow(df),average_total=mean(df$Total.Pay),average_overtime=mean(df$Overtime.Pay)))
}

# for each df, we group them by jobs by using the function above
average_2013_with_number <- ddply(Berkeley_2013,.(Job.Title),cal_number_average)
average_2014_with_number <- ddply(Berkeley_2014,.(Job.Title),cal_number_average)
average_2015_with_number <- ddply(Berkeley_2015,.(Job.Title),cal_number_average)
average_2016_with_number <- ddply(Berkeley_2016,.(Job.Title),cal_number_average)
average_2017_with_number <- ddply(Berkeley_2017,.(Job.Title),cal_number_average)

# select credible datas who has more than 10 observations
average_2013_credible <- subset(average_2013_with_number, number > 10)
average_2014_credible <- subset(average_2014_with_number, number > 10)
average_2015_credible <- subset(average_2015_with_number, number > 10)
average_2016_credible <- subset(average_2016_with_number, number > 10)
average_2017_credible <- subset(average_2017_with_number, number > 10)

# Plot
g2_13 <- ggplot(data = average_2013_credible,aes(x = average_total/1000,colour=year)) + geom_histogram(color = "white",fill = "deepskyblue",binwidth = 10) + ggtitle("Job count per salary in 2013") + labs(x="Average annual salary (k$)", y="Number of jobs")
g2_14 <- ggplot(data = average_2014_credible,aes(x = average_total/1000,colour=year))+geom_histogram(color = "white",fill = "deepskyblue1",binwidth = 10)+ ggtitle("Job count per salary in 2014") + labs(x="Average annual salary (k$)", y="Number of jobs")
g2_15 <- ggplot(data = average_2015_credible,aes(x = average_total/1000,colour=year))+geom_histogram(color = "white",fill = "deepskyblue2",binwidth = 10)+ ggtitle("Job count per salary in 2015") + labs(x="Average annual salary (k$)", y="Number of jobs")
g2_16 <- ggplot(data = average_2016_credible,aes(x = average_total/1000,colour=year))+geom_histogram(color = "white",fill = "deepskyblue3",binwidth = 10)+ ggtitle("Job count per salary in 2016") + labs(x="Average annual salary (k$)", y="Number of jobs")
g2_17 <- ggplot(data = average_2017_credible,aes(x = average_total/1000,colour=year))+geom_histogram(color = "white",fill = "deepskyblue4",binwidth = 10)+ ggtitle("Job count per salary in 2017") + labs(x="Average annual salary (k$)", y="Number of jobs")
grid.arrange(g2_13,g2_14,g2_15,g2_16,g2_17,nrow=2)
average_2013_credible

# ----------------------------------------------------------
# Third figure
# select jobs with a salary under the poverty threshold in 2017
low_2017 <- subset(average_2017_credible, average_total < 24000)
ggplot(data = low_2017,aes(x=Job.Title, y=number, fill=number))+geom_histogram(stat = "identity")+labs(title="Low Pay Jobs in 2017", x="Jobs Title", y="Number of employees")+theme(axis.text.x = element_text(angle = 65, hjust = 1))

# ----------------------------------------------------------
# Fourth figure
# Now we gonna analyse the overtime ratio to have a general look at different jobs

# connect these 5 dataframes and then plot it
# because we want to have a look of all the datas in one figure
# for each dataframe we add a colone to mark his year
average_2013_credible$year <- 2013
average_2014_credible$year <- 2014
average_2015_credible$year <- 2015
average_2016_credible$year <- 2016
average_2017_credible$year <- 2017

# add these five dataframes together
average_total_credible <- rbind(average_2013_credible,average_2014_credible,average_2015_credible,average_2016_credible,average_2017_credible)

# calculate the overtime ratio for each job
average_total_credible$overtime_ratio <- (average_total_credible$average_overtime)/(average_total_credible$average_total)

# select the ratio who is higher than 10%
average_total_credible_overtime <- subset(average_total_credible,overtime_ratio>0.1)

# Here we get a dataframe who contains all the credible datas
average_total_credible_overtime

# draw figure highest overtime pay-ratio jobs
ggplot(average_total_credible_overtime)+geom_point(aes(x=Job.Title,y=overtime_ratio,color=year))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(y="Overtime pay ratio (%)", x="Jobs", title = "Highest overtime pay-ratio jobs")


# ----------------------------------------------------------
# Fifth figure
# Here we continue analysing the jobs with high over-time ratio

# First chose five jobs we want to analyse for this figure
job1 <- subset(average_total_credible,Job.Title=="POLICE OFFICER")
job2 <- subset(average_total_credible,Job.Title=="COMMUNITY SERVICE OFFICER")
job3 <- subset(average_total_credible,Job.Title=="SOLID WASTE WORKER")
job4 <- subset(average_total_credible,Job.Title=="FIRE APPARATUS OPERATOR")
job5 <- subset(average_total_credible,Job.Title=="FIREFIGHTER")

# combien these five dataframes
jobs <- rbind(job1,job2,job3,job4,job5)

# plot
ggplot(jobs)+geom_line(aes(x=year,y=average_total,color=Job.Title))+labs(y="Average Total Pay (k$)", x = "Year", title = "Salaries evolution in various jobs")+ scale_colour_hue(name="Job Title", l=40)
