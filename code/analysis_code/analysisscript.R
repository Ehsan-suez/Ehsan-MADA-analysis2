###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(tidyverse)
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

# Create a plot that shows number of doses per week
plot1 <-mydata %>% ggplot(aes(x=MMWR_Week_Order,y=Doses))+
                   geom_line()+
                   labs(x="Week Number", y="Doses", title= "Flu Shot Doses")
#look at figure
plot(plot1)

# Create a plot that shows number of cumulative doses each week
plot2 <- mydata %>% ggplot(aes(x=MMWR_Week,y=Cumulative_Doses))+
                    geom_line()+
                    labs(x="Week Number", y="Cumulative Doses", title= "Cumulative Flu Shot Doses")

#look at figure
plot(plot2)

#save figure
figure_file1 = here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot=plot1)  

#save figure
figure_file2 = here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot=plot2) 
######################################
#Data fitting/statistical analysis
######################################

# fit linear model
lmfit <- lm(MMWR_Week ~ Doses, mydata)  
lmfitcumulative <- lm(MMWR_Week ~ Cumulative_Doses, mydata)


# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit)
lmCumulative <- broom::tidy(lmfitcumulative)

#look at fit results
print(lmtable)
print(lmCumulative)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

lmSummary_file = here("results", "resulttable1.rds")
saveRDS(lmCumulative, file = lmSummary_file)
