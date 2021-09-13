###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","influenza_data.xlsx")

#load data using readxl

rawdata <- readxl::read_excel(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#dataset is so small, we can print it to the screen.
#that is often not possible.
print(rawdata)

#I don't want the last column named "Current_Through" and "setting", so i am going to delete them.
col_del <- select(rawdata, -Current_Through, -Setting)

#Also, I want to keep the first 43 rows as it keeps the data for 2019-2020. So I am going to delete rows 44-237.
row_del <- col_del %>% slice(-c(44:237))

#After deletin rows and columns, final data will have 8 columns and 43 rows and put in "processeddata"
processeddata <- row_del
View(processeddata)

#The way I put it here, I want to  make plot MMWR_week_order (x-axis) and Cumulative_Doses (on y axis). That will give us a plot how cumulative dose progressed through weeks.
#Also, I want to see the total doses (in y axis) over 2019 and 2020 (in x-axis). For that, we have to sum 1-21 column of Doses for 2019, 22-43 column of doses for 2020.


# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


