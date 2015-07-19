

## load Packages
library(FredR)
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(RColorBrewer)


## access FRED database with API Key
api.key = "7de318a50907a5759b42a84831e657bf"
fred <- FredR(api.key)

## create a data table of FRED codes and their corresponding series name
fred.ref <- data.table(FRED_code = c("FYGDP", "FYGFD", "FYGFDPUN", "FYFSD", 
        "FYFR", "FYONET", "FYOINT"), 
        series_name = c("Gross Domestic Product", "Gross Federal Debt", 
        "Federal Debt Held by the Public", "Federal Surplus or Deficit", 
        "Federal Receipts", "Federal Net Outlays", "Federal Outlays - Interest"))
       

## loop through the vector of market codes and
## pass each list item to a function to pull the
## data series corresponding to the code from the
## FRED database and create a list of tables for each code 
data.list <- lapply(fred.ref$FRED_code, function (x) { 
                        fred$series.observations(series_id = x)
                })

## loop through the list of tables and select only the 
## date and value columns
## mutate date into a date class and value into a numeric class
## mutiply value by 1 billion if the the value is GDP
## mutiply value by 1 million otherwise
data.list2 <- lapply(seq_along(data.list), function(x) {
                        data.list[[x]] %>% 
                        select(date, value) %>% 
                        mutate(date = as.Date(date), value = 
                                ifelse (x == 1, 
                                value * 1000000000,
                                value * 1000000)) 
        })

## name each table in the list to its corresponding FRED Code
names(data.list) <- fred.ref$FRED_code

## rename the value column in each table in the list 
## to its corresponding FRED Code
lapply(seq_along(data.list), function(x) {
                setnames(data.list[[x]], "value", 
                        names(data.list[x])) 
        })

## merge all the table in the list into one big wide table
## with many many columns 
data.wide <- Reduce(function(x,y) {
                merge(x,y, by = "date", all = TRUE)
        } , data.list)

## gather the various value columns into one dimension
## transforms the wide table into a tall skinny table
data.skinny <- data.wide  %>% 
        gather(FRED_code, value, -date)

## identify complete cases (no NAs) and load into binary vector
good <- complete.cases(data.skinny)


## merge the skinny data set with the market references
## to get the market codes and states, and using only complete cases
data.budget <- merge(data.skinny[good], fred.ref, 
        by = "FRED_code", all = TRUE)


## check to see if director already exists and if not then create directory
## write the data table to a .csv 
if(!file.exists("./FRED")){dir.create("./FRED")}
write.table(data.budget, 
        "./FRED/Federal_Budget_Data.csv", sep=",", row.names = FALSE)
