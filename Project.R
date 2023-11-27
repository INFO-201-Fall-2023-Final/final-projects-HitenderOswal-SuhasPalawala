library(stringr)
library(dplyr)
library(tidyverse)

CPI <- read.csv("Consumer Price Index 1913-2023 Edited.csv")
HHI <- read.csv("Median household income US and by state 1984-2022 - Edited.csv")
SPH <- read.csv("Sale price of homes by state.csv")
HLEChange <- read.csv("Change-Table 1.csv")

SPH <- SPH[order(SPH$period_begin),]
SPH <- subset(SPH, select = c(period_begin,
                              state,
                              state_code,
                              property_type,
                              median_sale_price))
SPH <- SPH[str_detect(SPH$period_begin,"01-01") & !str_detect(SPH$period_begin,"2023"),]
SPH <- SPH[order(SPH$property_type),]
SPH <- SPH[order(SPH$state_code),]
SPH <- SPH[order(SPH$period_begin),]
SPH <- rename(
  SPH,
  DATE = period_begin,
)

HLEChange <- subset(HLEChange, select = c("State", paste(c("Change.in.Total.Homelessness.."), c(2021:2012), c(".2022"), sep = "")))

HLEChange <- rename(
  HLEChange,
  state_code = State,
)

new_df <- left_join(HLEChange, SPH, by = c("state_code"))

CPI <- subset(CPI, select = c(Year,
                              Jan.CPI))
CPI <- CPI[str_detect(CPI$Year,paste(c(2012:2022), collapse = "|")),]
CPI <- rename(
  CPI,
  DATE = Year,
)

new_df <- left_join(CPI, new_df, by = c("DATE"))

new_df$state.income <- apply(X = new_df[,c(1, 3)], MARGIN = 1, FUN = function(data){
                                 HHI[HHI$DATE == data[1],][[paste(data[2], ".Household.Income", sep = "")]]})

increasedOrDecreased <- function(number) {
  number_value <- as.numeric(sub("%","",number))
  if(number_value > 0) {
    return("increased")
  } else if(number_value < 0) {
    return("decreased")
  } else {
    return("stayed the same")
  }
}

new_df <- mutate(
  new_df,
  homelessness_final_change = mapply(increasedOrDecreased,new_df$Change.in.Total.Homelessness..2012.2022)
)

newPrice <- function(oldCost, oldCPI, newCPI) {
  return(round(oldCost * (newCPI / oldCPI)))
}

new_df <- mutate(
  new_df,
  inflation_adjusted_price = mapply(newPrice, new_df$median_sale_price, new_df$Jan.CPI, 281.148)
)

new_df <- mutate(
  new_df,
  numeric_homelessness_change = as.numeric(sub("%","",new_df$Change.in.Total.Homelessness..2012.2022))
)

summarized_data <- t(sapply(new_df[, c("numeric_homelessness_change","median_sale_price","state.income","inflation_adjusted_price")], summary))

new_df <- select(new_df, -numeric_homelessness_change)

#View(summarized_data)

write.csv(new_df, "Final_Data.csv", row.names =TRUE)
