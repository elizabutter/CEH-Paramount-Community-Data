print("Inititalizing the Data Import Handler")
library(tidyverse)
library(googlesheets4)

#Google Sheet settings
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

#Import data from gGogle sheet
communityMonitorResultsKey <- ("1T17psaWZYbCdPdK8tZZ5TDJoOUY0DYffKv5O154YVGU")
rawCommMonitorData <- NULL
rawCommMonitorData <- read_sheet(ss = communityMonitorResultsKey, sheet = 2, 
                                 na = c("DL", "<DL", "--", "NA", 
                                        "ND","NS", "NSD", "INV", "NULL", NULL))



# if (is.null(rawCommMonitorData)){
#   print("The Google Sheet Failed to Load")
#   preloadedCommMonitorData <- readr::read_csv("Output Data/preloadedData_Popups.csv")
#   print("Loading Preloaded Data")
#   dir.create(file.path("Output Data"), showWarnings = FALSE)
#   write.csv(preloadedCommMonitorData, file = "Output Data/cleanData_Popups.csv")
#   print("Finished writing preloaded data")
#   break
# } else {
#   print("The Google Sheet Has Loaded")
# }

#remove all the days where there was no data recorded for BOTH air monitors
#keep only the columns we want (the ones with data)
cleanCommMonitorData <-rawCommMonitorData%>%
  filter(!is.na(rawCommMonitorData[2]) | !is.na(rawCommMonitorData[3]) ) %>%
  select(`Sample Date`, `Vermont Ave. and 70th St. (PQ100)`, `Somerset Blvd and Colorado Ave (OMNI)`)
cleanCommMonitorData

#Add in some extra data for testing purposes
# cleanCommMonitorData <- cleanCommMonitorData %>%
#   mutate(testData1 = cleanCommMonitorData$`Vermont Ave. and 70th St. (PQ100)` + 1)%>%
#   mutate(testData2 = cleanCommMonitorData$`Somerset Blvd and Colorado Ave (OMNI)` + 1)


#move all the air monitor observations into a single column

meltedCommMonitorData <- as.tibble(melt(cleanCommMonitorData, id.var='Sample Date')) %>%
  filter(!is.na(value))

#Calculate how far above the threshold value each observation is. 
#`calculated` is how many times above the threshold each value is.
calcCommMonitorData<- meltedCommMonitorData %>%
  mutate(calculated = meltedCommMonitorData$value / 0.01)


popupCommMonitorData <- calcCommMonitorData %>%
  #Generate all the text for the graph hover popup
  mutate(popUp = paste0("<b>Air Monitor:</b> ", calcCommMonitorData[[2]], "</br></br>", 
                        "<b>Date:</b> ", as.Date(calcCommMonitorData[[1]]),
                        "<b>   Observation:</b> ", calcCommMonitorData[[3]], " ng/m3", 
                        '</br>',
                        '</br>',
                        "<b>This reading is ", calcCommMonitorData[[4]],
                        " times higher </br>than the EPA Risk Threshold.</b>")
  )

# #create EPA Threshold data using the first and last dates in the dataset
# 
# epaPopupMonitorData <- popupCommMonitorData %>%
#   add_row("Sample Date" = min(popupCommMonitorData$`Sample Date`), variable = "EPA Risk Threshold", value = 0.01, popUp = "This is the EPA Risk Threshold.") %>%
#   add_row("Sample Date" = max(popupCommMonitorData$`Sample Date`), variable = "EPA Risk Threshold", value = 0.01, popUp = "This is the EPA Risk Threshold.")
# 






#Create a directory for the import data to be saved in
dir.create(file.path("Output Data"), showWarnings = FALSE)

write.csv(popupCommMonitorData, file = "Output Data/fullData.csv", row.names = FALSE)


summaryData <- meltedCommMonitorData

summaryData$year = lubridate::year(summaryData$`Sample Date`)
summaryData$month = lubridate::month(summaryData$`Sample Date`)


monthlyAveragedSummaryData <- summaryData %>% 
  group_by(month, year, variable) %>%
  summarise(value = round(mean(value), 2))%>%
  ungroup() %>%
  mutate(Dates = as.Date(paste0(month, "-15-", year), format = "%m-%d-%Y")) %>%
  select(-`year`, -`month`) %>%
  mutate(calculated = value / 0.01) %>%
  select(Dates, variable, value, calculated) %>%
  arrange(Dates)




monthlyPopupData <- monthlyAveragedSummaryData %>%
  #Generate all the text for the graph hover popup
  mutate(popUp = paste0("Air Monitor: ", monthlyAveragedSummaryData[[2]], "</br></br>", 
                        "The average reading during ", format(as.Date(monthlyAveragedSummaryData[[1]]), '%B, %Y'),
                        " was: ", monthlyAveragedSummaryData[[3]], " g/m3", 
                        '</br>',
                        "<b>This monthly average is approximately ", monthlyAveragedSummaryData[[4]],
                        " times higher </br>than the EPA Risk Threshold.</b>")
  )

write.csv(monthlyPopupData, file = "Output Data/summaryData.csv", row.names = FALSE)



tooltipText <- tibble(text = c(paste("This is number the average air monitor reading for the selected dates.",
                                     "<br>",
                                     "Use the Date Selection slider below the graph to see how the readings have changed over time."),
                               paste('The "EPA" is the United States Environmental Protection Agecy. The EPA uses the best scientific information to ensure that we have clean air, land and water. ')))

tooltipText[[1]][2]



print("Finished with the Data Import Handler")


file.exists('ParamountNewApp006/dela1.jpg')


