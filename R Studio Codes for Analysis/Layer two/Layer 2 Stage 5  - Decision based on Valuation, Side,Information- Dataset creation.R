#Split by Side, Valuation and Game
#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#__________________________________________________________________________________________________________________________________________________________________________________


pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Plots_SPlit_useless.pdf")

#Adding Gain of Profit as a Column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))


#Histogram for Gain of Profit

hist(Data$GOP)


############################################################################################

###Information Value Split###


BB_Treat <- c("BB", "BBMoreS","BBLargeCE", "BBLimS", "BBMMK", "BBMoreB", "BBMoreRounds", "BBRandom")
BB <- filter(Data, treatment %in% BB_Treat)

FULL_OB_Treat <- c("Full","FullFromBtoS", "FullFromStoB", "FullLArgeCE", "FullLimS", "FUllMoreB", "FullMoreS", "FullSShift")
FULL_OB <- filter(Data, treatment %in% FULL_OB_Treat)

Other_treat <- c("Other")
Other_Side_Info <- filter(Data, treatment %in% Other_treat)

Same_treat <- c("Same","SameMMK")
Same_Side_Info <- filter(Data, treatment %in% Same_treat)


##########################

#FUNKTION 1#________________________________________________________________________________________________________________________________________________________________________

categorize_and_filter <- function(Data) {
  
  
  par(mfrow = c(1,2))
  hist(Data$valuation[Data$side == "Seller"], prob = T, xlab = "Valuation", main = "Seller")
  hist(Data$valuation[Data$side == "Buyer"], prob = T, xlab = "Valuation", main = "Buyer")
  median_seller <- median(Data$valuation[Data$side == "Seller"])
  median_buyer <- median(Data$valuation[Data$side == "Buyer"])
  
  
  Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Seller") & (Data$valuation < median_seller), 1,2))
  
  Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Buyer") & (Data$valuation < median_buyer), 3, Data$Valuation_groups))
  Data$Valuation_groups <- ifelse((Data$side == "Buyer") & (Data$valuation > median_buyer), 4, Data$Valuation_groups)
  
  
  
  
  
  ##############################################################################################
  
  
  #Data Groups matched into Categories ; Category 4 is the optimal
  #since Valuations work the opposite way for seller and buyer, we match the opposite groups into 1 Category 
  
  Data <- Data %>%
    mutate(Valuation_categories = case_when(
      side == "Seller" & Valuation_groups == 1 ~ 2, # Optimal Valuation Category
      side == "Buyer" & Valuation_groups == 4 ~ 4,  # Optimal Valuation Category
      side == "Seller" & Valuation_groups == 2 ~ 1, # Worst Valuation Category
      side == "Buyer" & Valuation_groups == 3 ~ 3,  # Worst Valuation Category
      
      TRUE ~ NA_real_ 
    ))
  
  
  
  
  Data_Buyer_Good <- filter(Data, Valuation_categories == 4)
  Data_Buyer_bad <- filter(Data, Valuation_categories == 3)
  Data_Seller_Good <- filter(Data, Valuation_categories == 2)
  Data_Seller_bad <- filter(Data, Valuation_categories == 1)
  
  
  
  
  
  #Section: Behavioral Variables
  
  #__________________________________________________________________________________________________________________________________________________________________________________
  
  
  
  #Patience__________________________________________________________________________________________________________________________________________________________________________
  
  #We are seeking to observe if the time when the first order is entered, influences gain of profit 
  #First a help column looking for the first order of every round
  
  Time_of_order_Seller_bad <- Data_Seller_bad %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
  Data_Seller_bad <- Data_Seller_bad %>% left_join(Time_of_order_Seller_bad, by = c("id", "round"))
  
  
  Time_of_order_Seller_Good <- Data_Seller_Good %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
  Data_Seller_Good <- Data_Seller_Good %>% left_join(Time_of_order_Seller_Good, by = c("id", "round"))
  
  
  Time_of_order_Buyer_bad <- Data_Buyer_bad %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
  Data_Buyer_bad <- Data_Buyer_bad %>% left_join(Time_of_order_Buyer_bad, by = c("id", "round"))
  
  
  Time_of_orderBuyer_Good <- Data_Buyer_Good %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
  Data_Buyer_Good <- Data_Buyer_Good %>% left_join(Time_of_orderBuyer_Good, by = c("id", "round"))
  
  #only for done deals
  
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))
  
  
  
  
  
  
  
  #Then the median of this time in the specific groups
  
  Median_Seller_bad_Patience <- median(Data_Seller_bad$Time_of_order, na.rm = TRUE)
  Median_Seller_Good_Patience <- median(Data_Seller_Good$Time_of_order, na.rm = TRUE)
  Median_Buyer_bad_Patience <- median(Data_Buyer_bad$Time_of_order, na.rm = TRUE)
  MedianBuyer_Good_Patience <- median(Data_Buyer_Good$Time_of_order, na.rm = TRUE)
  
  
  #Binary variable
  
  
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Seller_bad_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Seller_bad_Patience, 0, NA)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Seller_Good_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Seller_Good_Patience, 0, NA)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Buyer_bad_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Buyer_bad_Patience, 0, NA)))
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > MedianBuyer_Good_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= MedianBuyer_Good_Patience, 0, NA)))
  
  
  
  #Aggression________________________________________________________________________________________________________________________________________________________________________
  
  
  
  #Aggression defined by the absolute value of bid - valuation in realtion to the valuation
  
  #Creating the "First Bid" column
  
  
  first_bid_Seller_bad <- Data_Seller_bad %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
  Data_Seller_bad <- Data_Seller_bad %>% left_join(first_bid_Seller_bad, by = c("id", "round"))
  
  
  first_bid_Seller_Good <- Data_Seller_Good %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
  Data_Seller_Good <- Data_Seller_Good %>% left_join(first_bid_Seller_Good, by = c("id", "round"))
  
  first_bid_Buyer_bad <- Data_Buyer_bad %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
  Data_Buyer_bad <- Data_Buyer_bad %>% left_join(first_bid_Buyer_bad, by = c("id", "round"))
  
  first_bid_Buyer_Good <- Data_Buyer_Good %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
  Data_Buyer_Good <- Data_Buyer_Good %>% left_join(first_bid_Buyer_Good, by = c("id", "round"))
  
  #only for success
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))
  
  
  #Calculating the Aggression percentage value
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
  
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
  
  #Medians
  
  
  Median_Aggression_Seller_bad <- median(Data_Seller_bad$Percentage_Aggression, na.rm = TRUE)
  Median_Aggression_Seller_Good <- median(Data_Seller_Good$Percentage_Aggression, na.rm = TRUE)
  
  Median_Aggression_Buyer_bad <- median(Data_Buyer_bad$Percentage_Aggression, na.rm = TRUE)
  Median_Aggression_Buyer_Good <- median(Data_Buyer_Good$Percentage_Aggression, na.rm = TRUE)
  
  
  #From an older version but useful for analysis
  Match_price <- Data_Seller_bad %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
  Data_Seller_bad<- Data_Seller_bad %>% left_join(Match_price, by = c("id", "round"))
  
  Match_price <- Data_Seller_Good %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
  Data_Seller_Good<- Data_Seller_Good %>% left_join(Match_price, by = c("id", "round"))
  
  Match_price <- Data_Buyer_bad %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
  Data_Buyer_bad<- Data_Buyer_bad %>% left_join(Match_price, by = c("id", "round"))
  
  Match_price <- Data_Buyer_Good %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
  Data_Buyer_Good<- Data_Buyer_Good %>% left_join(Match_price, by = c("id", "round"))
  
  
  
  #Creation of the final binary variable "Aggression" 
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Seller_bad, 1, ifelse(Percentage_Aggression <= Median_Aggression_Seller_bad, 0, NA)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Seller_Good, 1, ifelse(Percentage_Aggression <= Median_Aggression_Seller_Good, 0, NA)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Buyer_bad, 1, ifelse(Percentage_Aggression <= Median_Aggression_Buyer_bad, 0, NA)))
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Buyer_Good, 1, ifelse(Percentage_Aggression <= Median_Aggression_Buyer_Good, 0, NA)))
  
  
  
  
  
  #Gain of Profit__________________________________________________________________________________________________________________________________________________________________________
  
  
  #Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions
  
  Median_Buyer_bad_GOP <- median(Data_Buyer_bad$GOP, na.rm = TRUE)
  Median_Buyer_Good_GOP <- median(Data_Buyer_Good$GOP, na.rm = TRUE)
  Median_Seller_bad_GOP <- median(Data_Seller_bad$GOP, na.rm = TRUE)
  Median_Seller_Good_GOP <- median(Data_Seller_Good$GOP, na.rm = TRUE)
  
  
  
  #Now the Binary Variable
  
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Buyer_bad_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Buyer_bad_GOP, 0, NA)))
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Buyer_Good_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Buyer_Good_GOP, 0, NA)))
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Seller_bad_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Seller_bad_GOP, 0, NA)))
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Seller_Good_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Seller_Good_GOP, 0, NA)))
  
  
  
  
  #Strategies________________________________________________________________________________________________________________________________________________________________________
  
  
  #Quick and Aggressive; Patience = 0; Aggression = 1
  
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))
  
  
  
  #Slow and Aggressive; Patience = 1; Aggression = 1
  
  
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))
  
  
  #Slow and Soft; Patience = 1; Aggression = 0
  
  
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))
  
  
  #Quick and Soft; Patience = 0; Aggression = 0
  
  
  Data_Buyer_Good <- Data_Buyer_Good %>%
    mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))
  
  Data_Seller_Good <- Data_Seller_Good %>%
    mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))
  
  Data_Seller_bad <- Data_Seller_bad %>%
    mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))
  
  Data_Buyer_bad <- Data_Buyer_bad %>%
    mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))
  
  
  
  
  
  
  ################################Section :Clean Up ###########################################
  

  
  
  
  
  #Hide helping columns
  Data_Seller_bad <- Data_Seller_bad %>% select(-Valuation_groups)
  Data_Seller_Good <- Data_Seller_Good %>% select(-Valuation_groups)
  Data_Buyer_bad <- Data_Buyer_bad %>% select(-Valuation_groups)
  Data_Buyer_Good <- Data_Buyer_Good %>% select(-Valuation_groups)
  
  
  
  
  
  
  # Return a list of data frames
  return(list(
    Data_Buyer_bad = Data_Buyer_bad,
    Data_Buyer_Good = Data_Buyer_Good,
    Data_Seller_Good = Data_Seller_Good,
    Data_Seller_bad = Data_Seller_bad
    
    
  ))
}


process_datasets <- function(...) {
  datasets <- list(...)
  result_list <- mapply(categorize_and_filter, datasets, SIMPLIFY = FALSE)
  names(result_list) <- c("BB", "FULL_OB", "Same", "Other")
  return(result_list)
}


result <- process_datasets(BB, FULL_OB, Same_Side_Info, Other_Side_Info)

BB_results <- result$BB
FULL_OB_results <- result$FULL_OB
Same_results <- result$Same
Other_results <- result$Other

# Accessing specific categories within a dataset
Data_Buyer_bad_BB <- BB_results$Data_Buyer_bad
Data_Buyer_Good_BB <- BB_results$Data_Buyer_Good
Data_Seller_Good_BB <- BB_results$Data_Seller_Good
Data_Seller_bad_BB <- BB_results$Data_Seller_bad


Data_Seller_bad_Full_OB <- FULL_OB_results$Data_Seller_bad
Data_Seller_Good_Full_OB <- FULL_OB_results$Data_Seller_Good
Data_Buyer_Good_Full_OB <- FULL_OB_results$Data_Buyer_Good
Data_Buyer_bad_Full_OB <- FULL_OB_results$Data_Buyer_bad


Data_Buyer_bad_Same <- Same_results$Data_Buyer_bad
Data_Buyer_Good_Same <- Same_results$Data_Buyer_Good
Data_Seller_Good_Same <- Same_results$Data_Seller_Good
Data_Seller_bad_Same <- Same_results$Data_Seller_bad


Data_Buyer_bad_Other <- Other_results$Data_Buyer_bad
Data_Buyer_Good_Other <- Other_results$Data_Buyer_Good
Data_Seller_Good_Other <- Other_results$Data_Seller_Good
Data_Seller_bad_Other <- Other_results$Data_Seller_bad




#Create a list

Datasets_for_analysis <- list(Data_Seller_bad_BB,Data_Seller_Good_BB,Data_Buyer_Good_BB,Data_Buyer_bad_BB,
                              Data_Seller_bad_Full_OB,Data_Seller_Good_Full_OB,Data_Buyer_Good_Full_OB,Data_Buyer_bad_Full_OB,
                              Data_Seller_bad_Other,Data_Seller_Good_Other,Data_Buyer_Good_Other,Data_Buyer_bad_Other,
                              Data_Seller_bad_Same,Data_Seller_Good_Same,Data_Buyer_Good_Same,Data_Buyer_bad_Same
)

dev.off

