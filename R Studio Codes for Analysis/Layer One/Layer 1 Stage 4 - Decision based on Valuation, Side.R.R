#As these Codes have been created before the naming of the Categories (Later named as Groups) are as follows:

#C1 == Seller Bad
#C2 == Seller Medium Low
#C3 == Seller Medium High
#C4 == Seller Good
#C5 == Buyer Bad
#C6 == Buyer Medium Low
#C7 == Buyer Medium High
#C8 == Buyer Good


#Reading the Data set

Data <- read.csv("path")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#creation of a PDF, containing the generated plots

pdf("path")

#Adding Gain of Profit as a Column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

#Histogram for Gain of Profit

hist(Data$GOP)


#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


#Splitting up the Data set
#Creating the helping column Valuation Groups
#Valuation grouping ; Quantiles for each side later to be matched in the according categories (The naming was changed later on in the thesis; "Valuation Categories" is calles "Valuation groups")



par(mfrow = c(1,2))
hist(Data$valuation[Data$side == "Seller"], prob = T, xlab = "Valuation", main = "Seller")
hist(Data$valuation[Data$side == "Buyer"], prob = T, xlab = "Valuation", main = "Buyer")
quantiles_seller <- quantile(Data$valuation[Data$side == "Seller"])
quantiles_buyer <- quantile(Data$valuation[Data$side == "Buyer"])


Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Seller") & (Data$valuation < quantiles_seller[4]), 3, 4))
Data$Valuation_groups <- ifelse((Data$side == "Seller") & (Data$valuation < quantiles_seller[3]), 2, Data$Valuation_groups)
Data$Valuation_groups <- ifelse((Data$side == "Seller") & (Data$valuation < quantiles_seller[2]), 1, Data$Valuation_groups)

Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Buyer") & (Data$valuation < quantiles_buyer[4]), 3, Data$Valuation_groups))
Data$Valuation_groups <- ifelse((Data$side == "Buyer") & (Data$valuation < quantiles_buyer[3]), 2, Data$Valuation_groups)
Data$Valuation_groups <- ifelse((Data$side == "Buyer") & (Data$valuation < quantiles_buyer[2]), 1, Data$Valuation_groups)



#Data Groups matched into Categories ; Category 4 is the optimal
#since Valuations work the opposite way for seller and buyer, we match the opposite groups into 1 Category 

Data <- Data %>%
  mutate(Valuation_categories = case_when(
    side == "Seller" & Valuation_groups == 1 ~ 4, # Optimal Valuation Category
    side == "Buyer" & Valuation_groups == 4 ~ 8,  # Optimal Valuation Category
    side == "Seller" & Valuation_groups == 2 ~ 3, 
    side == "Buyer" & Valuation_groups == 3 ~ 7,
    side == "Seller" & Valuation_groups == 3 ~ 2,
    side == "Buyer" & Valuation_groups == 2 ~ 6,
    side == "Seller" & Valuation_groups == 4 ~ 1, # Worst Valuation Category
    side == "Buyer" & Valuation_groups == 1 ~ 5,  # Worst Valuation Category
    TRUE ~ NA_real_ 
  ))


#seller

Data_C4 <- filter(Data, Valuation_categories == 4)
Data_C3 <- filter(Data, Valuation_categories == 3)
Data_C2 <- filter(Data, Valuation_categories == 2)
Data_C1 <- filter(Data, Valuation_categories == 1)

#Buyer

Data_C8 <- filter(Data, Valuation_categories == 8)
Data_C7 <- filter(Data, Valuation_categories == 7)
Data_C6 <- filter(Data, Valuation_categories == 6)
Data_C5 <- filter(Data, Valuation_categories == 5)

#############################################################################################

###############################Section: Behavioral Variables################################# 

#############################################################################################





#Section: Behavioral Variables

#__________________________________________________________________________________________________________________________________________________________________________________



#Variable Number 1

#Activeness(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping column Amount of bids:

#It shows the Amount Of Bids till a succesfull trade
#amount of bids per round for a specifc ID

#Seller

Bids_Amount_C1 <- Data_C1 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C1 <- Data_C1 %>% left_join(Bids_Amount_C1, by = c("id", "round"))

Bids_Amount_C2 <- Data_C2 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C2 <- Data_C2 %>% left_join(Bids_Amount_C2, by = c("id", "round"))

Bids_Amount_C3 <- Data_C3 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C3 <- Data_C3 %>% left_join(Bids_Amount_C3, by = c("id", "round"))

Bids_Amount_C4 <- Data_C4 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C4 <- Data_C4 %>% left_join(Bids_Amount_C4, by = c("id", "round"))

#Buyer


Bids_Amount_C5 <- Data_C5 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C5 <- Data_C5 %>% left_join(Bids_Amount_C5, by = c("id", "round"))

Bids_Amount_C6 <- Data_C6 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C6 <- Data_C6 %>% left_join(Bids_Amount_C6, by = c("id", "round"))

Bids_Amount_C7 <- Data_C7 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C7 <- Data_C7 %>% left_join(Bids_Amount_C7, by = c("id", "round"))

Bids_Amount_C8 <- Data_C8 %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_C8 <- Data_C8 %>% left_join(Bids_Amount_C8, by = c("id", "round"))

#only showing for the successful bid so it doesn't influence the histogram

#Seller

Data_C1 <- Data_C1 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))

Data_C2 <- Data_C2 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))

Data_C3 <- Data_C3 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))

Data_C4 <- Data_C4 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))


#Buyer


Data_C5 <- Data_C5 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))
Data_C6 <- Data_C6 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))
Data_C7 <- Data_C7 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))
Data_C8 <- Data_C8 %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))



#Histogram for the amount of bids of each Valuation Category it 

par(mfrow = c(2, 2))
hist(Data_C1$Amount_of_Bids, main = "Data C1", xlab = "Amount of Bids")
hist(Data_C2$Amount_of_Bids, main = "Data C2", xlab = "Amount of Bids")
hist(Data_C3$Amount_of_Bids, main = "Data C3", xlab = "Amount of Bids")
hist(Data_C4$Amount_of_Bids, main = "Data C4", xlab = "Amount of Bids")
hist(Data_C5$Amount_of_Bids, main = "Data C5", xlab = "Amount of Bids")
hist(Data_C6$Amount_of_Bids, main = "Data C6", xlab = "Amount of Bids")
hist(Data_C7$Amount_of_Bids, main = "Data C7", xlab = "Amount of Bids")
hist(Data_C8$Amount_of_Bids, main = "Data C8", xlab = "Amount of Bids")



median_C1 <- median(Data_C1$Amount_of_Bids, na.rm = TRUE)
median_C2 <- median(Data_C2$Amount_of_Bids, na.rm = TRUE)
median_C3 <- median(Data_C3$Amount_of_Bids, na.rm = TRUE)
median_C4 <- median(Data_C4$Amount_of_Bids, na.rm = TRUE)
median_C5 <- median(Data_C5$Amount_of_Bids, na.rm = TRUE)
median_C6 <- median(Data_C6$Amount_of_Bids, na.rm = TRUE)
median_C7 <- median(Data_C7$Amount_of_Bids, na.rm = TRUE)
median_C8 <- median(Data_C8$Amount_of_Bids, na.rm = TRUE)


#Now i am creating the binary variable calles Activeness which determines if a Player is overaverage active or not

#Activeness

#Seller

Data_C1 <- Data_C1 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C1, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C1, 0, NA)))

hist(Data_C1$Activeness, main = "Data_C1", xlab = "Activeness")


Data_C2 <- Data_C2 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C2, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C2, 0, NA)))

hist(Data_C1$Activeness, main ="Data_C2", xlab = "Activeness")


Data_C3 <- Data_C3 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C3, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C3, 0, NA)))

hist(Data_C3$Activeness, main ="Data_C3", xlab = "Activeness")


Data_C4 <- Data_C4 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C4, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C4, 0, NA)))

hist(Data_C4$Activeness, main ="Data_C4", xlab = "Activeness")


#Buyer


Data_C5 <- Data_C5 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C5, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C5, 0, NA)))

hist(Data_C5$Activeness, main ="Data_C5", xlab = "Activeness")



Data_C6 <- Data_C6 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C6, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C6, 0, NA)))

hist(Data_C6$Activeness, main ="Data_C6", xlab = "Activeness")



Data_C7 <- Data_C7 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C7, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C7, 0, NA)))

hist(Data_C7$Activeness, main ="Data_C7", xlab = "Activeness")



Data_C8 <- Data_C8 %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_C8, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_C8, 0, NA)))

hist(Data_C4$Activeness, main ="Data_C8", xlab = "Activeness")





#Flexebility(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creatin the helping Columns: Min_Bid, Max_Bid and Bid_Spread -  these show the lowest/highest Bid of an Id in a specific round and the spread between it
#We are trying to determine how much the player fluctuates with his offers.

#seller


Bid_Spread_C1 <- Data_C1 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C1 <- Data_C1 %>% left_join(Bid_Spread_C1, by = c("id", "round"))

Bid_Spread_C2 <- Data_C2 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C2 <- Data_C2 %>% left_join(Bid_Spread_C2, by = c("id", "round"))

Bid_Spread_C3 <- Data_C3 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C3 <- Data_C3 %>% left_join(Bid_Spread_C3, by = c("id", "round"))

Bid_Spread_C4 <- Data_C4 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C4 <- Data_C4 %>% left_join(Bid_Spread_C4, by = c("id", "round"))

#Buyer

Bid_Spread_C5 <- Data_C5 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C5 <- Data_C5 %>% left_join(Bid_Spread_C5, by = c("id", "round"))

Bid_Spread_C6 <- Data_C6 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C6 <- Data_C6 %>% left_join(Bid_Spread_C6, by = c("id", "round"))

Bid_Spread_C7 <- Data_C7 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C7 <- Data_C7 %>% left_join(Bid_Spread_C7, by = c("id", "round"))

Bid_Spread_C8 <- Data_C8 %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_C8 <- Data_C8 %>% left_join(Bid_Spread_C8, by = c("id", "round"))



#And again only for successful trades

#seller

Data_C1 <- Data_C1 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C2 <- Data_C2 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C3 <- Data_C3 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C4 <- Data_C4 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

#buyer

Data_C5 <- Data_C5 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C6 <- Data_C6 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C7 <- Data_C7 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_C8 <- Data_C8 %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

#Histogram for the bid spread of each Valuation Category it 



par(mfrow = c(2, 2))
hist(Data_C1$Bid_Spread, main = "Data C1", xlab = "Bid Spread")
hist(Data_C2$Bid_Spread, main = "Data C2", xlab = "Bid Spread")
hist(Data_C3$Bid_Spread, main = "Data C3", xlab = "Bid Spread")
hist(Data_C4$Bid_Spread, main = "Data C4", xlab = "Bid Spread")


par(mfrow = c(2, 2))
hist(Data_C5$Bid_Spread, main = "Data C5", xlab = "Bid Spread")
hist(Data_C6$Bid_Spread, main = "Data C6", xlab = "Bid Spread")
hist(Data_C7$Bid_Spread, main = "Data C7", xlab = "Bid Spread")
hist(Data_C8$Bid_Spread, main = "Data C8", xlab = "Bid Spread")

#Median of the spread so we can create a binary variable 

median_C1_Spread <- median(Data_C1$Bid_Spread, na.rm = TRUE)
median_C2_Spread <- median(Data_C2$Bid_Spread, na.rm = TRUE)
median_C3_Spread <- median(Data_C3$Bid_Spread, na.rm = TRUE)
median_C4_Spread <- median(Data_C4$Bid_Spread, na.rm = TRUE)

median_C5_Spread <- median(Data_C5$Bid_Spread, na.rm = TRUE)
median_C6_Spread <- median(Data_C6$Bid_Spread, na.rm = TRUE)
median_C7_Spread <- median(Data_C7$Bid_Spread, na.rm = TRUE)
median_C8_Spread <- median(Data_C8$Bid_Spread, na.rm = TRUE)


#Creating the binary variable called Flexibility 

#Flexibility (Not used for final Thesis)______________________________________________________________________________________________________________________________________________________________________________________

#Seller

Data_C1 <- Data_C1 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C1_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C1_Spread, 0, NA)))

Data_C2 <- Data_C2 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C2_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C2_Spread, 0, NA)))

Data_C3 <- Data_C3 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C3_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C3_Spread, 0, NA)))

Data_C4 <- Data_C4 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C4_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C4_Spread, 0, NA)))

#Buyer

Data_C5 <- Data_C5 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C5_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C5_Spread, 0, NA)))

Data_C6 <- Data_C6 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C6_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C6_Spread, 0, NA)))

Data_C7 <- Data_C7 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C7_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C7_Spread, 0, NA)))

Data_C8 <- Data_C8 %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_C8_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_C8_Spread, 0, NA)))

#Histogram

#Seller

hist(Data_C1$Flexible, main ="Data_C1", xlab = "Flexebile")
hist(Data_C2$Flexible, main ="Data_C2", xlab = "Flexebile")
hist(Data_C3$Flexible, main ="Data_C3", xlab = "Flexebile")
hist(Data_C4$Flexible, main ="Data_C4", xlab = "Flexebile")

#Buyer

hist(Data_C5$Flexible, main ="Data_C5", xlab = "Flexebile")
hist(Data_C6$Flexible, main ="Data_C6", xlab = "Flexebile")
hist(Data_C7$Flexible, main ="Data_C7", xlab = "Flexebile")
hist(Data_C8$Flexible, main ="Data_C8", xlab = "Flexebile")














#############################################################################################Done bis do








#Patience__________________________________________________________________________________________________________________________________________________________________________

#We are seeking to observe if the time when the first order is entered, influences gain of profit 
#First a help column looking for the first order of every round

#Seller

Time_of_order_C1 <- Data_C1 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C1 <- Data_C1 %>% left_join(Time_of_order_C1, by = c("id", "round"))


Time_of_order_C2 <- Data_C2 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C2 <- Data_C2 %>% left_join(Time_of_order_C2, by = c("id", "round"))


Time_of_order_C3 <- Data_C3 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C3 <- Data_C3 %>% left_join(Time_of_order_C3, by = c("id", "round"))


Time_of_order_C4 <- Data_C4 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C4 <- Data_C4 %>% left_join(Time_of_order_C4, by = c("id", "round"))

#Buyer

Time_of_order_C5 <- Data_C5 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C5 <- Data_C5 %>% left_join(Time_of_order_C5, by = c("id", "round"))


Time_of_order_C6 <- Data_C6 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C6 <- Data_C6 %>% left_join(Time_of_order_C6, by = c("id", "round"))


Time_of_order_C7 <- Data_C7 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C7 <- Data_C7 %>% left_join(Time_of_order_C7, by = c("id", "round"))


Time_of_order_C8<- Data_C8 %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_C8 <- Data_C8 %>% left_join(Time_of_order_C8, by = c("id", "round"))


##only for done deals

#seller

Data_C1 <- Data_C1 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C2 <- Data_C2 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C3 <- Data_C3 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C4 <- Data_C4 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

#Buyer

Data_C5 <- Data_C5 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C6 <- Data_C6 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C7 <- Data_C7 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_C8 <- Data_C8 %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))


#Then the median of said Time

Median_C1_Patience <- median(Data_C1$Time_of_order, na.rm = TRUE)
Median_C2_Patience <- median(Data_C2$Time_of_order, na.rm = TRUE)
Median_C3_Patience <- median(Data_C3$Time_of_order, na.rm = TRUE)
Median_C4_Patience <- median(Data_C4$Time_of_order, na.rm = TRUE)

Median_C5_Patience <- median(Data_C5$Time_of_order, na.rm = TRUE)
Median_C6_Patience <- median(Data_C6$Time_of_order, na.rm = TRUE)
Median_C7_Patience <- median(Data_C7$Time_of_order, na.rm = TRUE)
Median_C8_Patience <- median(Data_C8$Time_of_order, na.rm = TRUE)

#Binary variable

#Seller

Data_C1 <- Data_C1 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C1_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C1_Patience, 0, NA)))

Data_C2 <- Data_C2 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C2_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C2_Patience, 0, NA)))

Data_C3 <- Data_C3 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C3_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C3_Patience, 0, NA)))

Data_C4 <- Data_C4 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C4_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C4_Patience, 0, NA)))

#Buyer

Data_C5 <- Data_C5 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C5_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C5_Patience, 0, NA)))

Data_C6 <- Data_C6 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C6_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C6_Patience, 0, NA)))

Data_C7 <- Data_C7 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C7_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C7_Patience, 0, NA)))

Data_C8 <- Data_C8 %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_C8_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_C8_Patience, 0, NA)))


#Aggression________________________________________________________________________________________________________________________________________________________________________



#Aggression defined by the absolute value of bid - valuation in realtion to the valuation

#Creating the "First Bid" column


#seller


first_bid_C1 <- Data_C1 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C1 <- Data_C1 %>% left_join(first_bid_C1, by = c("id", "round"))


first_bid_C2 <- Data_C2 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C2 <- Data_C2 %>% left_join(first_bid_C2, by = c("id", "round"))

first_bid_C3 <- Data_C3 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C3 <- Data_C3 %>% left_join(first_bid_C3, by = c("id", "round"))

first_bid_C4 <- Data_C4 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C4 <- Data_C4 %>% left_join(first_bid_C4, by = c("id", "round"))

#buyer

first_bid_C5 <- Data_C5 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C5 <- Data_C5 %>% left_join(first_bid_C5, by = c("id", "round"))

first_bid_C6 <- Data_C6 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C6 <- Data_C6 %>% left_join(first_bid_C6, by = c("id", "round"))

first_bid_C7 <- Data_C7 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C7 <- Data_C7 %>% left_join(first_bid_C7, by = c("id", "round"))

first_bid_C8 <- Data_C8 %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_C8 <- Data_C8 %>% left_join(first_bid_C8, by = c("id", "round"))



#seller

Data_C1 <- Data_C1 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C2 <- Data_C2 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C3 <- Data_C3 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C4 <- Data_C4 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

#buyer

Data_C5 <- Data_C5 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C6 <- Data_C6 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C7 <- Data_C7 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_C8 <- Data_C8 %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))



#Look for the first bid

Data_C1 <- Data_C1 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

Data_C2 <- Data_C2 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
Data_C3 <- Data_C3 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
Data_C4 <- Data_C4 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))


Data_C5 <- Data_C5 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
Data_C6 <- Data_C6 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
Data_C7 <- Data_C7 %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))
Data_C8 <- Data_C8%>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))


#Medians
Median_Aggression_C1 <- median(Data_C1$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C2 <- median(Data_C2$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C3 <- median(Data_C3$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C4 <- median(Data_C4$Percentage_Aggression, na.rm = TRUE)

Median_Aggression_C5 <- median(Data_C5$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C6 <- median(Data_C6$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C7 <- median(Data_C7$Percentage_Aggression, na.rm = TRUE)
Median_Aggression_C8 <- median(Data_C8$Percentage_Aggression, na.rm = TRUE)



#From an older version but useful for analysis
Match_price <- Data_C1 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C1<- Data_C1 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C2 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C2<- Data_C2 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C3 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C3<- Data_C3 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C4 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C4<- Data_C4 %>% left_join(Match_price, by = c("id", "round"))

Match_price <- Data_C5 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C5<- Data_C5 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C6 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C6<- Data_C6 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C7 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C7<- Data_C7 %>% left_join(Match_price, by = c("id", "round"))
Match_price <- Data_C8 %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_C8<- Data_C8 %>% left_join(Match_price, by = c("id", "round"))

#create the binary variable

Data_C1 <- Data_C1 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C1, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C1, 0, NA)))
Data_C2 <- Data_C2 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C2, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C2, 0, NA)))
Data_C3 <- Data_C3 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C3, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C3, 0, NA)))
Data_C4 <- Data_C4 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C4, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C4, 0, NA)))

Data_C5 <- Data_C5 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C5, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C5, 0, NA)))
Data_C6 <- Data_C6 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C6, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C6, 0, NA)))
Data_C7 <- Data_C7 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C7, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C7, 0, NA)))
Data_C8 <- Data_C8 %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_C8, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_C8, 0, NA)))





#create histogramm



par(mfrow = c(2, 2))
hist(Data_C1$Percentage_Aggression, main = "Data C1", xlab = "Percentage_Aggression")
hist(Data_C2$Percentage_Aggression, main = "Data C2", xlab = "Percentage_Aggression")
hist(Data_C3$Percentage_Aggression, main = "Data C3", xlab = "Percentage_Aggression")
hist(Data_C4$Percentage_Aggression, main = "Data C4", xlab = "Percentage_Aggression")

hist(Data_C5$Percentage_Aggression, main = "Data C5", xlab = "Percentage_Aggression")
hist(Data_C6$Percentage_Aggression, main = "Data C6", xlab = "Percentage_Aggression")
hist(Data_C7$Percentage_Aggression, main = "Data C7", xlab = "Percentage_Aggression")
hist(Data_C8$Percentage_Aggression, main = "Data C8", xlab = "Percentage_Aggression")



#Gain of Profit__________________________________________________________________________________________________________________________________________________________________________


#Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions


Median_C1_GOP <- median(Data_C1$GOP, na.rm = TRUE)
Median_C2_GOP <- median(Data_C2$GOP, na.rm = TRUE)
Median_C3_GOP <- median(Data_C3$GOP, na.rm = TRUE)
Median_C4_GOP <- median(Data_C4$GOP, na.rm = TRUE)

Median_C5_GOP <- median(Data_C5$GOP, na.rm = TRUE)
Median_C6_GOP <- median(Data_C6$GOP, na.rm = TRUE)
Median_C7_GOP <- median(Data_C7$GOP, na.rm = TRUE)
Median_C8_GOP <- median(Data_C8$GOP, na.rm = TRUE)



#Binary Variable

#Seller

Data_C1 <- Data_C1 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C1_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C1_GOP, 0, NA)))
Data_C2 <- Data_C2 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C2_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C2_GOP, 0, NA)))
Data_C3 <- Data_C3 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C3_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C3_GOP, 0, NA)))
Data_C4 <- Data_C4 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C4_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C4_GOP, 0, NA)))

#Buyer

Data_C5 <- Data_C5 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C5_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C5_GOP, 0, NA)))
Data_C6 <- Data_C6 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C6_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C6_GOP, 0, NA)))
Data_C7 <- Data_C7 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C7_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C7_GOP, 0, NA)))
Data_C8 <- Data_C8 %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_C8_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_C8_GOP, 0, NA)))




#Logistic Regressions______________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_C1 <- Data_C1[!is.na(Data_C1$GOP_B), ]

#Creating a Model

model_Patience_C1 <- glm(GOP_B ~ Patience, data = Data_C1, family = binomial)
summary(model_Patience_C1)



# Create a data frame with predictions
Data_for_Regression_C1$predicted_prob <- predict(model_Patience_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C1)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set


Data_for_Regression_C2 <- Data_C2[!is.na(Data_C2$GOP_B), ]

#Creating a Model

model_Patience_C2 <- glm(GOP_B ~ Patience, data = Data_C2, family = binomial)
summary(model_Patience_C2)



# Create a data frame with predictions
Data_for_Regression_C2$predicted_prob <- predict(model_Patience_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C2)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set


Data_for_Regression_C3 <- Data_C3[!is.na(Data_C3$GOP_B), ]

#Creating a Model

model_Patience_C3 <- glm(GOP_B ~ Patience, data = Data_C3, family = binomial)
summary(model_Patience_C3)



# Create a data frame with predictions
Data_for_Regression_C3$predicted_prob <- predict(model_Patience_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C3)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set


Data_for_Regression_C4 <- Data_C4[!is.na(Data_C4$GOP_B), ]

#Creating a Model

model_Patience_C4 <- glm(GOP_B ~ Patience, data = Data_C4, family = binomial)
summary(model_Patience_C4)



# Create a data frame with predictions
Data_for_Regression_C4$predicted_prob <- predict(model_Patience_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C4)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_C5 <- Data_C5[!is.na(Data_C5$GOP_B), ]

#Creating a Model

model_Patience_C5 <- glm(GOP_B ~ Patience, data = Data_C5, family = binomial)
summary(model_Patience_C5)



# Create a data frame with predictions
Data_for_Regression_C5$predicted_prob <- predict(model_Patience_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C5)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()
#Choosing the data set


Data_for_Regression_C6 <- Data_C6[!is.na(Data_C6$GOP_B), ]

#Creating a Model

model_Patience_C6 <- glm(GOP_B ~ Patience, data = Data_C6, family = binomial)
summary(model_Patience_C6)



# Create a data frame with predictions
Data_for_Regression_C6$predicted_prob <- predict(model_Patience_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C6)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set


Data_for_Regression_C7 <- Data_C7[!is.na(Data_C7$GOP_B), ]

#Creating a Model

model_Patience_C7 <- glm(GOP_B ~ Patience, data = Data_C7, family = binomial)
summary(model_Patience_C7)



# Create a data frame with predictions
Data_for_Regression_C7$predicted_prob <- predict(model_Patience_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C7)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set


Data_for_Regression_C8 <- Data_C8[!is.na(Data_C8$GOP_B), ]

#Creating a Model

model_Patience_C8 <- glm(GOP_B ~ Patience, data = Data_C8, family = binomial)
summary(model_Patience_C8)



# Create a data frame with predictions
Data_for_Regression_C8$predicted_prob <- predict(model_Patience_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C8)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Activeness_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

#Choosing the data set

Data_for_Regression_C1 <- Data_C1[!is.na(Data_C1$GOP_B), ]


#Creating a Model

model_Activeness_C1 <- glm(GOP_B ~ Activeness, data = Data_C1, family = binomial)
summary(model_Activeness_C1)



# Create a data frame with predictions
Data_for_Regression_C1$predicted_prob <- predict(model_Activeness_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness(C1)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set

#First Test for Activeness_C2

Data_for_Regression_C2 <- Data_C2[!is.na(Data_C2$GOP_B), ]


#Creating a Model

model_Activeness_C2 <- glm(GOP_B ~ Activeness, data = Data_C2, family = binomial)
summary(model_Activeness_C2)


# Create a data frame with predictions
Data_for_Regression_C2$predicted_prob <- predict(model_Activeness_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C2)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

#First Test for Activeness_C3

Data_for_Regression_C3 <- Data_C3[!is.na(Data_C3$GOP_B), ]


#Creating a Model

model_Activeness_C3 <- glm(GOP_B ~ Activeness, data = Data_C3, family = binomial)
summary(model_Activeness_C3)


# Create a data frame with predictions
Data_for_Regression_C3$predicted_prob <- predict(model_Activeness_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C3)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()





#Choosing the data set

#First Test for Activeness_C4

Data_for_Regression_C4 <- Data_C4[!is.na(Data_C4$GOP_B), ]


#Creating a Model

model_Activeness_C4 <- glm(GOP_B ~ Activeness, data = Data_C4, family = binomial)
summary(model_Activeness_C4)


# Create a data frame with predictions
Data_for_Regression_C4$predicted_prob <- predict(model_Activeness_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C4)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

#First Test for Activeness_C5

Data_for_Regression_C5 <- Data_C5[!is.na(Data_C5$GOP_B), ]


#Creating a Model

model_Activeness_C5 <- glm(GOP_B ~ Activeness, data = Data_C5, family = binomial)
summary(model_Activeness_C5)


# Create a data frame with predictions
Data_for_Regression_C5$predicted_prob <- predict(model_Activeness_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C5)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Choosing the data set

Data_for_Regression_C6 <- Data_C6[!is.na(Data_C6$GOP_B), ]


#Creating a Model

model_Activeness_C6 <- glm(GOP_B ~ Activeness, data = Data_C6, family = binomial)
summary(model_Activeness_C6)


# Create a data frame with predictions
Data_for_Regression_C6$predicted_prob <- predict(model_Activeness_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C6)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_C7 <- Data_C7[!is.na(Data_C7$GOP_B), ]


#Creating a Model

model_Activeness_C7 <- glm(GOP_B ~ Activeness, data = Data_C7, family = binomial)
summary(model_Activeness_C7)


# Create a data frame with predictions
Data_for_Regression_C7$predicted_prob <- predict(model_Activeness_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C7)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set

Data_for_Regression_C8 <- Data_C8[!is.na(Data_C8$GOP_B), ]


#Creating a Model

model_Activeness_C8 <- glm(GOP_B ~ Activeness, data = Data_C8, family = binomial)
summary(model_Activeness_C8)


# Create a data frame with predictions
Data_for_Regression_C8$predicted_prob <- predict(model_Activeness_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C8)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Flexible_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_C1 <- Data_C1[!is.na(Data_C1$GOP_B), ]

#Creating a Model

model_Flexible_C1 <- glm(GOP_B ~ Flexible, data = Data_C1, family = binomial)
summary(model_Flexible_C1)



# Create a data frame with predictions

Data_for_Regression_C1$predicted_prob <- predict(model_Flexible_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C1)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_C2 <- Data_C2[!is.na(Data_C2$GOP_B), ]

#Creating a Model

model_Flexible_C2 <- glm(GOP_B ~ Flexible, data = Data_C2, family = binomial)
summary(model_Flexible_C2)



# Create a data frame with predictions

Data_for_Regression_C2$predicted_prob <- predict(model_Flexible_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C2)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Choosing the data set

Data_for_Regression_C3 <- Data_C3[!is.na(Data_C3$GOP_B), ]

#Creating a Model

model_Flexible_C3 <- glm(GOP_B ~ Flexible, data = Data_C3, family = binomial)
summary(model_Flexible_C3)



# Create a data frame with predictions

Data_for_Regression_C3$predicted_prob <- predict(model_Flexible_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C3)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_C4 <- Data_C4[!is.na(Data_C4$GOP_B), ]

#Creating a Model

model_Flexible_C4 <- glm(GOP_B ~ Flexible, data = Data_C4, family = binomial)
summary(model_Flexible_C4)



# Create a data frame with predictions

Data_for_Regression_C4$predicted_prob <- predict(model_Flexible_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C4)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_C5 <- Data_C5[!is.na(Data_C5$GOP_B), ]

#Creating a Model

model_Flexible_C5 <- glm(GOP_B ~ Flexible, data = Data_C5, family = binomial)
summary(model_Flexible_C5)



# Create a data frame with predictions

Data_for_Regression_C5$predicted_prob <- predict(model_Flexible_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C5)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set

Data_for_Regression_C6 <- Data_C6[!is.na(Data_C6$GOP_B), ]

#Creating a Model

model_Flexible_C6 <- glm(GOP_B ~ Flexible, data = Data_C6, family = binomial)
summary(model_Flexible_C6)



# Create a data frame with predictions

Data_for_Regression_C6$predicted_prob <- predict(model_Flexible_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C6)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Choosing the data set

Data_for_Regression_C7 <- Data_C7[!is.na(Data_C7$GOP_B), ]

#Creating a Model

model_Flexible_C7 <- glm(GOP_B ~ Flexible, data = Data_C7, family = binomial)
summary(model_Flexible_C7)



# Create a data frame with predictions

Data_for_Regression_C7$predicted_prob <- predict(model_Flexible_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (c7)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()
#Choosing the data set

Data_for_Regression_C8 <- Data_C8[!is.na(Data_C8$GOP_B), ]

#Creating a Model

model_Flexible_C8 <- glm(GOP_B ~ Flexible, data = Data_C8, family = binomial)
summary(model_Flexible_C8)



# Create a data frame with predictions

Data_for_Regression_C8$predicted_prob <- predict(model_Flexible_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C8)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                             
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
                                                                                                                                                                                        
#GoP ~ Aggression_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

#Choosing the data set

Data_for_Regression_C1 <- Data_C1[!is.na(Data_C1$GOP_B), ]

#Creating a Model

model_Aggression_C1 <- glm(GOP_B ~ Aggression, data = Data_C1, family = binomial)
summary(model_Aggression_C1)



# Create a data frame with predictions

Data_for_Regression_C1$predicted_prob <- predict(model_Aggression_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C1)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
#Choosing the data set

Data_for_Regression_C2 <- Data_C2[!is.na(Data_C2$GOP_B), ]

#Creating a Model

model_Aggression_C2 <- glm(GOP_B ~ Aggression, data = Data_C2, family = binomial)
summary(model_Aggression_C2)



# Create a data frame with predictions

Data_for_Regression_C2$predicted_prob <- predict(model_Aggression_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C2)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
#Choosing the data set

Data_for_Regression_C3 <- Data_C3[!is.na(Data_C3$GOP_B), ]

#Creating a Model

model_Aggression_C3 <- glm(GOP_B ~ Aggression, data = Data_C3, family = binomial)
summary(model_Aggression_C3)



# Create a data frame with predictions

Data_for_Regression_C3$predicted_prob <- predict(model_Aggression_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C3)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                                                                                                                                                                                       
                                                                                                                                                                                          

#Choosing the data set

Data_for_Regression_C4 <- Data_C4[!is.na(Data_C4$GOP_B), ]

#Creating a Model

model_Aggression_C4 <- glm(GOP_B ~ Aggression, data = Data_C4, family = binomial)
summary(model_Aggression_C4)



# Create a data frame with predictions

Data_for_Regression_C4$predicted_prob <- predict(model_Aggression_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C4)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       

#Choosing the data set

Data_for_Regression_C5 <- Data_C5[!is.na(Data_C5$GOP_B), ]

#Creating a Model

model_Aggression_C5 <- glm(GOP_B ~ Aggression, data = Data_C5, family = binomial)
summary(model_Aggression_C5)



# Create a data frame with predictions

Data_for_Regression_C5$predicted_prob <- predict(model_Aggression_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C5)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       

#Choosing the data set

Data_for_Regression_C6 <- Data_C6[!is.na(Data_C6$GOP_B), ]

#Creating a Model

model_Aggression_C6 <- glm(GOP_B ~ Aggression, data = Data_C6, family = binomial)
summary(model_Aggression_C6)



# Create a data frame with predictions

Data_for_Regression_C6$predicted_prob <- predict(model_Aggression_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C6)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
                                                                                                                                                                                          


#Choosing the data set

Data_for_Regression_C7 <- Data_C7[!is.na(Data_C7$GOP_B), ]

#Creating a Model

model_Aggression_C7 <- glm(GOP_B ~ Aggression, data = Data_C7, family = binomial)
summary(model_Aggression_C7)



# Create a data frame with predictions

Data_for_Regression_C7$predicted_prob <- predict(model_Aggression_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C7)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       






#Choosing the data set

Data_for_Regression_C8 <- Data_C8[!is.na(Data_C8$GOP_B), ]

#Creating a Model

model_Aggression_C8 <- glm(GOP_B ~ Aggression, data = Data_C8, family = binomial)
summary(model_Aggression_C8)



# Create a data frame with predictions

Data_for_Regression_C8$predicted_prob <- predict(model_Aggression_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C8)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       







################################Section :Clean Up ###########################################






#Hide helping columns
Data_C1 <- Data_C1 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C2 <- Data_C2 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C3 <- Data_C3 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C4 <- Data_C4 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)

Data_C5 <- Data_C5 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C6 <- Data_C6 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C7 <- Data_C7 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_C8 <- Data_C8 %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)


Data_C1 <- Data_C1 %>% relocate(Match_price,.after = match_time)
Data_C2 <- Data_C2 %>% relocate(Match_price,.after = match_time)
Data_C3 <- Data_C3 %>% relocate(Match_price,.after = match_time)
Data_C4 <- Data_C4 %>% relocate(Match_price,.after = match_time)

Data_C5 <- Data_C5 %>% relocate(Match_price,.after = match_time)
Data_C6 <- Data_C6 %>% relocate(Match_price,.after = match_time)
Data_C7 <- Data_C7 %>% relocate(Match_price,.after = match_time)
Data_C8 <- Data_C8 %>% relocate(Match_price,.after = match_time)




dev.off()




sink("path")


summary(model_Flexible_C1)
summary(model_Flexible_C2)
summary(model_Flexible_C3)
summary(model_Flexible_C4)

summary(model_Flexible_C5)
summary(model_Flexible_C6)
summary(model_Flexible_C7)
summary(model_Flexible_C8)


summary(model_Patience_C1)
summary(model_Patience_C2)
summary(model_Patience_C3)
summary(model_Patience_C4)

summary(model_Patience_C5)
summary(model_Patience_C6)
summary(model_Patience_C7)
summary(model_Patience_C8)

summary(model_Activeness_C1)
summary(model_Activeness_C2)
summary(model_Activeness_C3)
summary(model_Activeness_C4)

summary(model_Activeness_C5)
summary(model_Activeness_C6)
summary(model_Activeness_C7)
summary(model_Activeness_C8)


summary(model_Aggression_C1)
summary(model_Aggression_C2)
summary(model_Aggression_C3)
summary(model_Aggression_C4)

summary(model_Aggression_C5)
summary(model_Aggression_C6)
summary(model_Aggression_C7)
summary(model_Aggression_C8)




sink()

