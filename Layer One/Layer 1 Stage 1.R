#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#creation of a PDF, containing the generated plots

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Plots_SPlit_0.pdf")

#Adding Gain of Profit as a Column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

#Histogram for Gain of Profit

hist(Data$GOP)


#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


#Splitting up the Data set
#Creating the helping column Valuation Groups
#Valuation grouping ; for each side later to be matched in the according categories



par(mfrow = c(1,2))
hist(Data$valuation[Data$side == "Seller"], prob = T, xlab = "Valuation", main = "Seller")
hist(Data$valuation[Data$side == "Buyer"], prob = T, xlab = "Valuation", main = "Buyer")
median_seller <- median(Data$valuation[Data$side == "Seller"])
median_buyer <- median(Data$valuation[Data$side == "Buyer"])


Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Seller") & (Data$valuation < median_seller), 1,2))

Data <- mutate(Data, Valuation_groups = ifelse((Data$side == "Buyer") & (Data$valuation < median_buyer), 3, Data$Valuation_groups))
Data$Valuation_groups <- ifelse((Data$side == "Buyer") & (Data$valuation > median_buyer), 4, Data$Valuation_groups)



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



#Variable Number 1

#Activeness(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping column Amount of bids:

#It shows the Amount Of Bids till a succesfull trade
#amount of bids per round for a specifc ID



Bids_Amount_Seller_bad <- Data_Seller_bad %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Seller_bad <- Data_Seller_bad %>% left_join(Bids_Amount_Seller_bad, by = c("id", "round"))

Bids_Amount_Seller_Good <- Data_Seller_Good %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Seller_Good <- Data_Seller_Good %>% left_join(Bids_Amount_Seller_Good, by = c("id", "round"))

Bids_Amount_Buyer_bad <- Data_Buyer_bad %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Buyer_bad <- Data_Buyer_bad %>% left_join(Bids_Amount_Buyer_bad, by = c("id", "round"))

Bids_AmountBuyer_Good <- Data_Buyer_Good %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Buyer_Good <- Data_Buyer_Good %>% left_join(Bids_AmountBuyer_Good, by = c("id", "round"))





#Histogram for the amount of bids of each Valuation Category it 

par(mfrow = c(2, 2))
hist(Data_Seller_bad$Amount_of_Bids, main = "Data C1", xlab = "Amount of Bids")
hist(Data_Seller_Good$Amount_of_Bids, main = "Data C2", xlab = "Amount of Bids")
hist(Data_Buyer_bad$Amount_of_Bids, main = "Data C3", xlab = "Amount of Bids")
hist(Data_Buyer_Good$Amount_of_Bids, main = "Data C4", xlab = "Amount of Bids")



median_Seller_bad <- median(Data_Seller_bad$Amount_of_Bids, na.rm = TRUE)
median_Seller_Good <- median(Data_Seller_Good$Amount_of_Bids, na.rm = TRUE)
median_Buyer_bad <- median(Data_Buyer_bad$Amount_of_Bids, na.rm = TRUE)
medianBuyer_Good <- median(Data_Buyer_Good$Amount_of_Bids, na.rm = TRUE)



#Now i am creating the binary variable called Activeness which determines if a player is over average active or not



Data_Seller_bad <- Data_Seller_bad %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_Seller_bad, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_Seller_bad, 0, NA)))

hist(Data_Seller_bad$Activeness, main = "Data_Seller_bad", xlab = "Activeness")


Data_Seller_Good <- Data_Seller_Good %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_Seller_Good, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_Seller_Good, 0, NA)))

hist(Data_Seller_bad$Activeness, main ="Data_Seller_Good", xlab = "Activeness")


Data_Buyer_bad <- Data_Buyer_bad %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_Buyer_bad, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_Buyer_bad, 0, NA)))

hist(Data_Buyer_bad$Activeness, main ="Data_Buyer_bad", xlab = "Activeness")


Data_Buyer_Good <- Data_Buyer_Good %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > medianBuyer_Good, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= medianBuyer_Good, 0, NA)))

hist(Data_Buyer_Good$Activeness, main ="Data_Buyer_Good", xlab = "Activeness")




#Flexebility(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping Columns: Min_Bid, Max_Bid and Bid_Spread -  these show the lowest/highest Bid of an Id in a specific round and the spread between it
#We are trying to determine how much the player fluctuates with his offers.




Bid_Spread_Seller_bad <- Data_Seller_bad %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Seller_bad <- Data_Seller_bad %>% left_join(Bid_Spread_Seller_bad, by = c("id", "round"))

Bid_Spread_Seller_Good <- Data_Seller_Good %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Seller_Good <- Data_Seller_Good %>% left_join(Bid_Spread_Seller_Good, by = c("id", "round"))

Bid_Spread_Buyer_bad <- Data_Buyer_bad %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Buyer_bad <- Data_Buyer_bad %>% left_join(Bid_Spread_Buyer_bad, by = c("id", "round"))

Bid_SpreadBuyer_Good <- Data_Buyer_Good %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Buyer_Good <- Data_Buyer_Good %>% left_join(Bid_SpreadBuyer_Good, by = c("id", "round"))




#Histogram for the bid spread of each Valuation Category it 



par(mfrow = c(2, 2))
hist(Data_Seller_bad$Bid_Spread, main = "Data C1", xlab = "Bid Spread")
hist(Data_Seller_Good$Bid_Spread, main = "Data C2", xlab = "Bid Spread")
hist(Data_Buyer_bad$Bid_Spread, main = "Data C3", xlab = "Bid Spread")
hist(Data_Buyer_Good$Bid_Spread, main = "Data C4", xlab = "Bid Spread")


#Median of the spread so we can create a binary variable 

median_Seller_bad_Spread <- median(Data_Seller_bad$Bid_Spread, na.rm = TRUE)
median_Seller_Good_Spread <- median(Data_Seller_Good$Bid_Spread, na.rm = TRUE)
median_Buyer_bad_Spread <- median(Data_Buyer_bad$Bid_Spread, na.rm = TRUE)
medianBuyer_Good_Spread <- median(Data_Buyer_Good$Bid_Spread, na.rm = TRUE)

#Creating the binary variable called Flexibility 


Data_Seller_bad <- Data_Seller_bad %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Seller_bad_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Seller_bad_Spread, 0, NA)))

Data_Seller_Good <- Data_Seller_Good %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Seller_Good_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Seller_Good_Spread, 0, NA)))

Data_Buyer_bad <- Data_Buyer_bad %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Buyer_bad_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Buyer_bad_Spread, 0, NA)))

Data_Buyer_Good <- Data_Buyer_Good %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > medianBuyer_Good_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= medianBuyer_Good_Spread, 0, NA)))


#Histogram


hist(Data_Seller_bad$Flexible, main ="Data_Seller_bad", xlab = "Flexebile")
hist(Data_Seller_Good$Flexible, main ="Data_Seller_Good", xlab = "Flexebile")
hist(Data_Buyer_bad$Flexible, main ="Data_Buyer_bad", xlab = "Flexebile")
hist(Data_Buyer_Good$Flexible, main ="Data_Buyer_Good", xlab = "Flexebile")









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




#Then the median of this time

Median_Seller_bad_Patience <- median(Data_Seller_bad$Time_of_order, na.rm = TRUE)
Median_Seller_Good_Patience <- median(Data_Seller_Good$Time_of_order, na.rm = TRUE)
Median_Buyer_bad_Patience <- median(Data_Buyer_bad$Time_of_order, na.rm = TRUE)
MedianBuyer_Good_Patience <- median(Data_Buyer_Good$Time_of_order, na.rm = TRUE)


#And create the binary variable



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



#Binary variable

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


Data_Buyer_bad <- Data_Buyer_bad %>%
  mutate(Trade = ifelse(!is.na(GOP), 1, 0))

Data_Buyer_Good <- Data_Buyer_Good %>%
  mutate(Trade = ifelse(!is.na(GOP), 1, 0))

Data_Seller_bad <- Data_Seller_bad %>%
  mutate(Trade = ifelse(!is.na(GOP), 1, 0))

Data_Seller_Good <- Data_Seller_Good %>%
  mutate(Trade = ifelse(!is.na(GOP), 1, 0))





#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#Test for Patience Seller bad

Data_for_Regression_Seller_bad <- Data_Seller_bad

#Creating a Model

model_Patience_Seller_bad <- glm(Trade ~ Patience, data = Data_Seller_bad, family = binomial)
summary(model_Patience_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Patience_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Patience, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience Seller Bad",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Test for Patience Seller Good


Data_for_Regression_Seller_Good <- Data_Seller_Good

#Creating a Model

model_Patience_Seller_Good <- glm(Trade ~ Patience, data = Data_Seller_Good, family = binomial)
summary(model_Patience_Seller_Good)



# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Patience_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Patience, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience  Seller Good",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Test for Patience Buyer bad


Data_for_Regression_Buyer_bad <- Data_Buyer_bad

#Creating a Model

model_Patience_Buyer_bad <- glm(Trade ~ Patience, data = Data_Buyer_bad, family = binomial)
summary(model_Patience_Buyer_bad)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Patience_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Patience, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience  Buyer Bad",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Test for Patience Buyer Good


Data_for_RegressionBuyer_Good <- Data_Buyer_Good

#Creating a Model

model_PatienceBuyer_Good <- glm(Trade ~ Patience, data = Data_Buyer_Good, family = binomial)
summary(model_PatienceBuyer_Good)



# Create a data frame with predictions
Data_for_RegressionBuyer_Good$predicted_prob <- predict(model_PatienceBuyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_RegressionBuyer_Good, aes(x = Patience, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience  Buyer Good",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Activeness_Seller_bad

Data_for_Regression_Seller_bad <- Data_Seller_bad


#Creating a Model

model_Activeness_Seller_bad <- glm(Trade ~ Activeness, data = Data_Seller_bad, family = binomial)
summary(model_Activeness_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Activeness_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Activeness, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on ActivenessSeller Bad",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Test for Activeness_Seller_Good

Data_for_Regression_Seller_Good <- Data_Seller_Good


#Creating a Model

model_Activeness_Seller_Good <- glm(Trade ~ Activeness, data = Data_Seller_Good, family = binomial)
summary(model_Activeness_Seller_Good)


# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Activeness_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Activeness, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness  Seller Good",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Test for Activeness_Buyer_bad

Data_for_Regression_Buyer_bad <- Data_Buyer_bad


#Creating a Model

model_Activeness_Buyer_bad <- glm(Trade ~ Activeness, data = Data_Buyer_bad, family = binomial)
summary(model_Activeness_Buyer_bad)


# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Activeness_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Activeness, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness  Buyer Bad",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()






#Test for ActivenessBuyer_Good

Data_for_RegressionBuyer_Good <- Data_Buyer_Good


#Creating a Model

model_ActivenessBuyer_Good <- glm(Trade ~ Activeness, data = Data_Buyer_Good, family = binomial)
summary(model_ActivenessBuyer_Good)


# Create a data frame with predictions
Data_for_RegressionBuyer_Good$predicted_prob <- predict(model_ActivenessBuyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_RegressionBuyer_Good, aes(x = Activeness, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness  Buyer Good",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()





#Test for Flexible Seller Bad

Data_for_Regression_Seller_bad <- Data_Seller_bad

#Creating a Model

model_Flexible_Seller_bad <- glm(Trade ~ Flexible, data = Data_Seller_bad, family = binomial)
summary(model_Flexible_Seller_bad)



# Create a data frame with predictions

Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Flexible_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Flexible, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible Seller Bad",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Test for Flexible Seller Good

Data_for_Regression_Seller_Good <- Data_Seller_Good

#Creating a Model

model_Flexible_Seller_Good <- glm(Trade ~ Flexible, data = Data_Seller_Good, family = binomial)
summary(model_Flexible_Seller_Good)



# Create a data frame with predictions

Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Flexible_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Flexible, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible  Seller Good",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Test for Flexible Buyer Bad

Data_for_Regression_Buyer_bad <- Data_Buyer_bad

#Creating a Model

model_Flexible_Buyer_bad <- glm(Trade ~ Flexible, data = Data_Buyer_bad, family = binomial)
summary(model_Flexible_Buyer_bad)



# Create a data frame with predictions

Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Flexible_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Flexible, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible  Buyer Bad",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Flexible Buyer Good

Data_for_RegressionBuyer_Good <- Data_Buyer_Good

#Creating a Model

model_FlexibleBuyer_Good <- glm(Trade ~ Flexible, data = Data_Buyer_Good, family = binomial)
summary(model_FlexibleBuyer_Good)



# Create a data frame with predictions

Data_for_RegressionBuyer_Good$predicted_prob <- predict(model_FlexibleBuyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_RegressionBuyer_Good, aes(x = Flexible, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible  Buyer Good",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Aggression Seller bad

Data_for_Regression_Seller_bad <- Data_Seller_bad[!is.na(Data_Seller_bad$Trade), ]

#Creating a Model

model_Aggression_Seller_bad <- glm(Trade ~ Aggression, data = Data_Seller_bad, family = binomial)
summary(model_Aggression_Seller_bad)



# Create a data frame with predictions

Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Aggression_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Aggression, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression Seller Bad",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       


#Test for Aggression Seller Good

Data_for_Regression_Seller_Good <- Data_Seller_Good[!is.na(Data_Seller_Good$Trade), ]

#Creating a Model

model_Aggression_Seller_Good <- glm(Trade ~ Aggression, data = Data_Seller_Good, family = binomial)
summary(model_Aggression_Seller_Good)



#Create a data frame with predictions

Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Aggression_Seller_Good, type = "response")

#Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Aggression, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression  Seller Good",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
#Test for Aggression Buyer Bad

Data_for_Regression_Buyer_bad <- Data_Buyer_bad[!is.na(Data_Buyer_bad$Trade), ]

#Creating a Model

model_Aggression_Buyer_bad <- glm(Trade ~ Aggression, data = Data_Buyer_bad, family = binomial)
summary(model_Aggression_Buyer_bad)



# Create a data frame with predictions

Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Aggression_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Aggression, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression  Buyer Bad",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                                                                                                                                                                                       


#Test for Aggression Buyer Good

Data_for_Regression_Buyer_Good <- Data_Buyer_Good[!is.na(Data_Buyer_Good$Trade), ]

#Creating a Model

model_Aggression_Buyer_Good <- glm(Trade ~ Aggression, data = Data_Buyer_Good, family = binomial)
summary(model_Aggression_Buyer_Good)



# Create a data frame with predictions

Data_for_Regression_Buyer_Good$predicted_prob <- predict(model_Aggression_Buyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good, aes(x = Aggression, y = Trade)) + geom_point(aes(color = factor(Trade)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression  Buyer Good",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       













################################Section :Clean Up ###########################################





#Hide helping columns
Data_Seller_bad <- Data_Seller_bad %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_Seller_Good <- Data_Seller_Good %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time ,-Valuation_categories)
Data_Buyer_bad <- Data_Buyer_bad %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_Buyer_Good <- Data_Buyer_Good %>% select(-Valuation_groups, -Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)




dev.off()

sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Split_0.txt")

summary(model_FlexibleBuyer_Good)
summary(model_Flexible_Buyer_bad)
summary(model_Flexible_Seller_bad)
summary(model_Flexible_Seller_Good)

summary(model_PatienceBuyer_Good)
summary(model_Patience_Buyer_bad)
summary(model_Patience_Seller_bad)
summary(model_Patience_Seller_Good)

summary(model_ActivenessBuyer_Good)
summary(model_Activeness_Buyer_bad)
summary(model_Activeness_Seller_bad)
summary(model_Activeness_Seller_Good)

sink()

