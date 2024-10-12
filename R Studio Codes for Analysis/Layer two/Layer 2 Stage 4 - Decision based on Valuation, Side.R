#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#__________________________________________________________________________________________________________________________________________________________________________________

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Finalstages\\Plots_Stage_4.pdf")

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

#only for success


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


#Creation of the binary variable


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



#Binary Variable


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







#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#GoP ~ Quick and aggressive


#Choosing the data set

Data_for_Regression_Seller_bad <- Data_Seller_bad[!is.na(Data_Seller_bad$GOP_B), ]

#Creating a Model

model_Q_A_Seller_bad <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_bad, family = binomial)
summary(model_Q_A_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Q_A_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller Bad",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Seller_Good <- Data_Seller_Good[!is.na(Data_Seller_Good$GOP_B), ]

#Creating a Model

model_Q_A_Seller_Good <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_Good, family = binomial)
summary(model_Q_A_Seller_Good)



# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Q_A_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller Good",
                                                                                                                                                                                                           x = "Quick and Aggressive ",
                                                                                                                                                                                                           y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad <- Data_Buyer_bad[!is.na(Data_Buyer_bad$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_bad <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_bad, family = binomial)
summary(model_Q_A_Buyer_bad)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Q_A_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Buyer Bad",
                                                                                                                                                                                                         x = "Quick and Aggressive ",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good <- Data_Buyer_Good[!is.na(Data_Buyer_Good$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_Good <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_Good, family = binomial)
summary(model_Q_A_Buyer_Good)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good$predicted_prob <- predict(model_Q_A_Buyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#GoP ~ Slow and aggressive_______________________________________________________________________________________________________________________________________________________


#Choosing the data set

Data_for_Regression_Seller_bad <- Data_Seller_bad[!is.na(Data_Seller_bad$GOP_B), ]

#Creating a Model

model_S_A_Seller_bad <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_bad, family = binomial)
summary(model_S_A_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_S_A_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller Bad",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good <- Data_Seller_Good[!is.na(Data_Seller_Good$GOP_B), ]

#Creating a Model

model_S_A_Seller_Good <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_Good, family = binomial)
summary(model_S_A_Seller_Good)



# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_S_A_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller Good",
                                                                                                                                                                                                          x = "Slow and Aggressive",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad <- Data_Buyer_bad[!is.na(Data_Buyer_bad$GOP_B), ]

#Creating a Model

model_S_A_Buyer_bad <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_bad, family = binomial)
summary(model_S_A_Buyer_bad)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_S_A_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Bad",
                                                                                                                                                                                                        x = "Slow and Aggressive",
                                                                                                                                                                                                        y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good <- Data_Buyer_Good[!is.na(Data_Buyer_Good$GOP_B), ]

#Creating a Model

model_S_A_Buyer_Good <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_Good, family = binomial)
summary(model_S_A_Buyer_Good)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good$predicted_prob <- predict(model_S_A_Buyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Slow and soft_____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad <- Data_Seller_bad[!is.na(Data_Seller_bad$GOP_B), ]

#Creating a Model

model_S_S_Seller_bad <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_bad, family = binomial)
summary(model_S_S_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_S_S_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller Bad",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good <- Data_Seller_Good[!is.na(Data_Seller_Good$GOP_B), ]

#Creating a Model

model_S_S_Seller_Good <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_Good, family = binomial)
summary(model_S_S_Seller_Good)



# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_S_S_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller Good",
                                                                                                                                                                                                    x = "Slow and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad <- Data_Buyer_bad[!is.na(Data_Buyer_bad$GOP_B), ]

#Creating a Model

model_S_S_Buyer_bad <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_bad, family = binomial)
summary(model_S_S_Buyer_bad)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_S_S_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer Bad",
                                                                                                                                                                                                  x = "Slow and Soft",
                                                                                                                                                                                                  y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good <- Data_Buyer_Good[!is.na(Data_Buyer_Good$GOP_B), ]

#Creating a Model

model_S_S_Buyer_Good <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_Good, family = binomial)
summary(model_S_S_Buyer_Good)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good$predicted_prob <- predict(model_S_S_Buyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer Good",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Quick and soft____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad <- Data_Seller_bad[!is.na(Data_Seller_bad$GOP_B), ]

#Creating a Model

model_Q_S_Seller_bad <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_bad, family = binomial)
summary(model_Q_S_Seller_bad)



# Create a data frame with predictions
Data_for_Regression_Seller_bad$predicted_prob <- predict(model_Q_S_Seller_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller Bad",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good <- Data_Seller_Good[!is.na(Data_Seller_Good$GOP_B), ]

#Creating a Model

model_Q_S_Seller_Good <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_Good, family = binomial)
summary(model_Q_S_Seller_Good)



# Create a data frame with predictions
Data_for_Regression_Seller_Good$predicted_prob <- predict(model_Q_S_Seller_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller Good",
                                                                                                                                                                                                     x = "Quick and Soft",
                                                                                                                                                                                                     y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad <- Data_Buyer_bad[!is.na(Data_Buyer_bad$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_bad <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_bad, family = binomial)
summary(model_Q_S_Buyer_bad)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad$predicted_prob <- predict(model_Q_S_Buyer_bad, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer Bad",
                                                                                                                                                                                                   x = "Quick and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good <- Data_Buyer_Good[!is.na(Data_Buyer_Good$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_Good <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_Good, family = binomial)
summary(model_Q_S_Buyer_Good)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good$predicted_prob <- predict(model_Q_S_Buyer_Good, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer Good",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()










################################Section :Clean Up ###########################################





#Hide helping columns
Data_Seller_bad <- Data_Seller_bad %>% select(-Valuation_groups)
Data_Seller_Good <- Data_Seller_Good %>% select(-Valuation_groups)
Data_Buyer_bad <- Data_Buyer_bad %>% select(-Valuation_groups)
Data_Buyer_Good <- Data_Buyer_Good %>% select(-Valuation_groups)




dev.off()

sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Stage_4.txt")

summary(model_Q_A_Buyer_bad)
summary(model_Q_A_Buyer_Good)
summary(model_Q_A_Seller_bad)
summary(model_Q_A_Seller_Good)

summary(model_Q_S_Buyer_bad)
summary(model_Q_S_Buyer_Good)
summary(model_Q_S_Seller_bad)
summary(model_Q_S_Seller_Good)

summary(model_S_S_Buyer_bad)
summary(model_S_S_Buyer_Good)
summary(model_S_S_Seller_bad)
summary(model_S_S_Seller_Good)

summary(model_S_A_Buyer_bad)
summary(model_S_A_Buyer_Good)
summary(model_S_A_Seller_bad)
summary(model_S_A_Seller_Good)

sink()


