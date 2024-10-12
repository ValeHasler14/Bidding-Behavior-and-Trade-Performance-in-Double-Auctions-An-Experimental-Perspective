#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#__________________________________________________________________________________________________________________________________________________________________________________

#creation of a PDF, containing the generated plots

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Finalstages\\Plots_Stage_3.pdf")

#Adding Gain of Profit as a Column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

#Histogram for Gain of Profit

hist(Data$GOP)


#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


#Splitting up the Data set


Data <- Data %>%
  mutate(Valuation_categories = case_when(
    side == "Seller" ~ 2, 
    side == "Buyer" ~ 1,  
    
    TRUE ~ NA_real_ 
  ))


Data_Buyer <- filter(Data, Valuation_categories == 1)
Data_Seller <- filter(Data, Valuation_categories == 2)






#Section: Behavioral Variables

#__________________________________________________________________________________________________________________________________________________________________________________



#Patience__________________________________________________________________________________________________________________________________________________________________________

#We are seeking to observe if the time when the first order is entered, influences gain of profit 
#First a help column looking for the first order of every round


Time_of_order_Seller <- Data_Seller %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_Seller <- Data_Seller %>% left_join(Time_of_order_Seller, by = c("id", "round"))



Time_of_order_Buyer <- Data_Buyer %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_Buyer <- Data_Buyer %>% left_join(Time_of_order_Buyer, by = c("id", "round"))




Data_Seller <- Data_Seller %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_Buyer <- Data_Buyer %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))





#Then the median of this time in the specific groups

Median_Seller_Patience <- median(Data_Seller$Time_of_order, na.rm = TRUE)

Median_Buyer_Patience <- median(Data_Buyer$Time_of_order, na.rm = TRUE)



#Creation of the binary variable


Data_Seller <- Data_Seller %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Seller_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Seller_Patience, 0, NA)))


Data_Buyer <- Data_Buyer %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Buyer_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Buyer_Patience, 0, NA)))



#Aggression________________________________________________________________________________________________________________________________________________________________________



#Aggression defined by the absolute value of bid - valuation in realtion to the valuation

#Creating the "First Bid" column


first_bid_Seller <- Data_Seller %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_Seller <- Data_Seller %>% left_join(first_bid_Seller, by = c("id", "round"))

first_bid_Buyer <- Data_Buyer %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data_Buyer <- Data_Buyer %>% left_join(first_bid_Buyer, by = c("id", "round"))



#only for considering done deals
Data_Seller <- Data_Seller %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))

Data_Buyer <- Data_Buyer %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))


#Calculating the Aggression percentage value

Data_Seller <- Data_Seller %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

Data_Buyer <- Data_Buyer %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

#Medians


Median_Aggression_Seller <- median(Data_Seller$Percentage_Aggression, na.rm = TRUE)

Median_Aggression_Buyer <- median(Data_Buyer$Percentage_Aggression, na.rm = TRUE)


#From an older version but useful for analysis
Match_price <- Data_Seller %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_Seller<- Data_Seller %>% left_join(Match_price, by = c("id", "round"))


Match_price <- Data_Buyer %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_Buyer<- Data_Buyer %>% left_join(Match_price, by = c("id", "round"))



#Creation of the final binary variable "Aggression" 

Data_Seller <- Data_Seller %>%
  mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Seller, 1, ifelse(Percentage_Aggression <= Median_Aggression_Seller, 0, NA)))


Data_Buyer <- Data_Buyer %>%
  mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression_Buyer, 1, ifelse(Percentage_Aggression <= Median_Aggression_Buyer, 0, NA)))





#Gain of Profit__________________________________________________________________________________________________________________________________________________________________________


#Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions


Median_Buyer_GOP <- median(Data_Buyer$GOP, na.rm = TRUE)

Median_Seller_GOP <- median(Data_Seller$GOP, na.rm = TRUE)



#Binary Variable


Data_Buyer <- Data_Buyer %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Buyer_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Buyer_GOP, 0, NA)))

Data_Seller <- Data_Seller %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Seller_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Seller_GOP, 0, NA)))




#Strategies________________________________________________________________________________________________________________________________________________________________________


#Quick and Aggressive; Patience = 0; Aggression = 1


Data_Buyer <- Data_Buyer %>%
  mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))


Data_Seller <- Data_Seller %>%
  mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))


#Slow and Aggressive; Patience = 1; Aggression = 1


Data_Buyer <- Data_Buyer %>%
  mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))


Data_Seller <- Data_Seller %>%
  mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))


#Slow and Soft; Patience = 1; Aggression = 0



Data_Buyer <- Data_Buyer %>%
  mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))


Data_Seller <- Data_Seller %>%
  mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))


#Quick and Soft; Patience = 0; Aggression = 0



Data_Buyer <- Data_Buyer %>%
  mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))


Data_Seller <- Data_Seller %>%
  mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))







#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#GoP ~ Quick and aggressive


#Choosing the data set

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_Q_A_Seller <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller, family = binomial)
summary(model_Q_A_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_Q_A_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller ",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_Q_A_Buyer <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer, family = binomial)
summary(model_Q_A_Buyer)



# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_Q_A_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer ",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#GoP ~ Slow and aggressive_______________________________________________________________________________________________________________________________________________________


#Choosing the data set

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_S_A_Seller <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller, family = binomial)
summary(model_S_A_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_S_A_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller ",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#Choosing the data set

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_S_A_Buyer <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer, family = binomial)
summary(model_S_A_Buyer)



# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_S_A_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer ",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Slow and soft_____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_S_S_Seller <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller, family = binomial)
summary(model_S_S_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_S_S_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller ",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#Choosing the data set

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_S_S_Buyer <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer, family = binomial)
summary(model_S_S_Buyer)



# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_S_S_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer ",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Quick and soft____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_Q_S_Seller <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller, family = binomial)
summary(model_Q_S_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_Q_S_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller ",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#Choosing the data set

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_Q_S_Buyer <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer, family = binomial)
summary(model_Q_S_Buyer)



# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_Q_S_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer ",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()










################################Section :Clean Up ###########################################




#Hide helping columns


dev.off()

#Summary

sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Stage_3.txt")

summary(model_Q_A_Buyer)

summary(model_Q_A_Seller)


summary(model_Q_S_Buyer)

summary(model_Q_S_Seller)


summary(model_S_S_Buyer)

summary(model_S_S_Seller)


summary(model_S_A_Buyer)

summary(model_S_A_Seller)


sink()


