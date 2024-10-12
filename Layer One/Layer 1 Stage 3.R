#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________



#creation of a PDF, containing the generated plots

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Plots_SPlit_2.pdf")

#Adding Gain of Profit as a Column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

#Histogram for Gain of Profit

hist(Data$GOP)


#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


#Splitting up the Data set



par(mfrow = c(1,2))
hist(Data$valuation[Data$side == "Seller"], prob = T, xlab = "Valuation", main = "Seller")
hist(Data$valuation[Data$side == "Buyer"], prob = T, xlab = "Valuation", main = "Buyer")



Data <- Data %>%
  mutate(Valuation_categories = case_when(
    side == "Seller" ~ 1, # Optimal Valuation Category
    side == "Buyer" ~ 2,  # Optimal Valuation Category

    TRUE ~ NA_real_ 
  ))


#seller

Data_Seller <- filter(Data, Valuation_categories == 1)
Data_Buyer <- filter(Data, Valuation_categories == 2)






#Section: Behavioral Variables

#__________________________________________________________________________________________________________________________________________________________________________________



#Variable Number 1

#Activeness(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping column Amount of bids:

#It shows the Amount Of Bids till a succesfull trade
#amount of bids per round for a specifc ID




Bids_Amount_Seller <- Data_Seller %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Seller <- Data_Seller %>% left_join(Bids_Amount_Seller, by = c("id", "round"))

Bids_Amount_Buyer <- Data_Buyer %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data_Buyer <- Data_Buyer %>% left_join(Bids_Amount_Buyer, by = c("id", "round"))



#only showing for done deals bid so it doesn't influence the histogram



Data_Seller <- Data_Seller %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))

Data_Buyer <- Data_Buyer %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))




par(mfrow = c(2, 2))
hist(Data_Seller$Amount_of_Bids, main = "Data Seller", xlab = "Amount of Bids")
hist(Data_Buyer$Amount_of_Bids, main = "Data _Buyer", xlab = "Amount of Bids")



median_Seller <- median(Data_Seller$Amount_of_Bids, na.rm = TRUE)
median_Buyer <- median(Data_Buyer$Amount_of_Bids, na.rm = TRUE)



#Binary variable


Data_Seller <- Data_Seller %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_Seller, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_Seller, 0, NA)))

hist(Data_Seller$Activeness, main = "Data_Seller", xlab = "Activeness")


Data_Buyer <- Data_Buyer %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_Buyer, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_Buyer, 0, NA)))

hist(Data_Seller$Activeness, main ="Data_Buyer", xlab = "Activeness")




#Flexebility(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creatin the helping Columns: Min_Bid, Max_Bid and Bid_Spread -  these show the lowest/highest Bid of an Id in a specific round and the spread between it
#We are trying to determine how much the player fluctuates with his offers.

#seller


Bid_Spread_Seller <- Data_Seller %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Seller <- Data_Seller %>% left_join(Bid_Spread_Seller, by = c("id", "round"))

Bid_Spread_Buyer <- Data_Buyer %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data_Buyer <- Data_Buyer %>% left_join(Bid_Spread_Buyer, by = c("id", "round"))




#And again only for successful trades

#seller

Data_Seller <- Data_Seller %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))

Data_Buyer <- Data_Buyer %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))


#Histogram for the bid spread of each Valuation Category it 



par(mfrow = c(2, 2))
hist(Data_Seller$Bid_Spread, main = "Data Seller", xlab = "Bid Spread")
hist(Data_Buyer$Bid_Spread, main = "Data _Buyer", xlab = "Bid Spread")

#Median of the spread so we can create a binary variable 

median_Seller_Spread <- median(Data_Seller$Bid_Spread, na.rm = TRUE)
median_Buyer_Spread <- median(Data_Buyer$Bid_Spread, na.rm = TRUE)


#Creating the binary variable called Flexibility 

#Flexibility

#Seller

Data_Seller <- Data_Seller %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Seller_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Seller_Spread, 0, NA)))

Data_Buyer <- Data_Buyer %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Buyer_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Buyer_Spread, 0, NA)))


#Histogram

#Seller

hist(Data_Seller$Flexible, main ="Data_Seller", xlab = "Flexebile")
hist(Data_Buyer$Flexible, main ="Data_Buyer", xlab = "Flexebile")







#Patience__________________________________________________________________________________________________________________________________________________________________________

#We are seeking to observe if the time when the first order is entered, influences gain of profit 
#First a help column looking for the first order of every round



Time_of_order_Seller <- Data_Seller %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_Seller <- Data_Seller %>% left_join(Time_of_order_Seller, by = c("id", "round"))


Time_of_order_Buyer <- Data_Buyer %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data_Buyer <- Data_Buyer %>% left_join(Time_of_order_Buyer, by = c("id", "round"))




#only for done deals


Data_Seller <- Data_Seller %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Data_Buyer <- Data_Buyer %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))



#Then the median of this time

Median_Seller_Patience <- median(Data_Seller$Time_of_order, na.rm = TRUE)
Median_Buyer_Patience <- median(Data_Buyer$Time_of_order, na.rm = TRUE)

#And create the binary variable


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



#Aggression defined by the absolute value of bid - valuation in percentage to the valuation


Data_Buyer <- Data_Buyer %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

Median_Aggression_Buyer <- median(Data_Buyer$Percentage_Aggression, na.rm = TRUE)

#From an older version but useful for analysis
Match_price <- Data_Buyer %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_Buyer<- Data_Buyer %>% left_join(Match_price, by = c("id", "round"))


#create the binary 

Data_Buyer <- Data_Buyer %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_Buyer, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_Buyer, 0, NA)))





Data_Seller <- Data_Seller %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

Median_Aggression_Seller <- median(Data_Seller$Percentage_Aggression, na.rm = TRUE)

#From an older version but useful for analysis
Match_price <- Data_Seller %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data_Seller<- Data_Seller %>% left_join(Match_price, by = c("id", "round"))


#create the binary 

Data_Seller <- Data_Seller %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression_Seller, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression_Seller, 0, NA)))



#create histogramm

par(mfrow = c(2, 2))
hist(Data_Seller$Percentage_Aggression, main = "Data Seller", xlab = "Percentage_Aggression")
hist(Data_Buyer$Percentage_Aggression, main = "Data _Buyer", xlab = "Percentage_Aggression")



#Gain of Profit__________________________________________________________________________________________________________________________________________________________________________


#Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions


Median_Seller_GOP <- median(Data_Seller$GOP, na.rm = TRUE)
Median_Buyer_GOP <- median(Data_Buyer$GOP, na.rm = TRUE)



#Binary Variable



Data_Seller <- Data_Seller %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Seller_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Seller_GOP, 0, NA)))
Data_Buyer <- Data_Buyer %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_Buyer_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_Buyer_GOP, 0, NA)))



#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#Test for Patience Seller

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_Patience_Seller <- glm(GOP_B ~ Patience, data = Data_Seller, family = binomial)
summary(model_Patience_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_Patience_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (Seller)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#Test for Patience _Buyer


Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_Patience_Buyer <- glm(GOP_B ~ Patience, data = Data_Buyer, family = binomial)
summary(model_Patience_Buyer)



# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_Patience_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (_Buyer)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Activeness Seller

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]


#Creating a Model

model_Activeness_Seller <- glm(GOP_B ~ Activeness, data = Data_Seller, family = binomial)
summary(model_Activeness_Seller)



# Create a data frame with predictions
Data_for_Regression_Seller$predicted_prob <- predict(model_Activeness_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness(Seller)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Test for Activeness_Buyer

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]


#Creating a Model

model_Activeness_Buyer <- glm(GOP_B ~ Activeness, data = Data_Buyer, family = binomial)
summary(model_Activeness_Buyer)


# Create a data frame with predictions
Data_for_Regression_Buyer$predicted_prob <- predict(model_Activeness_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (_Buyer)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Flexible Seller

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_Flexible_Seller <- glm(GOP_B ~ Flexible, data = Data_Seller, family = binomial)
summary(model_Flexible_Seller)



# Create a data frame with predictions

Data_for_Regression_Seller$predicted_prob <- predict(model_Flexible_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (Seller)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Test for Flexible _Buyer

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_Flexible_Buyer <- glm(GOP_B ~ Flexible, data = Data_Buyer, family = binomial)
summary(model_Flexible_Buyer)



# Create a data frame with predictions

Data_for_Regression_Buyer$predicted_prob <- predict(model_Flexible_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (_Buyer)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#Test for Aggression Seller

Data_for_Regression_Seller <- Data_Seller[!is.na(Data_Seller$GOP_B), ]

#Creating a Model

model_Aggression_Seller <- glm(GOP_B ~ Aggression, data = Data_Seller, family = binomial)
summary(model_Aggression_Seller)



# Create a data frame with predictions

Data_for_Regression_Seller$predicted_prob <- predict(model_Aggression_Seller, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (Seller)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       


#Test for Aggression _Buyer

Data_for_Regression_Buyer <- Data_Buyer[!is.na(Data_Buyer$GOP_B), ]

#Creating a Model

model_Aggression_Buyer <- glm(GOP_B ~ Aggression, data = Data_Buyer, family = binomial)
summary(model_Aggression_Buyer)


# Create a data frame with predictions

Data_for_Regression_Buyer$predicted_prob <- predict(model_Aggression_Buyer, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (Buyer)",
                                                                                                                                                                                             x = "Aggression",
                                                                                                                                                                                             y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                                                                                                                                                           





#############################################################################################

################################Section :Clean Up ###########################################

#############################################################################################




#Hide helping columns
Data_Seller <- Data_Seller %>% select(-Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)
Data_Buyer <- Data_Buyer %>% select(-Max_Bid, -Min_Bid, -Min_Time,-Valuation_categories)


Data_Seller <- Data_Seller %>% relocate(Match_price,.after = match_time)
Data_Buyer <- Data_Buyer %>% relocate(Match_price,.after = match_time)


#done

dev.off()
sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Split_2.txt")

summary(model_Activeness_Buyer)
summary(model_Activeness_Seller)
summary(model_Aggression_Seller)
summary(model_Aggression_Buyer)
summary(model_Flexible_Buyer)
summary(model_Flexible_Seller)
summary(model_Patience_Buyer)
summary(model_Patience_Seller)

sink()
