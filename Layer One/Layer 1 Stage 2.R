#Stage 2 General Regression

#Adding Gain of Profit as a Column

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

library(tidyverse)
#Histogram for Gain of Profit

hist(Data$GOP)

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Plots_SPlit_1.pdf")



#Section: Behavioral Variables

#__________________________________________________________________________________________________________________________________________________________________________________



#Variable Number 1

#Activeness(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping column Amount of bids:

#It shows the Amount Of Bids till a succesfull trade
#amount of bids per round for a specifc ID

Bids_Amount <- Data %>% group_by(id, round) %>% summarise(Amount_of_Bids = n(), .groups = 'drop')
Data <- Data %>% left_join(Bids_Amount, by = c("id", "round"))

#only showing for the successful bid so it doesn't influence the histogram


Data <- Data %>%
  mutate(Amount_of_Bids = ifelse(is.na(match_id), NA, Amount_of_Bids))

median_AoB <- median(Data$Amount_of_Bids, na.rm = TRUE)

#Histogram for the amount of bids of each Valuation Category it 

hist(Data$Amount_of_Bids, main = "Data", xlab = "Amount of Bids")

#Now i am creating the binary variable calles Activeness which determines if a Player is overaverage active or not

Data <- Data %>%
  mutate(Activeness = ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids > median_AoB, 1, ifelse(!is.na(Amount_of_Bids) & Amount_of_Bids <= median_AoB, 0, NA)))

hist(Data$Activeness, main = "Data", xlab = "Activeness")




#Variable Number 2

#Flexibility(Not used for Theis)_____________________________________________________________________________________________________________________________________________________


#Creating the helping Columns: Min_Bid, Max_Bid and Bid_Spread -  these show the lowest/highest Bid of an Id in a specific round and the spread between it
#We are trying to determine how much the player fluctuates with his offers.



Bid_Spread <- Data %>% group_by(id, round) %>% summarise(Max_Bid = max(bid, na.rm = TRUE), Min_Bid = min(bid, na.rm = TRUE),Bid_Spread = Max_Bid - Min_Bid, .groups = 'drop')
Data <- Data %>% left_join(Bid_Spread, by = c("id", "round"))


Data <- Data %>%
  mutate(Bid_Spread = ifelse(is.na(match_id), NA, Bid_Spread))


#Median of the spread so we can create a binary variable 

median_Spread <- median(Data$Bid_Spread, na.rm = TRUE)

#Binary variable

Data <- Data %>%
  mutate(Flexible = ifelse(!is.na(Bid_Spread) & Bid_Spread > median_Spread, 1, ifelse(!is.na(Bid_Spread) & Bid_Spread <= median_Spread, 0, NA)))



#Patience__________________________________________________________________________________________________________________________________________________________________________

#We are seeking to observe if the time when the first order is entered, influences gain of profit 
#First a help column looking for the first order of every round



Time_of_order <- Data %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data <- Data %>% left_join(Time_of_order, by = c("id", "round"))


Data <- Data %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))

Median_Patience <- median(Data$Time_of_order, na.rm = TRUE)


Data <- Data %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Patience, 0, NA)))



#Aggression________________________________________________________________________________________________________________________________________________________________________



#Aggression defined by the absolute value of bid - valuation in realtion to the valuation

#Creating the "First Bid" column




first_bid <- Data %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data <- Data %>% left_join(first_bid, by = c("id", "round"))


#Aggression defined by the absolute value of bid - valuation in percentage to the valuation

Data <- Data %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))

Median_Aggression <- median(Data$Percentage_Aggression, na.rm = TRUE)

#From an older version but useful for analysis
Match_price <- Data %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data<- Data %>% left_join(Match_price, by = c("id", "round"))


#create the binary 

Data <- Data %>%
  mutate(Aggression = ifelse(!is.na(match_id) & Percentage_Aggression > Median_Aggression, 1, ifelse(!is.na(match_id) & Percentage_Aggression <= Median_Aggression, 0, NA)))




#Gain of Profit_____________________________________________________________________________________________________________________________________________________________________


#Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions


Median_GOP <- median(Data$GOP, na.rm = TRUE)


Data <- Data %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_GOP, 0, NA)))




#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#Test for Patience

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_Patience <- glm(GOP_B ~ Patience, data = Data, family = binomial)
summary(model_Patience)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_Patience, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience ",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


#Test for Activeness

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]


#Creating a Model

model_Activeness <- glm(GOP_B ~ Activeness, data = Data, family = binomial)
summary(model_Activeness)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_Activeness, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#Test for Flexible

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_Flexible <- glm(GOP_B ~ Flexible, data = Data, family = binomial)
summary(model_Flexible)



# Create a data frame with predictions

Data_for_Regression$predicted_prob <- predict(model_Flexible, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible ",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

                                                                                                                                                                                       

#Test for Aggression

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_Aggression <- glm(GOP_B ~ Aggression, data = Data, family = binomial)
summary(model_Aggression)



#Create a data frame with predictions

Data_for_Regression$predicted_prob <- predict(model_Aggression, type = "response")

#Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression ",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       


#Clean up
#Hide helping columns
Data <- Data %>% select(-Max_Bid, -Min_Bid, -Min_Time)
Data <- Data %>% relocate(Match_price,.after = match_time)






dev.off()

sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Split_1.txt")
summary(model_Activeness)
summary(model_Aggression)
summary(model_Flexible)
summary(model_Patience)

sink()