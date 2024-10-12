#Reading the Data set

Data <- read.csv("path")

#Download the necessary libraries

library(tidyverse)

#Section: Data Set Preparation
#__________________________________________________________________________________________________________________________________________________________________________________

#creation of a PDF, containing the generated plots


pdf("path")

#Adding Gain of Profit as a column

Data <- Data %>%
  mutate(GOP = ifelse(!is.na(price), abs(bid - valuation), NA))

#Histogram for Gain of Profit

hist(Data$GOP)



#Section: Behavioral Variables

#__________________________________________________________________________________________________________________________________________________________________________________



#Patience__________________________________________________________________________________________________________________________________________________________________________

#We are seeking to observe if the time when the first order is entered, influences gain of profit 
#First a help column looking for the first order of every round



Time_of_order <- Data %>% group_by(id, round) %>% summarise(Min_Time = min(time, na.rm = TRUE),Time_of_order = Min_Time, .groups = 'drop')
Data <- Data %>% left_join(Time_of_order, by = c("id", "round"))


#only for considering done deals


Data <- Data %>%
  mutate(Time_of_order = ifelse(is.na(match_id), NA, Time_of_order))




#Then the median of this time in the specific groups

Median_Patience <- median(Data$Time_of_order, na.rm = TRUE)

#Creation of the binary variable



Data <- Data %>%
  mutate(Patience = ifelse(!is.na(Time_of_order) & Time_of_order > Median_Patience, 1, ifelse(!is.na(Time_of_order) & Time_of_order <= Median_Patience, 0, NA)))



#Aggression________________________________________________________________________________________________________________________________________________________________________



#Aggression defined by the absolute value of bid - valuation in realtion to the valuation

#Creating the "First Bid" column


first_bid <- Data %>% group_by(id, round) %>% summarise(First_Bid = first(bid, order_by = time), .groups = 'drop')
Data <- Data %>% left_join(first_bid, by = c("id", "round"))


#only for considering done deals
Data <- Data %>%
  mutate(First_Bid = ifelse(is.na(match_id), NA, First_Bid))


#Calculating the Aggression percentage value

Data <- Data %>%
  mutate(Percentage_Aggression = round(((abs(First_Bid - valuation)) / valuation)*100, 2))


#Medians

Median_Aggression <- median(Data$Percentage_Aggression, na.rm = TRUE)



#From an older version but useful for analysis
Match_price <- Data %>% filter(!is.na(match_id)) %>% select(id, round,bid) %>% rename(Match_price = bid)
Data<- Data %>% left_join(Match_price, by = c("id", "round"))



#Creation of the final binary variable "Aggression" 

Data <- Data %>%
  mutate(Aggression = ifelse(Percentage_Aggression > Median_Aggression, 1, ifelse(Percentage_Aggression <= Median_Aggression, 0, NA)))



#Gain of Profit__________________________________________________________________________________________________________________________________________________________________________


#Creating a binary variable based on if a deal was sealed in order to conduct logistic regressions


#Median

Median_GOP <- median(Data$GOP, na.rm = TRUE)


#Binary Variable


Data <- Data %>%
  mutate(GOP_B = ifelse(!is.na(GOP) & GOP > Median_GOP, 1, ifelse(!is.na(GOP) & GOP <= Median_GOP, 0, NA)))


#Strategies________________________________________________________________________________________________________________________________________________________________________


#Quick and Aggressive; Patience = 0; Aggression = 1


Data <- Data %>%
  mutate(Quick_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 0, 1, 0)))

#Slow and Aggressive; Patience = 1; Aggression = 1


Data <- Data %>%
  mutate(Slow_and_Aggressive = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 1 & Patience == 1, 1, 0)))

#Slow and Soft; Patience = 1; Aggression = 0


Data <- Data %>%
  mutate(Slow_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 1, 1, 0)))

#Quick and Soft; Patience = 0; Aggression = 0


Data <- Data %>%
  mutate(Quick_and_soft = ifelse(is.na(Aggression) | is.na(Patience), NA, ifelse(Aggression == 0 & Patience == 0, 1, 0)))




#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#GoP ~ Quick and aggressive


#Choosing the data set

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_Q_A <- glm(GOP_B ~ Quick_and_Aggressive, data = Data, family = binomial)
summary(model_Q_A)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_Q_A, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive  ",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#GoP ~ Slow and aggressive_______________________________________________________________________________________________________________________________________________________


#Choosing the data set
Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_S_A <- glm(GOP_B ~ Slow_and_Aggressive, data = Data, family = binomial)
summary(model_S_A)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_S_A, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive  ",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Slow and soft_____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_S_S <- glm(GOP_B ~ Slow_and_soft, data = Data, family = binomial)
summary(model_S_S)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_S_S, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft  ",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Quick and soft____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression <- Data[!is.na(Data$GOP_B), ]

#Creating a Model

model_Q_S <- glm(GOP_B ~ Quick_and_soft, data = Data, family = binomial)
summary(model_Q_S)



# Create a data frame with predictions
Data_for_Regression$predicted_prob <- predict(model_Q_S, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft  ",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()






################################Section :Clean Up ###########################################








dev.off()

sink("path")

summary(model_Q_A)

summary(model_Q_S)

summary(model_S_S)

summary(model_S_A)


sink()


