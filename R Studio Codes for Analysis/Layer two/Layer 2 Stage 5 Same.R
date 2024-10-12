#Reading the Data set

Data <- read.csv("path")

#Download the necessary libraries

library(tidyverse)

#creating the PDF
pdf("path")



#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#GoP ~ Quick and aggressive

#Choosing the data set

Data_for_Regression_Seller_bad_Same <- Data_Seller_bad_Same[!is.na(Data_Seller_bad_Same$GOP_B), ]

#Creating a Model

model_Q_A_Seller_bad_Same <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_bad_Same, family = binomial)
summary(model_Q_A_Seller_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_Same$predicted_prob <- predict(model_Q_A_Seller_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_Same, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller bad_Same",
                                                                                                                                                                                                                x = "Quick and Aggressive ",
                                                                                                                                                                                                                y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_Same <- Data_Seller_Good_Same[!is.na(Data_Seller_Good_Same$GOP_B), ]

#Creating a Model

model_Q_A_Seller_Good_Same <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_Good_Same, family = binomial)
summary(model_Q_A_Seller_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_Same$predicted_prob <- predict(model_Q_A_Seller_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_Same, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller Good_Same",
                                                                                                                                                                                                                 x = "Quick and Aggressive ",
                                                                                                                                                                                                                 y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_Same <- Data_Buyer_bad_Same[!is.na(Data_Buyer_bad_Same$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_bad_Same <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_bad_Same, family = binomial)
summary(model_Q_A_Buyer_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_Same$predicted_prob <- predict(model_Q_A_Buyer_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_Same, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Buyer bad_Same",
                                                                                                                                                                                                               x = "Quick and Aggressive ",
                                                                                                                                                                                                               y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_Same <- Data_Buyer_Good_Same[!is.na(Data_Buyer_Good_Same$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_Good_Same <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_Good_Same, family = binomial)
summary(model_Q_A_Buyer_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_Same$predicted_prob <- predict(model_Q_A_Buyer_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_Same, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good_Same",
                                                                                                                                                                                                                x = "Quick and Aggressive ",
                                                                                                                                                                                                                y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#GoP ~ Slow and aggressive_______________________________________________________________________________________________________________________________________________________


#Choosing the data set

Data_for_Regression_Seller_bad_Same <- Data_Seller_bad_Same[!is.na(Data_Seller_bad_Same$GOP_B), ]

#Creating a Model

model_S_A_Seller_bad_Same <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_bad_Same, family = binomial)
summary(model_S_A_Seller_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_Same$predicted_prob <- predict(model_S_A_Seller_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_Same, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller bad_Same",
                                                                                                                                                                                                               x = "Slow and Aggressive",
                                                                                                                                                                                                               y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_Same <- Data_Seller_Good_Same[!is.na(Data_Seller_Good_Same$GOP_B), ]

#Creating a Model

model_S_A_Seller_Good_Same <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_Good_Same, family = binomial)
summary(model_S_A_Seller_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_Same$predicted_prob <- predict(model_S_A_Seller_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_Same, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller Good_Same",
                                                                                                                                                                                                                x = "Slow and Aggressive",
                                                                                                                                                                                                                y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_Same <- Data_Buyer_bad_Same[!is.na(Data_Buyer_bad_Same$GOP_B), ]

#Creating a Model

model_S_A_Buyer_bad_Same <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_bad_Same, family = binomial)
summary(model_S_A_Buyer_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_Same$predicted_prob <- predict(model_S_A_Buyer_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_Same, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer bad_Same",
                                                                                                                                                                                                              x = "Slow and Aggressive",
                                                                                                                                                                                                              y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_Same <- Data_Buyer_Good_Same[!is.na(Data_Buyer_Good_Same$GOP_B), ]

#Creating a Model

model_S_A_Buyer_Good_Same <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_Good_Same, family = binomial)
summary(model_S_A_Buyer_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_Same$predicted_prob <- predict(model_S_A_Buyer_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_Same, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good_Same",
                                                                                                                                                                                                               x = "Slow and Aggressive",
                                                                                                                                                                                                               y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Slow and soft_____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad_Same <- Data_Seller_bad_Same[!is.na(Data_Seller_bad_Same$GOP_B), ]

#Creating a Model

model_S_S_Seller_bad_Same <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_bad_Same, family = binomial)
summary(model_S_S_Seller_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_Same$predicted_prob <- predict(model_S_S_Seller_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_Same, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller bad_Same",
                                                                                                                                                                                                         x = "Slow and Soft",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_Same <- Data_Seller_Good_Same[!is.na(Data_Seller_Good_Same$GOP_B), ]

#Creating a Model

model_S_S_Seller_Good_Same <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_Good_Same, family = binomial)
summary(model_S_S_Seller_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_Same$predicted_prob <- predict(model_S_S_Seller_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_Same, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller Good_Same",
                                                                                                                                                                                                          x = "Slow and Soft",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_Same <- Data_Buyer_bad_Same[!is.na(Data_Buyer_bad_Same$GOP_B), ]

#Creating a Model

model_S_S_Buyer_bad_Same <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_bad_Same, family = binomial)
summary(model_S_S_Buyer_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_Same$predicted_prob <- predict(model_S_S_Buyer_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_Same, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer bad_Same",
                                                                                                                                                                                                        x = "Slow and Soft",
                                                                                                                                                                                                        y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_Same <- Data_Buyer_Good_Same[!is.na(Data_Buyer_Good_Same$GOP_B), ]

#Creating a Model

model_S_S_Buyer_Good_Same <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_Good_Same, family = binomial)
summary(model_S_S_Buyer_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_Same$predicted_prob <- predict(model_S_S_Buyer_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_Same, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer Good_Same",
                                                                                                                                                                                                         x = "Slow and Soft",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Quick and soft____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad_Same <- Data_Seller_bad_Same[!is.na(Data_Seller_bad_Same$GOP_B), ]

#Creating a Model

model_Q_S_Seller_bad_Same <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_bad_Same, family = binomial)
summary(model_Q_S_Seller_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_Same$predicted_prob <- predict(model_Q_S_Seller_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_Same, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller bad_Same",
                                                                                                                                                                                                          x = "Quick and Soft",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_Same <- Data_Seller_Good_Same[!is.na(Data_Seller_Good_Same$GOP_B), ]

#Creating a Model

model_Q_S_Seller_Good_Same <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_Good_Same, family = binomial)
summary(model_Q_S_Seller_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_Same$predicted_prob <- predict(model_Q_S_Seller_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_Same, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller Good_Same",
                                                                                                                                                                                                           x = "Quick and Soft",
                                                                                                                                                                                                           y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_Same <- Data_Buyer_bad_Same[!is.na(Data_Buyer_bad_Same$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_bad_Same <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_bad_Same, family = binomial)
summary(model_Q_S_Buyer_bad_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_Same$predicted_prob <- predict(model_Q_S_Buyer_bad_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_Same, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer bad_Same",
                                                                                                                                                                                                         x = "Quick and Soft",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_Same <- Data_Buyer_Good_Same[!is.na(Data_Buyer_Good_Same$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_Good_Same <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_Good_Same, family = binomial)
summary(model_Q_S_Buyer_Good_Same)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_Same$predicted_prob <- predict(model_Q_S_Buyer_Good_Same, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_Same, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer Good_Same",
                                                                                                                                                                                                          x = "Quick and Soft",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()










################################Section :Clean Up ###########################################






dev.off()

sink("path")

summary(model_Q_A_Buyer_bad_Same)
summary(model_Q_A_Buyer_Good_Same)
summary(model_Q_A_Seller_bad_Same)
summary(model_Q_A_Seller_Good_Same)

summary(model_Q_S_Buyer_bad_Same)
summary(model_Q_S_Buyer_Good_Same)
summary(model_Q_S_Seller_bad_Same)
summary(model_Q_S_Seller_Good_Same)

summary(model_S_S_Buyer_bad_Same)
summary(model_S_S_Buyer_Good_Same)
summary(model_S_S_Seller_bad_Same)
summary(model_S_S_Seller_Good_Same)

summary(model_S_A_Buyer_bad_Same)
summary(model_S_A_Buyer_Good_Same)
summary(model_S_A_Seller_bad_Same)
summary(model_S_A_Seller_Good_Same)

sink()


