#Reading the Data set

Data <- read.csv("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Data Set\\Bachelorarbeit\\Data.csv")

#Download the necessary libraries

library(tidyverse)

#############################################################################################

###############################Section: Data Set Preparation################################# 

#############################################################################################

pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Finalstages\\Plots_Stage_5_BB.pdf")



#Logistic Regressions______________________________________________________________________________________________________________________________________________________________


#GoP ~ Quick and aggressive


#Choosing the data set

Data_for_Regression_Seller_bad_BB <- Data_Seller_bad_BB[!is.na(Data_Seller_bad_BB$GOP_B), ]

#Creating a Model

model_Q_A_Seller_bad_BB <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_bad_BB, family = binomial)
summary(model_Q_A_Seller_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_BB$predicted_prob <- predict(model_Q_A_Seller_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_BB, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller bad_BB",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_BB <- Data_Seller_Good_BB[!is.na(Data_Seller_Good_BB$GOP_B), ]

#Creating a Model

model_Q_A_Seller_Good_BB <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Seller_Good_BB, family = binomial)
summary(model_Q_A_Seller_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_BB$predicted_prob <- predict(model_Q_A_Seller_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_BB, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Seller Good_BB",
                                                                                                                                                                                                           x = "Quick and Aggressive ",
                                                                                                                                                                                                           y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_BB <- Data_Buyer_bad_BB[!is.na(Data_Buyer_bad_BB$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_bad_BB <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_bad_BB, family = binomial)
summary(model_Q_A_Buyer_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_BB$predicted_prob <- predict(model_Q_A_Buyer_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_BB, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Aggressive Buyer bad_BB",
                                                                                                                                                                                                         x = "Quick and Aggressive ",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_BB <- Data_Buyer_Good_BB[!is.na(Data_Buyer_Good_BB$GOP_B), ]

#Creating a Model

model_Q_A_Buyer_Good_BB <- glm(GOP_B ~ Quick_and_Aggressive, data = Data_Buyer_Good_BB, family = binomial)
summary(model_Q_A_Buyer_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_BB$predicted_prob <- predict(model_Q_A_Buyer_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_BB, aes(x = Quick_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good_BB",
                                                                                                                                                                                                          x = "Quick and Aggressive ",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#GoP ~ Slow and aggressive_______________________________________________________________________________________________________________________________________________________


#Choosing the data set

Data_for_Regression_Seller_bad_BB <- Data_Seller_bad_BB[!is.na(Data_Seller_bad_BB$GOP_B), ]

#Creating a Model

model_S_A_Seller_bad_BB <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_bad_BB, family = binomial)
summary(model_S_A_Seller_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_BB$predicted_prob <- predict(model_S_A_Seller_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_BB, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller bad_BB",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_BB <- Data_Seller_Good_BB[!is.na(Data_Seller_Good_BB$GOP_B), ]

#Creating a Model

model_S_A_Seller_Good_BB <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Seller_Good_BB, family = binomial)
summary(model_S_A_Seller_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_BB$predicted_prob <- predict(model_S_A_Seller_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_BB, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Seller Good_BB",
                                                                                                                                                                                                          x = "Slow and Aggressive",
                                                                                                                                                                                                          y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_BB <- Data_Buyer_bad_BB[!is.na(Data_Buyer_bad_BB$GOP_B), ]

#Creating a Model

model_S_A_Buyer_bad_BB <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_bad_BB, family = binomial)
summary(model_S_A_Buyer_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_BB$predicted_prob <- predict(model_S_A_Buyer_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_BB, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer bad_BB",
                                                                                                                                                                                                        x = "Slow and Aggressive",
                                                                                                                                                                                                        y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_BB <- Data_Buyer_Good_BB[!is.na(Data_Buyer_Good_BB$GOP_B), ]

#Creating a Model

model_S_A_Buyer_Good_BB <- glm(GOP_B ~ Slow_and_Aggressive, data = Data_Buyer_Good_BB, family = binomial)
summary(model_S_A_Buyer_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_BB$predicted_prob <- predict(model_S_A_Buyer_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_BB, aes(x = Slow_and_Aggressive, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Aggressive Buyer Good_BB",
                                                                                                                                                                                                         x = "Slow and Aggressive",
                                                                                                                                                                                                         y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()





#GoP ~ Slow and soft_____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad_BB <- Data_Seller_bad_BB[!is.na(Data_Seller_bad_BB$GOP_B), ]

#Creating a Model

model_S_S_Seller_bad_BB <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_bad_BB, family = binomial)
summary(model_S_S_Seller_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_BB$predicted_prob <- predict(model_S_S_Seller_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_BB, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller bad_BB",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_BB <- Data_Seller_Good_BB[!is.na(Data_Seller_Good_BB$GOP_B), ]

#Creating a Model

model_S_S_Seller_Good_BB <- glm(GOP_B ~ Slow_and_soft, data = Data_Seller_Good_BB, family = binomial)
summary(model_S_S_Seller_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_BB$predicted_prob <- predict(model_S_S_Seller_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_BB, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Seller Good_BB",
                                                                                                                                                                                                    x = "Slow and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_BB <- Data_Buyer_bad_BB[!is.na(Data_Buyer_bad_BB$GOP_B), ]

#Creating a Model

model_S_S_Buyer_bad_BB <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_bad_BB, family = binomial)
summary(model_S_S_Buyer_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_BB$predicted_prob <- predict(model_S_S_Buyer_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_BB, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer bad_BB",
                                                                                                                                                                                                  x = "Slow and Soft",
                                                                                                                                                                                                  y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_BB <- Data_Buyer_Good_BB[!is.na(Data_Buyer_Good_BB$GOP_B), ]

#Creating a Model

model_S_S_Buyer_Good_BB <- glm(GOP_B ~ Slow_and_soft, data = Data_Buyer_Good_BB, family = binomial)
summary(model_S_S_Buyer_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_BB$predicted_prob <- predict(model_S_S_Buyer_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_BB, aes(x = Slow_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Slow and Soft Buyer Good_BB",
                                                                                                                                                                                                   x = "Slow and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()



#GoP ~ Quick and soft____________________________________________________________________________________________________________________________________________________________



#Choosing the data set

Data_for_Regression_Seller_bad_BB <- Data_Seller_bad_BB[!is.na(Data_Seller_bad_BB$GOP_B), ]

#Creating a Model

model_Q_S_Seller_bad_BB <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_bad_BB, family = binomial)
summary(model_Q_S_Seller_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_bad_BB$predicted_prob <- predict(model_Q_S_Seller_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_bad_BB, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller bad_BB",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()




#Choosing the data set

Data_for_Regression_Seller_Good_BB <- Data_Seller_Good_BB[!is.na(Data_Seller_Good_BB$GOP_B), ]

#Creating a Model

model_Q_S_Seller_Good_BB <- glm(GOP_B ~ Quick_and_soft, data = Data_Seller_Good_BB, family = binomial)
summary(model_Q_S_Seller_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Seller_Good_BB$predicted_prob <- predict(model_Q_S_Seller_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Seller_Good_BB, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Seller Good_BB",
                                                                                                                                                                                                     x = "Quick and Soft",
                                                                                                                                                                                                     y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_bad_BB <- Data_Buyer_bad_BB[!is.na(Data_Buyer_bad_BB$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_bad_BB <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_bad_BB, family = binomial)
summary(model_Q_S_Buyer_bad_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_bad_BB$predicted_prob <- predict(model_Q_S_Buyer_bad_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_bad_BB, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer bad_BB",
                                                                                                                                                                                                   x = "Quick and Soft",
                                                                                                                                                                                                   y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()


#Choosing the data set

Data_for_Regression_Buyer_Good_BB <- Data_Buyer_Good_BB[!is.na(Data_Buyer_Good_BB$GOP_B), ]

#Creating a Model

model_Q_S_Buyer_Good_BB <- glm(GOP_B ~ Quick_and_soft, data = Data_Buyer_Good_BB, family = binomial)
summary(model_Q_S_Buyer_Good_BB)



# Create a data frame with predictions
Data_for_Regression_Buyer_Good_BB$predicted_prob <- predict(model_Q_S_Buyer_Good_BB, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_Buyer_Good_BB, aes(x = Quick_and_soft, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Quick and Soft Buyer Good_BB",
                                                                                                                                                                                                    x = "Quick and Soft",
                                                                                                                                                                                                    y = "Probability of GOP_B") + scale_color_discrete(name = "GOP") + theme_minimal()










################################Section :Clean Up ###########################################





dev.off()

sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\model_summaries_Stage_5_BB.txt")

summary(model_Q_A_Buyer_bad_BB)
summary(model_Q_A_Buyer_Good_BB)
summary(model_Q_A_Seller_bad_BB)
summary(model_Q_A_Seller_Good_BB)

summary(model_Q_S_Buyer_bad_BB)
summary(model_Q_S_Buyer_Good_BB)
summary(model_Q_S_Seller_bad_BB)
summary(model_Q_S_Seller_Good_BB)

summary(model_S_S_Buyer_bad_BB)
summary(model_S_S_Buyer_Good_BB)
summary(model_S_S_Seller_bad_BB)
summary(model_S_S_Seller_Good_BB)

summary(model_S_A_Buyer_bad_BB)
summary(model_S_A_Buyer_Good_BB)
summary(model_S_A_Seller_bad_BB)
summary(model_S_A_Seller_Good_BB)

sink()


