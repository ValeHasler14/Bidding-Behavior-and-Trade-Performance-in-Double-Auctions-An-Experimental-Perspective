#############################################################################################

###############################Section: Logistic Regressions################################# 

#############################################################################################

#Same C1-C8


pdf("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Plots PDF\\Plots_SPlit_4_Full_Same.pdf")

##First Test for Patience C1

Data_for_Regression_C1 <- Data_C1_Same[!is.na(Data_C1_Same$GOP_B), ]

#Creating a Model

model_Patience_C1 <- glm(GOP_B ~ Patience, data = Data_C1_Same, family = binomial)
summary(model_Patience_C1)



# Create a data frame with predictions
Data_for_Regression_C1$predicted_prob <- predict(model_Patience_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C1_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

################

##First Test for Patience C2


Data_for_Regression_C2 <- Data_C2_Same[!is.na(Data_C2_Same$GOP_B), ]

#Creating a Model

model_Patience_C2 <- glm(GOP_B ~ Patience, data = Data_C2_Same, family = binomial)
summary(model_Patience_C2)



# Create a data frame with predictions
Data_for_Regression_C2$predicted_prob <- predict(model_Patience_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C2_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


################

##First Test for Patience C3


Data_for_Regression_C3 <- Data_C3_Same[!is.na(Data_C3_Same$GOP_B), ]

#Creating a Model

model_Patience_C3 <- glm(GOP_B ~ Patience, data = Data_C3_Same, family = binomial)
summary(model_Patience_C3)



# Create a data frame with predictions
Data_for_Regression_C3$predicted_prob <- predict(model_Patience_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C3_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


################

##First Test for Patience C4


Data_for_Regression_C4 <- Data_C4_Same[!is.na(Data_C4_Same$GOP_B), ]

#Creating a Model

model_Patience_C4 <- glm(GOP_B ~ Patience, data = Data_C4_Same, family = binomial)
summary(model_Patience_C4)



# Create a data frame with predictions
Data_for_Regression_C4$predicted_prob <- predict(model_Patience_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C4_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


################

##First Test for Patience C5


Data_for_Regression_C5 <- Data_C5_Same[!is.na(Data_C5_Same$GOP_B), ]

#Creating a Model

model_Patience_C5 <- glm(GOP_B ~ Patience, data = Data_C5_Same, family = binomial)
summary(model_Patience_C5)



# Create a data frame with predictions
Data_for_Regression_C5$predicted_prob <- predict(model_Patience_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C5_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()
################

##First Test for Patience C6


Data_for_Regression_C6 <- Data_C6_Same[!is.na(Data_C6_Same$GOP_B), ]

#Creating a Model

model_Patience_C6 <- glm(GOP_B ~ Patience, data = Data_C6_Same, family = binomial)
summary(model_Patience_C6)



# Create a data frame with predictions
Data_for_Regression_C6$predicted_prob <- predict(model_Patience_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C6_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


##First Test for Patience C7


Data_for_Regression_C7 <- Data_C7_Same[!is.na(Data_C7_Same$GOP_B), ]

#Creating a Model

model_Patience_C7 <- glm(GOP_B ~ Patience, data = Data_C7_Same, family = binomial)
summary(model_Patience_C7)



# Create a data frame with predictions
Data_for_Regression_C7$predicted_prob <- predict(model_Patience_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C7_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

##First Test for Patience C8


Data_for_Regression_C8 <- Data_C8_Same[!is.na(Data_C8_Same$GOP_B), ]

#Creating a Model

model_Patience_C8 <- glm(GOP_B ~ Patience, data = Data_C8_Same, family = binomial)
summary(model_Patience_C8)



# Create a data frame with predictions
Data_for_Regression_C8$predicted_prob <- predict(model_Patience_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Patience, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) + geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Patience (C8_Same)",
                                                                                                                                                                                      x = "Patience",
                                                                                                                                                                                      y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#############################################################################################

#First Test for Activeness_C1

Data_for_Regression_C1 <- Data_C1_Same[!is.na(Data_C1_Same$GOP_B), ]


#Creating a Model

model_Activeness_C1 <- glm(GOP_B ~ Activeness, data = Data_C1_Same, family = binomial)
summary(model_Activeness_C1)



# Create a data frame with predictions
Data_for_Regression_C1$predicted_prob <- predict(model_Activeness_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness(C1_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

##########

#First Test for Activeness_C2

Data_for_Regression_C2 <- Data_C2_Same[!is.na(Data_C2_Same$GOP_B), ]


#Creating a Model

model_Activeness_C2 <- glm(GOP_B ~ Activeness, data = Data_C2_Same, family = binomial)
summary(model_Activeness_C2)


# Create a data frame with predictions
Data_for_Regression_C2$predicted_prob <- predict(model_Activeness_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C2_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


##########

#First Test for Activeness_C3

Data_for_Regression_C3 <- Data_C3_Same[!is.na(Data_C3_Same$GOP_B), ]


#Creating a Model

model_Activeness_C3 <- glm(GOP_B ~ Activeness, data = Data_C3_Same, family = binomial)
summary(model_Activeness_C3)


# Create a data frame with predictions
Data_for_Regression_C3$predicted_prob <- predict(model_Activeness_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C3_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()





##########

#First Test for Activeness_C4

Data_for_Regression_C4 <- Data_C4_Same[!is.na(Data_C4_Same$GOP_B), ]


#Creating a Model

model_Activeness_C4 <- glm(GOP_B ~ Activeness, data = Data_C4_Same, family = binomial)
summary(model_Activeness_C4)


# Create a data frame with predictions
Data_for_Regression_C4$predicted_prob <- predict(model_Activeness_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C4_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


##########

#First Test for Activeness_C5

Data_for_Regression_C5 <- Data_C5_Same[!is.na(Data_C5_Same$GOP_B), ]


#Creating a Model

model_Activeness_C5 <- glm(GOP_B ~ Activeness, data = Data_C5_Same, family = binomial)
summary(model_Activeness_C5)


# Create a data frame with predictions
Data_for_Regression_C5$predicted_prob <- predict(model_Activeness_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C5_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



##########

#First Test for Activeness_C6

Data_for_Regression_C6 <- Data_C6_Same[!is.na(Data_C6_Same$GOP_B), ]


#Creating a Model

model_Activeness_C6 <- glm(GOP_B ~ Activeness, data = Data_C6_Same, family = binomial)
summary(model_Activeness_C6)


# Create a data frame with predictions
Data_for_Regression_C6$predicted_prob <- predict(model_Activeness_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C6_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()


##########

#First Test for Activeness_C7

Data_for_Regression_C7 <- Data_C7_Same[!is.na(Data_C7_Same$GOP_B), ]


#Creating a Model

model_Activeness_C7 <- glm(GOP_B ~ Activeness, data = Data_C7_Same, family = binomial)
summary(model_Activeness_C7)


# Create a data frame with predictions
Data_for_Regression_C7$predicted_prob <- predict(model_Activeness_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C7_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

##########

#First Test for Activeness_C8

Data_for_Regression_C8 <- Data_C8_Same[!is.na(Data_C8_Same$GOP_B), ]


#Creating a Model

model_Activeness_C8 <- glm(GOP_B ~ Activeness, data = Data_C8_Same, family = binomial)
summary(model_Activeness_C8)


# Create a data frame with predictions
Data_for_Regression_C8$predicted_prob <- predict(model_Activeness_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Activeness, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +   geom_line(aes(y = predicted_prob), color = "blue", size = 1) + labs(title = "Logistic Regression of GOP on Activeness (C8_Same)",
                                                                                                                                                                                          x = "Activeness",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()





#############################################################################################



#First Test for Flexible C1

Data_for_Regression_C1 <- Data_C1_Same[!is.na(Data_C1_Same$GOP_B), ]

#Creating a Model

model_Flexible_C1 <- glm(GOP_B ~ Flexible, data = Data_C1_Same, family = binomial)
summary(model_Flexible_C1)



# Create a data frame with predictions

Data_for_Regression_C1$predicted_prob <- predict(model_Flexible_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C1_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#############


#First Test for Flexible C2

Data_for_Regression_C2 <- Data_C2_Same[!is.na(Data_C2_Same$GOP_B), ]

#Creating a Model

model_Flexible_C2 <- glm(GOP_B ~ Flexible, data = Data_C2_Same, family = binomial)
summary(model_Flexible_C2)



# Create a data frame with predictions

Data_for_Regression_C2$predicted_prob <- predict(model_Flexible_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C2_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()



#############


#First Test for Flexible C3

Data_for_Regression_C3 <- Data_C3_Same[!is.na(Data_C3_Same$GOP_B), ]

#Creating a Model

model_Flexible_C3 <- glm(GOP_B ~ Flexible, data = Data_C3_Same, family = binomial)
summary(model_Flexible_C3)



# Create a data frame with predictions

Data_for_Regression_C3$predicted_prob <- predict(model_Flexible_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C3_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#############


#First Test for Flexible C4

Data_for_Regression_C4 <- Data_C4_Same[!is.na(Data_C4_Same$GOP_B), ]

#Creating a Model

model_Flexible_C4 <- glm(GOP_B ~ Flexible, data = Data_C4_Same, family = binomial)
summary(model_Flexible_C4)



# Create a data frame with predictions

Data_for_Regression_C4$predicted_prob <- predict(model_Flexible_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C4_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()




#First Test for Flexible C5

Data_for_Regression_C5 <- Data_C5_Same[!is.na(Data_C5_Same$GOP_B), ]

#Creating a Model

model_Flexible_C5 <- glm(GOP_B ~ Flexible, data = Data_C5_Same, family = binomial)
summary(model_Flexible_C5)



# Create a data frame with predictions

Data_for_Regression_C5$predicted_prob <- predict(model_Flexible_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C5_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#First Test for Flexible C6

Data_for_Regression_C6 <- Data_C6_Same[!is.na(Data_C6_Same$GOP_B), ]

#Creating a Model

model_Flexible_C6 <- glm(GOP_B ~ Flexible, data = Data_C6_Same, family = binomial)
summary(model_Flexible_C6)



# Create a data frame with predictions

Data_for_Regression_C6$predicted_prob <- predict(model_Flexible_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C6_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()

#First Test for Flexible C7

Data_for_Regression_C7 <- Data_C7_Same[!is.na(Data_C7_Same$GOP_B), ]

#Creating a Model

model_Flexible_C7 <- glm(GOP_B ~ Flexible, data = Data_C7_Same, family = binomial)
summary(model_Flexible_C7)



# Create a data frame with predictions

Data_for_Regression_C7$predicted_prob <- predict(model_Flexible_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C7_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()
#First Test for Flexible C8

Data_for_Regression_C8 <- Data_C8_Same[!is.na(Data_C8_Same$GOP_B), ]

#Creating a Model

model_Flexible_C8 <- glm(GOP_B ~ Flexible, data = Data_C8_Same, family = binomial)
summary(model_Flexible_C8)



# Create a data frame with predictions

Data_for_Regression_C8$predicted_prob <- predict(model_Flexible_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Flexible, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Flexible (C8_Same)",
                                                                                                                                                                                        x = "Flexible",
                                                                                                                                                                                        y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                             








#########################################                                                                                                                                                                                       

#First Test for Aggression C1

Data_for_Regression_C1 <- Data_C1_Same[!is.na(Data_C1_Same$GOP_B), ]

#Creating a Model

model_Aggression_C1 <- glm(GOP_B ~ Aggression, data = Data_C1_Same, family = binomial)
summary(model_Aggression_C1)



# Create a data frame with predictions

Data_for_Regression_C1$predicted_prob <- predict(model_Aggression_C1, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C1, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C1_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
#

#First Test for Aggression C2

Data_for_Regression_C2 <- Data_C2_Same[!is.na(Data_C2_Same$GOP_B), ]

#Creating a Model

model_Aggression_C2 <- glm(GOP_B ~ Aggression, data = Data_C2_Same, family = binomial)
summary(model_Aggression_C2)



# Create a data frame with predictions

Data_for_Regression_C2$predicted_prob <- predict(model_Aggression_C2, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C2, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C2_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       
#First Test for Aggression C3

Data_for_Regression_C3 <- Data_C3_Same[!is.na(Data_C3_Same$GOP_B), ]

#Creating a Model

model_Aggression_C3 <- glm(GOP_B ~ Aggression, data = Data_C3_Same, family = binomial)
summary(model_Aggression_C3)



# Create a data frame with predictions

Data_for_Regression_C3$predicted_prob <- predict(model_Aggression_C3, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C3, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C3_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                                                                                                                                                                                                       


#First Test for Aggression C4

Data_for_Regression_C4 <- Data_C4_Same[!is.na(Data_C4_Same$GOP_B), ]

#Creating a Model

model_Aggression_C4 <- glm(GOP_B ~ Aggression, data = Data_C4_Same, family = binomial)
summary(model_Aggression_C4)



# Create a data frame with predictions

Data_for_Regression_C4$predicted_prob <- predict(model_Aggression_C4, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C4, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C4_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       

#First Test for Aggression C5

Data_for_Regression_C5 <- Data_C5_Same[!is.na(Data_C5_Same$GOP_B), ]

#Creating a Model

model_Aggression_C5 <- glm(GOP_B ~ Aggression, data = Data_C5_Same, family = binomial)
summary(model_Aggression_C5)



# Create a data frame with predictions

Data_for_Regression_C5$predicted_prob <- predict(model_Aggression_C5, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C5, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C5_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       

#First Test for Aggression C6

Data_for_Regression_C6 <- Data_C6_Same[!is.na(Data_C6_Same$GOP_B), ]

#Creating a Model

model_Aggression_C6 <- glm(GOP_B ~ Aggression, data = Data_C6_Same, family = binomial)
summary(model_Aggression_C6)



# Create a data frame with predictions

Data_for_Regression_C6$predicted_prob <- predict(model_Aggression_C6, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C6, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C6_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       



#First Test for Aggression C7

Data_for_Regression_C7 <- Data_C7_Same[!is.na(Data_C7_Same$GOP_B), ]

#Creating a Model

model_Aggression_C7 <- glm(GOP_B ~ Aggression, data = Data_C7_Same, family = binomial)
summary(model_Aggression_C7)



# Create a data frame with predictions

Data_for_Regression_C7$predicted_prob <- predict(model_Aggression_C7, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C7, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C7_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       






#First Test for Aggression C8

Data_for_Regression_C8 <- Data_C8_Same[!is.na(Data_C8_Same$GOP_B), ]

#Creating a Model

model_Aggression_C8 <- glm(GOP_B ~ Aggression, data = Data_C8_Same, family = binomial)
summary(model_Aggression_C8)



# Create a data frame with predictions

Data_for_Regression_C8$predicted_prob <- predict(model_Aggression_C8, type = "response")

# Plot the data points and the logistic regression curve
ggplot(Data_for_Regression_C8, aes(x = Aggression, y = GOP_B)) + geom_point(aes(color = factor(GOP_B)), size = 3) +  geom_line(aes(y = predicted_prob), color = "blue", size = 1) +  labs(title = "Logistic Regression of GOP on Aggression (C8_Same)",
                                                                                                                                                                                          x = "Aggression",
                                                                                                                                                                                          y = "Probability of GOP") + scale_color_discrete(name = "GOP") + theme_minimal()                                       



dev.off()

#############################################################################################

################################Section :Clean Up ###########################################


sink("C:\\Users\\info\\OneDrive\\Valentin\\Uni\\BA\\Summaries\\Split 4\\model_summaries_Split_4_Same.txt")


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