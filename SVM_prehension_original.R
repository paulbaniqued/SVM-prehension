#library(ggplot2)
library(dplyr)
options(max.print=999999)

#Create main data frame
vp <- data.frame(x.V8=numeric(), x.V1=numeric(), y=raw())

#Load existing data
vp <- read.csv("C:/Users/Paul/SVM/P07_vp.csv")
# ggplot(data = vp, aes(x=vp$x.V1, y=vp$x.V8, col=vp$y))+
#   geom_path() + 
#   ggtitle("SVM Training with Test Data") +
#   xlab("Time") + ylab("Velocity") +
#   labs(col = "Distance")
#far, med, near

# Train SVM algorithm via e1071. 
library("e1071")
svmfit <- svm(y ~., data = vp, kernel = "radial", cost = 100, scale = TRUE)
summary(svmfit)
#plot(svmfit, vp)

# Predict new data
#new_file = readline(prompt = "Enter complete directory and filename: ")
#dist_new = readline(prompt = "What distance? (Near/Medium/Far): ")

ypred = predict(svmfit, vp)
table(predict = ypred, truth = vp$y)

# #Plot New Data
# ggplot(data = vp, aes(x=vp$x.V1, y=vp$x.V8, col=vp$y))+
#   geom_path() +
#   geom_line(data = vp_test, aes(x=x.V1, y=x.V8, col=y), size=1, colour="black")
#   ggtitle("SVM Training with Test Data") +
#   xlab("Time") + ylab("Velocity") +
#   labs(col = "Distance")
