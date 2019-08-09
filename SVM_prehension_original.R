library(ggplot2)
library(dplyr)
options(max.print=999999)

#Create main data frame
vp <- data.frame(x.V8=numeric(), x.V1=numeric(), y=raw())

#Load existing data
vp <- read.csv("C:/Users/paclab/SVMPrehension/P08_vp.csv")
ggplot(data = vp, aes(x=vp$x.V1, y=vp$x.V8, col=vp$y))+
  geom_path() +
  ggtitle("SVM Training with Test Data") +
  xlab("Time") + ylab("Velocity") +
  labs(col = "Distance")
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

participant_predictions <- data.frame()[1:90, ] # call once per participant


# -10% FOR EACH TRIAL

#Filter by distance
dist_new = "Near"
vp_dist <- dplyr::filter(vp, y == dist_new)

# Initialisation
start_points = c(which(vp_dist$x.V1 == 0))
trial_counter = 1
last_trial = nrow(vp_dist)
trial_predictions <- data.frame(trajectory = numeric(), results_score = numeric())

#loop starts
for (i in 1:90) # change according to files
{
  #slice
  start_i = start_points[trial_counter]
  end_i = start_points[trial_counter+1] - 1
  if (is.na(end_i) == TRUE)
  {
   end_i = last_trial 
  }
  
  trial_x <- data.frame()
  trial_x <- vp_dist[start_i:end_i,]
  
  #test
  vp_new <- data.frame(V1 = trial_x$x.V1, V8 = trial_x$x.V8)
  
  #Extract Peak Maxima
  y_peak = max(vp_new$V8)
  x_peak = (which.max(vp_new$V8))
  tatpeak <- (vp_new[x_peak,])
  tatpeak = tatpeak$V1
  peak <- data.frame(tatpeak, y_peak)
  
  #Extract 10% Before Peak
  get10 = as.integer(nrow(vp_new)*0.1)
  lowerlimit = x_peak - get10
  test_vp_new = vp_new[lowerlimit:x_peak, ]
  
  #Predict 10% Before Peak
  test_vp_new <- data.frame(x.V8 = test_vp_new$V8, x.V1 = test_vp_new$V1, y=dist_new)
  ypred = predict(svmfit, test_vp_new)
  results <- data.frame(table(predict = ypred, truth = test_vp_new$y))
  results_freq <- dplyr::filter(results, truth == dist_new, predict == dist_new)
  results_score = results_freq$Freq / sum(results$Freq)*100
  results_score = round(results_score, digits = 2)
  results_score <- data.frame(trial_counter, results_score)
  
  trial_predictions <- rbind(trial_predictions, results_score)
  trial_counter = trial_counter + 1
}

names(trial_predictions)[2] = dist_new

#if exact
participant_predictions <- cbind(participant_predictions, trial_predictions)

#if not exact
blank <- data.frame(trial_counter = 0, Near = 0)[1:4,]
trial_predictions <- rbind(trial_predictions,blank)

#When complete
P_trials_test <- cbind(participant_predictions$trial_counter, participant_predictions$Far, participant_predictions$Medium, participant_predictions$Near)
column_names <- c("Trial Number", "Far", "Medium", "Near")
names(P_trials_test) <- column_names

write.table(P_trials_test, file = "P08 Individual Trajectories Prediction.csv", sep = ",")

# #Plot New Data
# ggplot(data = vp, aes(x=vp$x.V1, y=vp$x.V8, col=vp$y))+
#   geom_path() +
#   geom_line(data = vp_test, aes(x=x.V1, y=x.V8, col=y), size=1, colour="black")
#   ggtitle("SVM Training with Test Data") +
#   xlab("Time") + ylab("Velocity") +
#   labs(col = "Distance")

