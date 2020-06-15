options(max.print=999999)

#Create main data frame
vp <- data.frame(x.V8=numeric(), x.V1=numeric(), y=raw())

#Load existing data
#vp <- read.csv("C:/Users/Paul/OneDrive - University of Leeds/Side Projects/SVM Prehension/Results/P01_vp.csv")
#plot(vp$x.V1, vp$x.V8, col=vp$y)

# The dataset subfolders are organised as Participant/Distance/Object/File_description/Trial.extension 

#Import 3 trials per DISTANCE
file_directory = "C:/Users/mnpdeb/SVMPrehension"
#p_code <- readline(prompt = "Enter participant code (e.g. /P01): ")
p_code = "/P05/"
file_description = "/differentiated_filtered_C#"
file_extension = ".P05.csv"

for (i in 1:3)
{
  dist_code <- readline(prompt = "Enter distance folder (e.g. Near/Medium/Far): ")
  for (i in 1:10)
  {
    obj_code <- readline(prompt = "Enter object code (e.g. /0): ")
    for (i in 1:9)
    {
      trial_sample <- readline(prompt = "Enter trial code (e.g. 191): ")
      file_name <- paste(file_directory, p_code, dist_code, obj_code, file_description, trial_sample, file_extension, sep="")
      vp_import <- read.csv(file = file_name, header = FALSE)[ ,c(8, 1)]
      
      # set x and y axis
      y = rep(dist_code, nrow(vp_import))
      vp_data <- data.frame(x = vp_import, y = y)
      vp <- rbind(vp, vp_data)
      plot(vp$x.V1, vp$x.V8, col=vp$y)
    }
  }
}

write.table(vp, file = "P05_vp.csv", sep = ",")

# Train SVM algorithm via e1071. 
library("e1071")
svmfit <- svm(y ~., data = vp, kernel = "radial", cost = 100, scale = TRUE)
summary(svmfit)
plot(svmfit, vp)

# 10-fold Cross Validation
tuned <- tune(svm, y ~., data = vp, kernel = "radial", ranges = list(cost=c(0.001,0.01,.1,1,10,100)))
summary(tuned) 
best_model = tuned$best.model
summary(best_model) # Will show the optimal cost parameter




# Predict new data
new_file = readline(prompt = "Enter complete directory and filename: ")
dist_new = readline(prompt = "What distance? (Near/Medium/Far): ")

vp_new <- read.csv(file = new_file, header = FALSE, skip = 1)[ ,c(8, 1)]
y_new = rep(dist_new, nrow(vp_new))
vp_test <- data.frame(x = vp_new, y = y_new)

ypred = predict(best_model, vp_test)
table(predict = ypred, truth = vp_test$y)
