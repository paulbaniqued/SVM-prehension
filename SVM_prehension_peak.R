options(max.print=999999)

#Create main data frame
vp <- data.frame(x.V8=numeric(), x.V1=numeric(), y=raw())

#Load existing data
vp <- read.csv("C:/Users/Paul/SVM/P01_vp.csv")
plot(vp$x.V1, vp$x.V8, pch = 19, col = c("orange","yellowgreen","cadetblue")[unclass(vp$y)], xlab="Time", ylab="Velocity", main="SVM Training Data")
#far, med, near

# Train SVM algorithm via e1071. 
library("e1071")
svmfit <- svm(y ~., data = vp, kernel = "radial", cost = 100, scale = TRUE)
summary(svmfit)
#plot(svmfit, vp)

# Predict new data
#new_file = readline(prompt = "Enter complete directory and filename: ")
#dist_new = readline(prompt = "What distance? (Near/Medium/Far): ")
new_file = "C:/Users/Paul/SVM/P01.csv"
dist_new = "Medium"

vp_new <- read.csv(file = new_file, header = FALSE, skip = 1)[ ,c(8, 1)]
y_new = rep(dist_new, nrow(vp_new))
vp_test <- data.frame(x = vp_new, y = y_new)

ypred = predict(svmfit, vp_test)
table(predict = ypred, truth = vp_test$y)

#Extract Peak Maxima
y_peak = max(vp_new$V8)
y_peak
x_peak = (which.max(vp_new$V8))
x_peak
tatpeak <- (vp_new[x_peak,])
tatpeak = tatpeak$V1
tatpeak
peak <- data.frame(tatpeak, y_peak)

#Extract 10% Before Peak
get10 = as.integer(nrow(vp_new)*0.1)
get10
lowerlimit = x_peak - get10
lowerlimit
test_vp_new = vp_new[lowerlimit:x_peak, ]
test_vp_new

#Predict 10% Before Peak
test_vp_new <- data.frame(x.V8 = test_vp_new$V8, x.V1 = test_vp_new$V1, y=dist_new)
ypred = predict(svmfit, test_vp_new)
table(predict = ypred, truth = test_vp_new$y)
ypred

#Plot New Data
lines(vp_new$V1, vp_new$V8, col="white", lwd = 2)
lines(test_vp_new$x.V1, test_vp_new$x.V8, col="black", lwd = 3)
points(peak, type="p", pch = 19, col="black", cex = 1.5)


