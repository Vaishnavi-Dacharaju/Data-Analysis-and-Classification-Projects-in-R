install.packages("corrplot")
install.packages("rpart")
library(corrplot)
library(rpart)
rice_data <- read.csv("C:/Users/Vaishnavi/Downloads/Rice_Cammeo_Osmancik.csv")
head(rice_data)
boxplot(rice_data[, c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength")], main = "Boxplots of Numerical Features")
names(rice_data)
boxplot(rice_data[, c("Area", "Perimeter", "Major_Axis_Length", "Minor_Axis_Length")], main = "Boxplots of Numerical Features")
hist(rice_data$Area, main = "Histogram of Area", xlab = "Area")
hist(rice_data$Perimeter, main = "Histogram of Perimeter", xlab = "Perimeter")
hist(rice_data$Major_Axis_Length, main = "Histogram of Major Axis Length", xlab = "Major Axis Length")
hist(rice_data$Minor_Axis_Length, main = "Histogram of Minor Axis Length", xlab = "Minor Axis Length")
barplot(table(rice_data$Class), main = "Bar Chart of Class Labels", xlab = "Class", ylab = "Frequency")
correlation_matrix <- cor(rice_data[, c("Area", "Perimeter", "Major_Axis_Length", "Minor_Axis_Length")])
corrplot(correlation_matrix, method = "color", main = "Correlation Plot of Numerical Features")
X <- rice_data[, c("Area", "Perimeter", "Major_Axis_Length", "Minor_Axis_Length")]
y <- rice_data$Class
decision_tree_model <- rpart(Class ~ ., data = rice_data, method = "class")
predictions <- predict(decision_tree_model, newdata = rice_data, type = "class")
accuracy <- mean(predictions == rice_data$Class)
cat("Accuracy:", accuracy, "\n")
cat("The decision tree classifier achieved an accuracy of", accuracy * 100, "%, indicating its effectiveness in classifying rice varieties.\n")









