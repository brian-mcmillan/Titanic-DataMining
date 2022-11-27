]# Step 1: Load the train dataset.

# Possible imports needed
library(dplyr)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Amelia)
library(psych)
library(GGally)
library(caret)


# rename train as df
df <- train

# glimpse
print(df)

# Upon looking at the dataset, it is apparent Cabin has far too many NA values.
# For now, I will delete it.

# This is unfortunate, because in my EDA I hypothesized that a Cabins proximity to
# the upper deck could effect survivability.

# If I feel it could become relevant later on, I will replace 
# the null values with the mode since cabin is categorical.

# Also, I will drop (for now) PassengerID, Name, Ticket, Fare, Embarked.

df = select(df, Survived, Pclass, Age, Sex, SibSp, Parch)
print(df)

# Let's check what data type is associated with each variable

str(df)

# There are a number of issues. First, Survived and Pclass should be
# categorical variables.

df$Survived = factor(df$Survived)

# Pclass is ordinal
df$Pclass = factor(df$Pclass, order=TRUE, levels=c(3,2,1))

# Also, Sex is saved as a string. This needs to be cast as a factor (2 levels).

df$Sex = factor(df$Sex)
str(df)

# Next, let's replace null values with the mean.

  # Age has a lot of null values
  df$Age[is.na(df$Age)] <- mean(df$Age, na.rm=TRUE)
  # Normalize Age
  normalize <-function(x) { (x -min(x))/(max(x)-min(x)) }
  df$Age <- normalize(df$Age)
  
  
# Next, let's make some graphs to visualize how the target variable (Survived)
# is shaped by the other variables.

# Survived vs did not survive

ggplot(df, aes(x = Survived)) + 
    geom_bar(fill = "light blue") + 
    geom_text( stat = 'count', aes(label = stat(count))) 

# 549 survived, 342 did not.
  
ggplot(df, aes(x = Survived, fill = Sex)) +
      geom_bar(width=.5, position = position_dodge(width=1)) +
      geom_text(stat  = 'count', aes(label=stat(count)),
                position = position_dodge(width=1))

# Of those who did not survive, 468 were male--81 female. 
# Of those who did survive, 109 were male, 233 female.


# Survival by PClass

ggplot(df, aes(x = Survived, fill=Pclass)) +
  geom_bar(position = position_dodge())

# Survival by Age

df$grouped.age = cut(df$Age, c(0,10,20,30,40,50,60,70,80,100))

ggplot(df, aes(x = grouped.age, fill=Survived)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

df$grouped.age = NULL


# Decision Tree


sample <- createDataPartition(df$Survived, p = 0.8, list = FALSE)
train_set <- df[sample, ]
test_set <- df[-sample, ]



mytree <- rpart(Survived~., 
          data = df, 
          method = "class"
          )

rpart.plot(mytree)


pred_cart <- predict(mytree, test_set, type = "class")
confusionMatrix(pred_cart, positive = "1", test_set$Survived,
                dnn = c("Predicted", "Actual"))


# KNN
