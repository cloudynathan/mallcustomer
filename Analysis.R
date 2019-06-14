#analysis of mall customers

df <- read.csv("C:/workspaceR/mallcustomer/Mall_Customers.csv")
str(df)
colnames(df)[colnames(df) == 'Annual.Income..k..'] <- 'Annual_Income'
colnames(df)[colnames(df) == 'Spending.Score..1.100.'] <- 'Spending_Score'
df$CustomerID <- NULL
levels(df$Gender) <- c("1", "2")
anyNA(df)

#descriptive statistics
gender <- df$Gender
levels(gender) <- c("Female", "Male")
plot(gender, main="Gender",ylim=c(0,120), ylab="Frequency")

Age <- df$Age
hist(Age, main="Age", ylim=c(0,40))

Annual_Income <- df$Annual_Income
hist(Annual_Income, main="Annual Income")

Spending_Score <- df$Spending_Score
hist(Spending_Score, main="Spending Score", ylim=c(0,50))

#k-means clustering
results <- kmeans(df, 5)
results

plot(x=df$Gender,y=df$Spending_Score, main="Gender vs. Spending Score", col=c('mistyrose', 'powderblue'), 
     xlab="Gender", ylab="Spending Score")

AgeSpendingScore <- data.frame("Age" = df$Age, "Spending_Score" = df$Spending_Score)
plot(AgeSpendingScore, main="Age vs. Spending Score")

plot(df[c("Annual_Income", "Spending_Score")], col = results$cluster, main="Annual Income vs. Spending Score")

#independent samples t-test for gender vs spending score
genderspendingscore <- data.frame("Gender" = df$Gender, "Spending_Score" = df$Spending_Score)

bartlett.test(Spending_Score ~ Gender, data=genderspendingscore) #If p-value >= 0.05, use var.equal=TRUE below
t.test(Spending_Score ~ Gender, data=genderspendingscore, var.equal=TRUE, conf.level=0.95)

#multiple linear regression
model <- lm(Spending_Score ~ Age + Annual_Income, data=df)
summary(model)


