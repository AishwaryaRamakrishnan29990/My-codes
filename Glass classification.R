df <- read.csv("/Users/aishwaryaramakrishnan/Desktop/BDAP/Machine Learning/glass.csv",header = TRUE)

# Finding the highly correlated columns
library(corrplot)
M <- cor(df)
corrplot(M,method = 'circle')
df_new <- data.frame("Na" = df$Na, 
                     "Mg" = df$Mg, 
                     "Al" = df$Al,
                     "Ba" = df$Ba,
                     "Type" = df$Type) 
df_new$Type <- as.factor(df_new$Type)

# Create test and training data set
set.seed(101)
g<-runif(nrow(df_new))
df1<-df_new[order(g),]

library(caTools)
Y = df1[,5]
ss <- sample.split(Y, SplitRatio = 3/4)
train = df1[ss,1:5]  
test = df1[!ss,1:5]  

####################################### KNN
set.seed(101)
m <- knn(train[,1:4],test[,1:4], cl = train[,5],k=5)
tbl <- table(test[,5],m)
tbl
accuracy <- sum(diag(tbl))/sum(tbl)
accuracy


####################################### Decision  trees
set.seed(101)
library(rpart)
library(rpart.plot)
m<-rpart(Type~.,data = train,method = "class")
rpart.plot(m,extra = 101,type = 3,fallen.leaves = TRUE)
p<-predict(m,test,type = "class")
tbl1 <- table(test$Type,p)
tbl1
accuracy1 <- sum(diag(tbl1))/sum(tbl1)
accuracy1

####################################### SVM
set.seed(101)
svm.model <- svm(Type ~ ., data = train, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test[,-5])
tbl2 <- table(pred = svm.pred, true = test[,5])
accuracy2 <- sum(diag(tbl2))/sum(tbl2)
accuracy2

#SVM Tuning
mytunedsvm <- tune.svm(Type ~ ., data = train,cost = 2^(2:8),kernel = "polynomial") 
summary(mytunedsvm)
plot (mytunedsvm, transform.x=log10, xlab=expression(log[10](gamma)), ylab="C")

####################################### Naive Bayes

m<-naiveBayes(Type~.,data = train)
m
p<-predict(m,test)
p
tbl3 <- table(test[,5],p)
accuracy3 <- sum(diag(tbl3))/sum(tbl3)
accuracy3


