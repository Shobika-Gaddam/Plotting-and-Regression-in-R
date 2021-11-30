data()
data(package=.packages(all.available = TRUE))

#apply() takes dataframe or matrix as an input and gives output in vector, list or array
m1<-matrix(c<-(1:10),nrow=5,ncol=6)
m1
a_m1<-apply(m1,1,sum) #manipulation on rows
a_m1
a_m1<-apply(m1,2,sum) #manipulation on columns
a_m1

#list apply lapply() takes list, vector or dataframe as input and converts into list as output
movies<-c("SPIDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower<-lapply(movies,tolower) #tolower is used to convert to lowercase
str(movies_lower)
movies_lower<-unlist(lapply(movies,tolower)) #convert to list
str(movies_lower)

#set apply sapply() takes list,vector and dataframes as input and gives vector or matrix as output
dt<-iris
head(dt)
sapply(dt,class)

#statistics apply tapply() computes a measure(mean,mode,median,min,max etc) or a function for each factor variable in a vector
data(iris)
tapply(iris$Sepal.Width,iris$Species,median)

require(ggplot2)
data(diamonds)
head(diamonds)
hist(diamonds$carat,main="Carat Histogram",xlab="Carat")
plot(price~carat,data=diamonds)
plot(diamonds$carat,diamonds$price)
boxplot(diamonds$carat)
ggplot(data=diamonds)+geom_histogram(aes(x=carat),stat="bin",binwidth = 1)
g<-ggplot(diamonds,aes(x=carat,y=price))
g+geom_point(aes(color=color))
g+geom_point(aes(color=color))+facet_wrap(~color)
g+geom_point(aes(color=color))+facet_grid(cut~color)
ggplot(diamonds,aes(y=carat,x=cut))+geom_boxplot()
require(gridExtra)
p1<-ggplot(diamonds,aes(y=carat,x=cut))+geom_point()+geom_violin()
p2<-ggplot(diamonds,aes(y=carat,x=cut))+geom_violin()+geom_violin()
grid.arrange(p1,p2,ncol=2)

data(economics)
head(economics)
ggplot(economics,aes(x=date,y=pop))+geom_line()
require(lubridate) #Create month and year columns
economics$year<-year(economics$date)
economics$month<-month(economics$date) #Subset the data
econ2000<-economics[which(economics$year>=2000),]
head(econ2000)
g<-ggplot(econ2000,aes(x=month,y=pop))
g<-g+geom_line(aes(color=factor(year),group=year))
g<-g+scale_color_discrete(name="Year")
g<-g+labs(title="Population Growth",x="Month",y="Population")
g

library(caret)
data("iris")
head(iris)
nrow(iris)
ncol(iris)

df<-data("iris")
df<-iris
dim(df)
names(df)
validation_index<-createDataPartition(df$Species,p=0.80,list=FALSE)
validation<-df[-validation_index,]
df<-df[validation_index,]
dim(df)

sapply(df,class)
head(df)
levels(df$Species)
percentage<-prop.table(table(df$Species))*100
cbind(freq=table(df$Species),percentage=percentage)
summary(df)

x<-df[,1:4]
y<-df[,5]
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(x[,i],main=names(iris)[i])
}
plot(y) # barplot for class breakdown
featurePlot(x=x,y=y,plot="ellipse")

#box and whisker plots for each attribute
featurePlot(x=x,y=y,plot = "box")
#density plots for each attribute by class value
scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)
control<-trainControl(method="cv",number=10)
metric="Accuracy"
set.seed(7) #data random assignments
fit.lda<-train(Species~.,data=df,method="lda",metric=metric,trControl=control) #linear discriminatory analysis
set.seed(7)
fit.cart<-train(Species~.,data=df, method="rpart",metric=metric,trControl=control) #classification and regression trees
set.seed(7)
fit.knn<-train(Species~.,data=df, method="knn",metric=metric,trControl=control) #k- nearest neighbours
set.seed(7)
fit.svm<-train(Species~.,data=df, method="svmRadial",metric=metric,trControl=control)
set.seed(7)
fit.rf<-train(Species~.,data=df, method="rf",metric=metric,trControl=control) #random forest

#Summarize accuracy of models
results<- resamples(list(lda=fit.lda,cart=fit.cart,knn=fit.knn,svm=fit.svm,rf=fit.rf))
summary(results)
dotplot(results)

#Summarize best model
print(fit.lda)
predictions<-predict(fit.lda,validation)
confusionMatrix(predictions,validation$Species)
