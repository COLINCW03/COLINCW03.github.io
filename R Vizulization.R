health_data <- read.csv("C:/Users/Colin/Downloads/health_data.csv")
str(health_data)
quality_freq= table(health_data$Quality)

quality_freq

frequency <-table(health_data)
frequency

barplot(frequency, 
        main= "Health data", 
        xlab= "Quality of health",
        ylab= "Number of responses",
        col= "blue", #define Color
        ylim=c(0,80))#creates Horizontial axis between 0 and 300.
barplot
# Census Data
census_data <- read.csv("C:/Users/Colin/Downloads/census_data.csv")
hist(census_data$HouseValue,labels = TRUE)
hist(census_data$HouseValue)

athletic_data <- read.csv("C:/Users/Colin/Downloads/athletic_data.csv")
health_freq= table(athletic_data$Age,athletic_data$Brand)
health_freq

barplot(health_freq,
        main="Relationship of Age and brand",
        col= c("blue","orange"),
        legend= rownames(health_freq),
        xlab="brand",
        ylab= "persons")
#Question 4 
testscores_data <- read.csv("C:/Users/Colin/Downloads/testscores_data.csv")
plot(testscores_data$Final,testscores_data$Midterm,
  main = "Exam Scores",
  xlab = "final",
  ylab = "midterm",
  col= c("blue","red"),
  pch=16,
  )
legend("bottom",legend=c("Final","Mideterm"),col = c("blue","red"),lty=1,cex=.5)
#Question 5
internetstocks_data <- read.csv("C:/Users/Colin/Downloads/internetstocks_data.csv")
plot(internetstocks_data$Amazon~internetstocks_data$Year,type="l")
plot(internetstocks_data$Amazon~internetstocks_data$Year,
     xlab="year",
     ylab="Stock prices",
     col= "orange",
     ylim=c(0,1300),
     type="l")
lines(internetstocks_data$Google~internetstocks_data$Year,col="blue",type="l")
legend("bottom",legend=c("Amazon","Google"),col = c("orange","blue"),lty=1,cex=.5)


#Quiz 2 

enrollment_data <-read.csv("C:/Users/Colin/Downloads/enrollment_data.csv")
enrollment_freq= table(enrollment_data$Major,enrollment_data$Class)
enrollment_freq
barplot(enrollment_freq,
        main="Number of students in each class and Major",
        col= c("blue","orange","red","green"),
        xlab="Class",
        ylab= "Major")
legend("bottom",legend=c("arts","business","engineering","sciences"),col= c("blue","orange","red","green", lty=1,cex=.5))
