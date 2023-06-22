setwd("~/Documents/MSc Data Science and Analytics/Statistics Theory & Methods/Practicals/Coursework")
load(file="killersandmotives.Rdata")

killersandmotives$Motive

#Installing packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("psych")
install.packages("nortest")
install.packages("BSDA")

#Using packages
library("dplyr")
library("tidyverse")
library("psych")
library("nortest")
library("BSDA")


#creating and saving the sample

identification <- Sys.getenv("ID_NUMBER")

createsample(identification)
save(mysample, file="mysample.Rdata")

#loading mysample

table(mysample$Motive)

head(mysample)

sum(is.na(mysample))

#Checking the file size of the data
file.info("mysample.Rdata")$size

typeof(mysample$AgeFirstKill)

#Checking the type of the data variables
str(mysample)

#Storing dataset in "killers"
killers <- mysample

# <-----   CLEANING   -----> #

#structure of the dataset
str(killers)

typeof(killers$KillerID)
typeof(killers$Motive)
typeof(killers$Sex)

#Checking for NA values
sum(is.na(killers$Sentence))
sum(is.na(killers$InsanityPlea))

#Check different values and finding any inconsistencies in the columns
unique(killers$KillerID) #perfect
unique(killers$AgeFirstKill) #out of range(99999) values found
unique(killers$AgeLastKill) #perfect
unique(killers$YearBorn) #perfect
unique(killers$Motive) # NA value found
unique(killers$Sex) #perfect
unique(killers$Race) #perfect
unique(killers$Sentence) #NA value found
unique(killers$InsanityPlea) #NA value found

#Displaying the data wher agefirstkill is > 100
killers[killers$AgeFirstKill > 100, c("AgeFirstKill", "Motive")]

#Removing the missing(99999) values
missing <- filter(killers, ! killers$AgeFirstKill > 100)

#checking for NA values in row
sum(rowSums(is.na(missing)))

#Removing rows with NA values (Cleaned Data)
df <- na.omit(missing) 

sum(df$YearFirstKill < 1900) #1 value

#Finding out the year at first kill for each year
df$YearFirstKill <- df$YearBorn + df$AgeFirstKill 

#Removing the data where YearFirstKill is less than 1900
df <- filter(df, ! df$YearFirstKill < 1900)

#Adding a new variable CareerDuration
df$CareerDuration <- df$AgeLastKill - df$AgeFirstKill 
sorteddf <- arrange(df, df$CareerDuration)

#Random
head(df,2)

sum(is.na(df))

sorted <- arrange(df, AgeFirstKill)

sorted %>%
  group_by(KillerID) %>%
  select(df)

duplicated(df$AgeFirstKill) #finding duplicates in agefirstkill

group_by(df, KillerID)

#boxplot

boxplot(df$AgeFirstKill)

sort(df$AgeFirstKill)

# <--- DATA EXPLORATION ---> #


# How many were males

males <- filter(df, df$Sex == "Male") #Filtering the male killers
females <- filter(df, df$Sex == "Female") #Filtering the female killers

table(df$Sex,df$Motive)

nrow(males) #133 males 
nrow(females) #21 females
summary(males)
summary(females)

summary(df)

sd(df$CareerDuration)



#How long killing careers
mean(df$CareerDuration)
?hist
?bar

# Based on three motives 

#numerical summary
motives <- df %>%
  group_by(Motive) %>%
  summarise(mean(AgeFirstKill))

motives

gang <- filter(df, df$Motive == "Gang, cult or organised crime")
mental <- filter(df, df$Motive == "Mental illness (including paranoia, visionary or Munchausen's syndrome)")
revenge <- filter(df, df$Motive == "Revenge or vigilante justice")

mean(gang$AgeFirstKill)
mean(mental$AgeFirstKill)
mean(revenge$AgeFirstKill)

#graphical summary
boxplot(AgeFirstKill ~ Motive, data = df)

?hist

par(mfrow=c(1,1))
hist(gang$AgeFirstKill,
     main = "Gang , cult or organized crime",
     ylim = c(0,40),
     xlim = c(10,60),
     xlab = "Age at first Kill",
     col = "#ea5545",
     )

hist(mental$AgeFirstKill,
     main = "Mental illness",
     ylim = c(0,20),
     xlim = c(15,60),
     xlab = "Age at first Kill",
     col = "#ef9b20",
)

citation("tidyverse")

hist(revenge$AgeFirstKill,
     main = "Revenge or vigilante justice",
     ylim = c(0,20),
     xlim = c(15,60),
     xlab = "Age at first Kill",
     col = "#bdcf32",
)

#Density
hist(gang$AgeFirstKill,
     main = "Gang , cult or organized crime",
     freq = FALSE, 
     right = TRUE,
     xlab = "Age at first Kill",
     col = "#ea5545",
)

hist(mental$AgeFirstKill,
     main = "Mental illness",
     freq = FALSE, 
     right = TRUE,
     xlim= c(15,35),
     xlab = "Age at first Kill",
     col = "#ef9b20",
)

hist(revenge$AgeFirstKill,
     main = "Revenge or vigilante justice",
     freq = FALSE, 
     right = TRUE,
     xlab = "Age at first Kill",
     col = "#bdcf32",
)



# Graphical representation of different motives

ggplot(gang, aes(x = AgeFirstKill)) + #First motive
  geom_histogram( binwidth = 1 )


# <--- MODELLING & ESTIMATION ---> #

### Testing normaility for the "Gang" Motive

#Empirical CDF test (sample CDF)

Fn <- ecdf(revenge$AgeFirstKill) 
Fn(200)


mu <- mean(revenge$AgeFirstKill)
sigma <- sd(revenge$AgeFirstKill)


G <- function(x){
  
  return(pnorm(x, mean = mu, sd = sigma))
  
}

G(200)

plot(Fn, verticals = TRUE, pch =NA)

x <- 1:500
x

lines(x, G(x), col = "red") #We don't reject the null hypothesis

# Kolmogorov-Smirnov test:

ks.test(x = gang$AgeFirstKill,  
        y = "pnorm",
        mean = mu, sd = sigma)  

#Shapiro - Wilk test
shapiro.test(gang$AgeFirstKill) #We reject the null hypothesis

#Chi-squared goodness of fit test

hist(gang$AgeFirstKill, freq=TRUE, breaks = c(10,20,30,40,50))

pearson.test(gang$AgeFirstKill) #We reject the null hypothesis

#QQ test

# Standardised order statistics:
mu <- mean(revenge$AgeFirstKill)
sigma <- sd(revenge$AgeFirstKill)


z <- (sort(revenge$AgeFirstKill) - mu)/sigma

n <- length(revenge$AgeFirstKill)
r <- (1:n)

# Quantiles of N(0, 1):
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)

# Normal Q-Q plot:
plot(q, z, 
     main="Q-Q plot ( Revenge or vigilante justice )", 
     xlab = "Quantiles of Age First Kill",
     ylab = "Standardised order statistics"
     )
# Line with intercept 0 and slope 1:
abline(a = 0, b = 1, col = "red")



## Histogram showing normal distributions

mean(revenge$AgeFirstKill)
sd(revenge$AgeFirstKill)

length(gang$AgeFirstKill) #79
length(mental$AgeFirstKill) #20
length(revenge$AgeFirstKill) #55

hist(revenge$AgeFirstKill, freq = FALSE)  

x <- seq(from = min(revenge$AgeFirstKill), to = max(revenge$AgeFirstKill), by = 0.1) 

lines(x, dnorm(x, mean = 30.4, sd = 9.8), lwd = 1, col = "blue")

hist(gang$AgeFirstKill, freq = FALSE) 

# <--- ESTIMATION ---> #

#Estimating for mean
mu      <- gang_mean
sigma   <- gang_sigma 

AgeFirstKill  <- rep(NA, 1000)
AgeFirstKill2  <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = length(gang_test), mean = mu, sd = sigma)
  AgeFirstKill[i] <- mean(x)
  AgeFirstKill2[i] <- quantile(x, type = 1)[3]   
  
}

par(mfrow = c(1, 1))

hist(AgeFirstKill, xlim = range(c(AgeFirstKill, AgeFirstKill2)), 
     main="Histogram of Revenge")
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(AgeFirstKill), col = "blue", lty = 2, lwd = 3)

hist(AgeFirstKill2, xlim = range(c(AgeFirstKill, AgeFirstKill2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(AgeFirstKill2), col = "blue", lty = 2, lwd = 3)

#Estimation for variance

mu      <- gang_mean
sigma   <- gang_sigma 

AgeFirstKillSigma <- rep(NA, 1000)
AgeFirstKillSigma2 <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = length(gang_test), mean = mu, sd = sigma)
  
  AgeFirstKillSigma[i] <- sd(x)^2
  AgeFirstKillSigma2[i] <- (9/10)*sd(x)^2   
}

par(mfrow = c(1, 1))

hist(AgeFirstKillSigma, 
     xlim = range(c(AgeFirstKillSigma, AgeFirstKillSigma2)),
     main = "Histogram of Revenge")
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(AgeFirstKillSigma), col = "blue", lty = 2, lwd = 3)

hist(AgeFirstKillSigma2, xlim = range(c(AgeFirstKillSigma, AgeFirstKillSigma2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(AgeFirstKillSigma2), col = "blue", lty = 2, lwd = 3)

# <--- INFERENCE ABOUT EACH MOTIVE ---> #

gang_test <- gang$AgeFirstKill
mental_test <- mental$AgeFirstKill
revenge_test <- revenge$AgeFirstKill

length(gang_test) #z-test
length(mental_test) #t-test
length(revenge_test) #z-test

# Performing the z-test for the "Gang" Motive

gang_mean <- mean(gang_test)
gang_sigma <- sd(gang_test)

z.test(gang_test, 
       mu = 27 ,
       sigma.x = gang_sigma) #mean = 27

#Manually performing z-test

x <- revenge_test
# Sample size:
n <- length(x)

# Estimate of the standard deviation:
sigma <- sd(x)

# Sample mean:
xbar <- mean(x)

# Test statistic:
z = (xbar - 27)/sqrt((sigma^2)/ n)
z
# Critical value (for a 5% level test):
c <- qnorm(0.975)

# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*1.96*sqrt(sigma^2/n)

# Reject the null?
(z >  c) | (z < -c)
CI

# p-value:
2*pnorm(-0.1228468)

##We reject the null hypothesis

#Performing the t-test for the "Mental" Motive

mental_mean <- mean(mental_test)
mental_sigma <- sd(mental_test)

t.test(x= mental_test,
       mu = 27,
       ) #mean = 27

#Manually performing the t-test

x <- mental_test
# Sample size:
n <- length(x)

# Estimate of the standard deviation:
sigma <- sd(x)

# Sample mean:
xbar <- mean(x)

# Test statistic:
#z = (xbar - 27)/sqrt((sigma^2)/ n)
t = (xbar - 27)/sqrt(s^2/n)
pt(q = abs(t), df = n-1)
# Critical value (for a 5% level test):
c <- qnorm(0.975)

# Calculate the confidence interval:
CI =  xbar + c(-1, 1)*2.306004*sqrt(sigma^2/n)

# Reject the null?
(z >  c) | (z < -c)
CI
pt
# p-value:
2*pnorm(-1.164994)

#SHapiro test 
shapiro.test(mental_test)
pearson.test(mental_test)
qqnorm(mental_test)

##We reject a true null hypothesis

#Performing the z-test for the "Revenge" Motive

revenge_mean <- mean(revenge_test)
revenge_sigma <- sd(revenge_test)

z.test(revenge_test, 
       mu = 27,
       sigma.x = revenge_sigma) #mean = 27

##We reject the null hypothesis

#Performing two sample z-test for Gang and Revenge motive.

z.test(gang_test,
       revenge_test,
       alternative="two.sided",
       mu = 27 ,
       sigma.x = gang_sigma,
       sigma.y = revenge_sigma
       ) #mean = 27






