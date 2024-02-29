imdb=read.csv("/Users/Desktop/MGSC 661/Project/IMDB_data.csv")
attach(imdb)
require(car)
summary(imdbScore)
boxplot(imdbScore)
hist(imdbScore)
summary(movieBudget)
boxplot(movieBudget)
hist(movieBudget)
plot(movieBudget, imdbScore)
reg1=lm(imdbScore~movieBudget)
summary(reg1)
ncvTest(reg1)
summary(releaseYear)
boxplot(releaseYear)
hist(releaseYear)
plot(releaseYear, imdbScore)
reg2=lm(imdbScore~releaseYear)
summary(reg2)
ncvTest(reg2)
summary(duration)
boxplot(duration)
hist(duration)
plot(duration, imdbScore)
reg3=lm(imdbScore~duration)
summary(reg3)
ncvTest(reg3)
summary(nbNewsArticles)
boxplot(nbNewsArticles)
hist(nbNewsArticles)
plot(nbNewsArticles, imdbScore)
reg4=lm(imdbScore~nbNewsArticles)
summary(reg4)
ncvTest(reg4)
summary(actor1_starMeter)
summary(actor2_starMeter)
summary(actor3_starMeter)
par(mfrow=c(1,3))
boxplot(actor1_starMeter)
boxplot(actor2_starMeter)
boxplot(actor3_starMeter)
hist(actor1_starMeter)
hist(actor2_starMeter)
hist(actor3_starMeter)
plot(actor1_starMeter, imdbScore)
plot(actor2_starMeter, imdbScore)
plot(actor3_starMeter, imdbScore)
par(mfrow=c(1,1))

reg5=lm(imdbScore~actor1_starMeter)
summary(reg5)
ncvTest(reg5)
reg6=lm(imdbScore~actor2_starMeter)
summary(reg6)
ncvTest(reg6)
reg7=lm(imdbScore~actor3_starMeter)
summary(reg7)
ncvTest(reg7)
summary(actor2_starMeter)
boxplot(actor2_starMeter)
hist(actor2_starMeter)
plot(actor2_starMeter, imdbScore)
reg6=lm(imdbScore~actor2_starMeter)
summary(reg6)
ncvTest(reg6)
summary(actor3_starMeter)
boxplot(actor3_starMeter)
hist(actor3_starMeter)
plot(actor3_starMeter, imdbScore)
reg7=lm(imdbScore~actor3_starMeter)
summary(reg7)
ncvTest(reg7)
summary(nbFaces)
boxplot(nbFaces)
hist(nbFaces)
plot(nbFaces, imdbScore)
reg8=lm(imdbScore~nbFaces)
summary(reg8)
ncvTest(reg8)
summary(movieMeter_IMDBpro)
boxplot(movieMeter_IMDBpro)
hist(movieMeter_IMDBpro)
plot(movieMeter_IMDBpro, imdbScore)
reg9=lm(imdbScore~movieMeter_IMDBpro)
summary(reg9)
ncvTest(reg9)

#Data Description (Categorical):
IMDB_data =imdb
attach(IMDB_data)
library(ggplot2)
require(methods)
library(tidyverse)
library(fastDummies)
library(car)

IMDB_data$releaseMonth = factor(IMDB_data$releaseMonth, levels = month.abb)

# Boxplot - releaseMonth
ggplot(IMDB_data, aes(x=releaseMonth, y=imdbScore)) +  geom_boxplot()

# Bar chart - releaseMonth
ggplot(IMDB_data, aes(x=reorder(releaseMonth, releaseMonth, function(x)-length(x)))) + geom_bar() + labs(x='releaseMonth')

IMDB_data$language <- ifelse(language == 'English', 'English', 'Others')

# Boxplot - language
ggplot(IMDB_data, aes(x=language, y=imdbScore)) +  geom_boxplot()

# Bar chart - language
ggplot(IMDB_data, aes(x=reorder(language, language, function(x)-length(x)))) + geom_bar() + labs(x='language')

IMDB_data$country <- ifelse(country=='USA','USA', ifelse(country=='UK','UK','Others'))

# Boxplot - country
ggplot(IMDB_data, aes(x=country, y=imdbScore)) +  geom_boxplot()

# Bar chart - country
ggplot(IMDB_data, aes(x=reorder(country, country, function(x)-length(x)))) + geom_bar() + labs(x='country')
# Boxplot - colourFilm  
ggplot(IMDB_data, aes(x=colourFilm, y=imdbScore)) +  geom_boxplot()

# Bar chart - colourFilm
ggplot(IMDB_data, aes(x=reorder(colourFilm, colourFilm, function(x)-length(x)))) + geom_bar() + labs(x='colourFilm')

#Data Preprocessing:
IMDB <- imdb
IMDB_test <- read.csv("/Users/irisliu/Desktop/MGSC 661/Project/test_data_IMDB.csv")
attach(IMDB)
library(ggplot2)
library(tidyverse)
library(fastDummies)
library(car)
IMDB_year <- IMDB[releaseYear>=1980,]
attach(IMDB_year)
IMDB_year_language<-IMDB_year
IMDB_year_language$language2 <- ifelse(language == 'English', 'English', 'Others')
inflation <- read.csv("/Users/irisliu/Desktop/MGSC 661/Project/inflation_data.csv")
inflation <- inflation[,1:2]
colnames(inflation) <- c("releaseYear", "amount")
IMDB_year_language_budget <- IMDB_year_language
IMDB_year_language_budget<-merge(x = IMDB_year_language_budget , y = inflation, by = "releaseYear", all.x = TRUE)
IMDB_year_language_budget$movieBudget_inflated<-
  360.20/IMDB_year_language_budget$amount*IMDB_year_language_budget$movieBudget
IMDB_year_language_budget<-IMDB_year_language_budget[,-44]
IMDB_year_language_budget_starMeter<-IMDB_year_language_budget
attach(IMDB_year_language_budget_starMeter)
summary(actor1_starMeter)
IMDB_year_language_budget_starMeter<-IMDB_year_language_budget_starMeter %>% filter(actor1_starMeter<=4548)
summary(actor2_starMeter)
IMDB_year_language_budget_starMeter<-IMDB_year_language_budget_starMeter %>% filter(actor2_starMeter<=7445)
summary(actor3_starMeter)
IMDB_year_language_budget_starMeter<-IMDB_year_language_budget_starMeter %>% filter(actor3_starMeter<=11960)
IMDB_year_language_budget_starMeterSum<-IMDB_year_language_budget
IMDB_year_language_budget_starMeterSum$starMeterSum<- actor1_starMeter+actor2_starMeter+actor3_starMeter

summary(IMDB_year_language_budget_starMeterSum$starMeterSum)
IMDB_year_language_budget_starMeterSum<- IMDB_year_language_budget_starMeterSum %>%
  filter(starMeterSum<=28474)
#attach(IMDB_year_language_budget_starMeterSum)
#summary(lm(imdbScore~starMeterSum))
#IMDB_year_language_budget_starMeter_duration <-IMDB_year_language_budget_starMeter
#IMDB_year_language_budget_starMeter_duration <-IMDB_year_language_budget_starMeterSum
IMDB_year_language_budget_starMeter_duration <-IMDB_year_language_budget_starMeter
attach(IMDB_year_language_budget_starMeter_duration)
IMDB_year_language_budget_starMeter_duration<- 
  IMDB_year_language_budget_starMeter_duration[duration>=60 & duration<=200,]
IMDB_year_language_budget_starMeter_duration_faces <- IMDB_year_language_budget_starMeter_duration
attach(IMDB_year_language_budget_starMeter_duration_faces)
IMDB_year_language_budget_starMeter_duration_faces<-
  IMDB_year_language_budget_starMeter_duration_faces[nbFaces<15,]
IMDB_year_language_budget_starMeter_duration_faces_country<-IMDB_year_language_budget_starMeter_duration_faces
attach(IMDB_year_language_budget_starMeter_duration_faces_country)
IMDB_year_language_budget_starMeter_duration_faces_country$country2<-
  ifelse(country=='USA','USA',
         ifelse(country=='UK','UK','Others'))
IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter<-IMDB_year_language_budget_starMeter_duration_faces_country
attach(IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter)
IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter<-
  IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter[movieMeter_IMDBpro<=20000,]
attach(IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter)
IMDB_clean<-IMDB_year_language_budget_starMeter_duration_faces_country_movieMeter %>% 
  select(movieTitle,  movieID,imdbScore, releaseYear, movieBudget_inflated, releaseMonth,
         duration, language2, country2, maturityRating, actor1_starMeter, actor2_starMeter, actor3_starMeter,
         colourFilm, nbFaces, action, adventure, scifi, thriller, musical, romance, western,
         sport, horror, drama, war, animation, crime, movieMeter_IMDBpro)
write.csv(IMDB_clean, file = "IMDB_clean.csv")

#Test Data Processing:
  IMDB_test <- read.csv("/Users/irisliu/Desktop/MGSC 661/Project/test_data_IMDB.csv")
attach(IMDB_test)
library(tidyverse)
#langauge
IMDB_test$language2 <- ifelse(language == 'English', 'English', 'Others')
#country
IMDB_test$country2<-
  ifelse(country=='United States','USA',
         ifelse(country=='UK','UK','Others'))

IMDB_test_clean<-IMDB_test %>% 
  select(movieTitle,  movieID,imdbScore, releaseYear, movieBudget, releaseMonth,
         duration, language2, country2, maturityRating, actor1_starMeter,actor2_starMeter,actor3_starMeter,
         colourFilm, nbFaces, action, adventure, scifi, thriller, musical, romance, western,
         sport, horror, drama, war, animation, crime, movieMeter_IMDBpro)
write.csv(IMDB_test_clean, file = "IMDB_test_clean.csv")

#Model Selection:
library("car")
#IMDB_clean$maturityRating[IMDB_clean$maturityRating == "NC-17"] <- "NC_17"
IMDB_clean$maturityRating[IMDB_clean$maturityRating == "PG-13"] <- "PG_13"
IMDB_clean$maturityRating[IMDB_clean$maturityRating == "TV-14"] <- "PG_13"
#IMDB_clean$maturityRating[IMDB_clean$maturityRating == "TV-G"] <- "TV_G"
IMDB_clean$maturityRating[IMDB_clean$maturityRating == "TV-G"] <- "G"
IMDB_clean$maturityRating[IMDB_clean$maturityRating == "NC-17"] <- "R"
library('fastDummies')
library(tidyverse)
names(IMDB_clean)
data = IMDB_clean
data$country2 = as.factor(data$country2)
levels(data$country2)
#change the excluded dummy to Others
country3 = relevel(data$country2, ref = "Others")
#set categories as factor
data$language2 = as.factor(data$language2)
data$colourFilm = as.factor(data$colourFilm)
data$maturityRating = as.factor(data$maturityRating)
data$releaseMonth = as.factor(data$releaseMonth)

reg2=lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+duration+language2+country3+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
          +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data)

summary(reg2)
residualPlots(reg2)
require(lmtest)
require(plm)

#Step 1: Detect Heteroskedasticity
#Visually
residualPlot(reg2, quadratic=FALSE) #remove the blue line
#numerical test: Non-constant variance (NCV) test
ncvTest(reg2) 
#if p-value of test < 0.05, it is heteroskedasticity
require(lmtest)
require(plm)

coeftest(reg2, vcov=vcovHC(reg2, type="HC1"))
##estimate unchanged for original & corrected model, only made it more significant
outlierTest(reg2) 
#remove outliers
data2=data[-c(858,542,610,495), ]

reg3=lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+duration+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
          +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

summary(reg3)
require(psych) #show correlation matrices between observations
#select quantitative variables - numberical
#quantvars=data2[, -c(1,3,4,5,9,11,12,13,)]
#corr_matrix=cor(quantvars) #known as pairs panels matrix
#round(corr_matrix,2)
#Approach 2: Variance inflation factors (VIF)
vif(reg3)

#If VIF>4, it is a sign of collinearity.
reg5=lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+duration+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
          +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

summary(reg5)
#Creating a polynomial regression with degree d=1
reg41=lm(imdbScore ~ poly(duration,1, raw = TRUE), data = data2)
reg42=lm(imdbScore ~ poly(duration,2),data = data2)
reg43=lm(imdbScore ~ poly(duration,3),data = data2)
reg44=lm(imdbScore ~ poly(duration,4),data = data2)
reg45=lm(imdbScore ~ poly(duration,5),data = data2)
reg41 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,1, raw = TRUE)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg42 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg43 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,3)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg44 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,4)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg45 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,5)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)
#Polynomial ANOVA test shows value of adding extra polynomial, by comparing between tested models.
anova(reg41, reg42, reg43, reg44,reg45)

#add polynomial if the p-value is below or around 0.05
reg42 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)
summary(reg42)

plot(data2$duration, data2$imdbScore)
#choose 5 knot
k1= quantile(data2$duration,1/10) # 1/6
k2= quantile(data2$duration,2/7) 
k3= 140
#splines
library(splines)
reg71=lm(imdbScore~bs(duration,knots=c(k3), degree=1), data = data2)
reg72=lm(imdbScore~bs(duration,knots=c(k3), degree=2), data = data2)
reg73=lm(imdbScore~bs(duration,knots=c(k3), degree=3), data = data2)
reg74=lm(imdbScore~bs(duration,knots=c(k3), degree=4), data = data2)
reg75=lm(imdbScore~bs(duration,knots=c(k3), degree=5), data = data2)

plot=ggplot(data2, aes(y=imdbScore, x=duration))
scatter= geom_point(color="grey")

spline_1 = geom_smooth(method = "lm", formula = y~bs(x,knots=c(k3), degree=1), color="blue")

spline_2 = geom_smooth(method = "lm", formula = y~bs(x,knots=c(k3), degree=2), color="blue")

spline_3=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k3), degree=3), color="blue")

spline_4=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k3), degree=4), color = "blue")

spline_5=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k3), degree=5), color="blue") 


spline_plot1=plot+scatter+spline_1 + geom_vline(xintercept=c(k3), linetype="dashed")
spline_plot2=plot+scatter+spline_2 + geom_vline(xintercept=c(k3), linetype="dashed")
spline_plot3=plot+scatter+spline_3 + geom_vline(xintercept=c(k3), linetype="dashed")
spline_plot4=plot+scatter+spline_4 + geom_vline(xintercept=c(k3), linetype="dashed")
spline_plot5=plot+scatter+spline_5 + geom_vline(xintercept=c(k3), linetype="dashed")

library(ggpubr)
library("gridExtra")
ggarrange(spline_plot1, spline_plot2,spline_plot3,spline_plot4,spline_plot5, labels = c("d=1", "d=2", "d=3","d=4","d=5"), ncol = 2, nrow = 3)

reg43 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+bs(duration,knots=c(k3), degree=3)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)
summary(reg43)
reg42 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)
summary(reg42)
reg51 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+poly(nbFaces,1, raw=TRUE)+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg52 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+poly(nbFaces,2)+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg53 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+poly(nbFaces,3)+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg54 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+poly(nbFaces,4)+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg55 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration,2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+poly(nbFaces,5)+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)
#Polynomial ANOVA test shows value of adding extra polynomial, by comparing between tested models.
anova(reg51, reg52, reg53, reg54,reg55)
#add polynomial if the p-value is below or around 0.05
plot(data2$nbFaces,data2$imdbScore)
#bs(duration,knots=c(k1,k2,k3), degree=2)
summary(reg51)
reg61 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+movieMeter_IMDBpro,data=data2)

reg62 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+poly(movieMeter_IMDBpro,2),data=data2)

reg63 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+poly(movieMeter_IMDBpro,3),data=data2)

reg64 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+poly(movieMeter_IMDBpro,4),data=data2)

reg65 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+poly(movieMeter_IMDBpro,5),data=data2)
#Polynomial ANOVA test shows value of adding extra polynomial, by comparing between tested models.
anova(reg61, reg62, reg63, reg64,reg65)

#add polynomial if the p-value is below or around 0.05
plot(data2$movieMeter_IMDBpro, data2$imdbScore)
#choose 5 knot
k21= 5000
k22= 10000
k23= 15000 
#k4= quantile(duration,4/7) 
#k5= quantile(duration,5/7) 
#splines
library(splines)
reg71=lm(imdbScore~bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=1), data = data2)
reg72=lm(imdbScore~bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=2), data = data2)
reg73=lm(imdbScore~bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=3), data = data2)
reg74=lm(imdbScore~bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=4), data = data2)
reg75=lm(imdbScore~bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5), data = data2)

plot=ggplot(data2, aes(y=imdbScore, x=movieMeter_IMDBpro))
scatter= geom_point(color="grey")

spline_1 = geom_smooth(method = "lm", formula = y~bs(x,knots=c(k21,k22,k23), degree=1), color="blue")

spline_2 = geom_smooth(method = "lm", formula = y~bs(x,knots=c(k21,k22,k23), degree=2), color="blue")

spline_3=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k21,k22,k23), degree=3), color="blue")

spline_4=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k21,k22,k23), degree=4), color = "blue")

spline_5=geom_smooth(method = "lm", formula = y~bs(x,knots=c(k21,k22,k23), degree=5), color="blue") 


spline_plot1=plot+scatter+spline_1 + geom_vline(xintercept=c(k21,k22,k23), linetype="dashed")
spline_plot2=plot+scatter+spline_2 + geom_vline(xintercept=c(k21,k22,k23), linetype="dashed")
spline_plot3=plot+scatter+spline_3 + geom_vline(xintercept=c(k21,k22,k23), linetype="dashed")
spline_plot4=plot+scatter+spline_4 + geom_vline(xintercept=c(k21,k22,k23), linetype="dashed")
spline_plot5=plot+scatter+spline_5 + geom_vline(xintercept=c(k21,k22,k23), linetype="dashed")

library(ggpubr)
library("gridExtra")
ggarrange(spline_plot1, spline_plot2,spline_plot3,spline_plot4,spline_plot5, labels = c("d=1", "d=2", "d=3","d=4","d=5"), ncol = 2, nrow = 3)

reg82 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)

summary(reg82)
reg91 = lm(imdbScore~releaseYear+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)

reg92 = lm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)

reg93 = lm(imdbScore~poly(releaseYear,3)+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)

reg94 = lm(imdbScore~poly(releaseYear,4)+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)

reg95 = lm(imdbScore~poly(releaseYear,5)+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)
#Polynomial ANOVA test shows value of adding extra polynomial, by comparing between tested models.
anova(reg91, reg92, reg93, reg64,reg95)

#add polynomial if the p-value is below or around 0.05
plot(data2$releaseYear,data2$imdbScore)
reg92 = lm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+releaseMonth+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+colourFilm+nbFaces+action+adventure +scifi+thriller+musical+romance+
             +western+sport+horror+drama+war+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5),data=data2)
summary(reg92)
library('fastDummies')
data3 <- dummy_cols(data2, select_columns = 'releaseMonth')

names(data3)
data3 <- dummy_cols(data3, select_columns = 'colourFilm')
data4 <- dummy_cols(data3, select_columns = 'maturityRating')
names(data4)
library(tidyverse)
data3  = rename(data3, colourFilm_Black_and_White = "colourFilm_Black and White")

names(data3)
data5 = data3
data5$maturityRating[data3$maturityRating == "X"] <- "R"
reg92 = lm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5)+releaseMonth_Aug+releaseMonth_Dec+releaseMonth_Feb+releaseMonth_Jan+releaseMonth_Jul+releaseMonth_Jun+releaseMonth_Mar +releaseMonth_Nov +colourFilm_Color,data=data5)
summary(reg92)
reg_try = lm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5)+releaseMonth_Aug+releaseMonth_Dec+releaseMonth_Feb+releaseMonth_Jan+releaseMonth_Jul+releaseMonth_Jun+releaseMonth_Mar +releaseMonth_Nov +colourFilm_Color,data=data5)

summary(reg_try)
library(boot) #package to do cross-validation

k21= 5000
k22= 10000
fit = glm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5)+releaseMonth_Aug+releaseMonth_Dec+releaseMonth_Feb+releaseMonth_Jan+releaseMonth_Jul+releaseMonth_Jun+releaseMonth_Mar +releaseMonth_Nov +colourFilm_Color,data=data5)

mse=cv.glm(data3, fit)$delta[1]
mse 
library(boot) #package to do cross-validation
mse=rep(NA,10)
for (i in 1:10) {
  fit = glm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5)+releaseMonth_Aug+releaseMonth_Dec+releaseMonth_Feb+releaseMonth_Jan+releaseMonth_Jul+releaseMonth_Jun+releaseMonth_Mar +releaseMonth_Nov +colourFilm_Color,data=data5)
  
  mse[i]=cv.glm(data5, fit, K=10)$delta[1]
}

round(mse,3)
mse
mean(mse)
require(caTools)
require(splines)
require(methods)
library(ggplot2)

a = rep(NA,10)


for(n in 1:10){
  sample=sample.split(data5$imdbScore, SplitRatio=0.5)
  train_set=subset(data5, sample==TRUE)
  test_set=subset(data5, sample==FALSE)
  actual=test_set$imdbScore
  
  fit = glm(imdbScore~poly(releaseYear,2)+movieBudget_inflated+poly(duration, degree=2)+language2+country2+maturityRating+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+action+adventure +scifi+thriller+musical+romance+western+sport+horror+drama+animation+crime+bs(movieMeter_IMDBpro,knots=c(k21,k22,k23), degree=5)+releaseMonth_Aug+releaseMonth_Dec+releaseMonth_Feb+releaseMonth_Jan+releaseMonth_Jul+releaseMonth_Jun+releaseMonth_Mar +releaseMonth_Nov +colourFilm_Color,data=data5)
  
  a[n]=mean((actual-predict(fit, test_set))^2)
}

mse=mean(a)
round(mse,3)

#Movies Prediction:
IMDB_test_clean$maturityRating[IMDB_test_clean$maturityRating == "PG-13"] <- "PG_13"
IMDB_test_clean$country2 = as.factor(IMDB_test_clean$country2)
country3 = relevel(IMDB_test_clean$country2, ref = "Others")
#set categories as factor
IMDB_test_clean$language2 = as.factor(IMDB_test_clean$language2)
IMDB_test_clean$colourFilm = as.factor(IMDB_test_clean$colourFilm)
IMDB_test_clean$maturityRating = as.factor(IMDB_test_clean$maturityRating)
IMDB_test_clean$releaseMonth = as.factor(IMDB_test_clean$releaseMonth)
library('fastDummies')
IMDB_test_clean <- dummy_cols(IMDB_test_clean, select_columns = 'releaseMonth')
names(IMDB_test_clean)
IMDB_test_clean['colourFilm_Color'] <- 1
IMDB_test_clean['releaseMonth_Aug'] <- 0
IMDB_test_clean['releaseMonth_Aug'] <- 0
IMDB_test_clean['releaseMonth_Dec'] <- 0
IMDB_test_clean['releaseMonth_Jan'] <- 0
IMDB_test_clean['releaseMonth_Jul'] <- 0
IMDB_test_clean['releaseMonth_Jun'] <- 0
IMDB_test_clean['releaseMonth_Mar'] <- 0
IMDB_test_clean['releaseMonth_Feb'] <- 0
IMDB_test_clean['movieBudget_inflated'] <- IMDB_test_clean$movieBudget
IMDB_test_clean$imdbScore = predict(fit, newdata = IMDB_test_clean)
round(IMDB_test_clean$imdbScore,4)
IMDB_test_clean

