rm(list = ls()) #removes all of the variables stored previously
library("Hmisc") #import

COVID19_line_list_data <- read.csv("C:/Users/A S C/Downloads/Covid R analysis/COVID19_line_list_data.csv")
data <- read.csv("C:/Users/A S C/Downloads/Covid R analysis/COVID19_line_list_data.csv")
describe(data)  #Hmisc command

#cleaned up death column 
data$death_dummy <- as.integer(data$death != 0)
#death rate
sum(data$death_dummy) / nrow(data)

#AGE
#claim: people who die older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#is this statistically significant?

t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
#normally, if p-value is <0.05, we reject null hypothesis and
#conclude that this is statically significant

#GENDER
#claim: GENDER has no effect
men = subset(data, gender == "male") 
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%!
#is this statistically significant?

t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.95)
#95% confidence: men have from 1.7% to 7.8% higher chance
#of dying
#p-value = 0.02 < 0.05, so this is statistically
#significant
