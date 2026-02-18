#Applied Quantitative Methods for the Social Sciences II
#Assignment 2
#Student: Ana Paula Muto

#Part 1
#1.1. Set up and data preparation

##library set up
library(tidyverse)
library(dplyr)
library(broom)
library (modelsummary)
library(gt)
library(ggplot2)

setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment2") #Set up wd

qoqdata <-read.csv("C:/Users/pc/Downloads/qog_std_cs_jan26.csv") #loading dataset

df = qoqdata %>% #select variables and rename them
  select(country=cname,
         epi= epi_epi,
         womenparl=wdi_wip,
         gov_eff=wbgi_gee,
         green_seats=cpds_lg)

nrow(df) #to check how main countries 
summary(df)#to have general info about df

df_compobs<-df %>% 
  filter(complete.cases(.))
nrow(df_compobs) #to check how many countries remain
df_compobs$country #to check which countries remain
##If we drop observations with missing values, 36 countries remain. In this case, it is better to not exclude missing values (omit.na) as the sample would be too small.

skimr::skim(df) #An additional way of presenting the data

#1.2. Exploratory visualization

plot1<-ggplot(df, aes(x = womenparl, y = epi)) + #creating a scatterplot with a linear fit.
                geom_point() + 
                geom_smooth(method = 'lm')+
                labs(x= "Women in Parliament (%)", y = "EPI Score")
plot1

##We can observe a positive relatinship. This means that it appears that parlaments with more women representatives are more likely to have higher EPI scores. 

#1.3. Bivariate regression

m1_p1<-lm(epi ~ womenparl, data = df) #Run a bivariate regression

tidy(m1_p1) #Extract the results using tidy 
modelsummary(m1_p1, output="gt") 

df_womenparl_cq<-data.frame(
  womenparl = quantile (df$womenparl, c(0.25, 0.75), na.rm = TRUE))
df_womenparl_cq
epi_wp25<-39.180+0.308*15.36199 ## Calculate epi for 25%
epi_wp75<-39.180+0.308*33.68051 ## Calculate epi for 75%
epi_wpiq<-epi_wp75-epi_wp25 ## Calculate difference
epi_wpiq 
#1.4. Multiple regression
m2_p1<-lm(epi ~ womenparl+gov_eff, data = df) #multiple regression adding gov eff as a control
tidy(m2_p1) #Extract the results using tidy 
#1.4.b.Compare the coefficient on women parl between the bivariate and multiple regression. Does it change? In what direction? Explain in a comment what this suggests
modelsummary(list("Bivariate model" = m1_p1,#create a table to compare both models
                  "Multiple model"  = m2_p1), 
             estimate="{estimate}{stars}", output = "gt")
plot2<- modelplot(list("Bivariate model" = m1_p1,#creating a plot to compare both models
                "Multiple model"  = m2_p1))
plot2
##These results show that the effect of women in parlament decreases drastically once you control for goverment effectiveness. This suggests that in the bivariate model results both women in parlament as EPI scores were affected by goverment effectiveness.  

#1.5 Demonstrating OVB
##extract relevant coefficients
beta1_biva = tidy(m1_p1) %>% filter(term == "womenparl") %>% pull(estimate)
beta1_mult = tidy(m2_p1) %>% filter(term == "womenparl") %>% pull(estimate)
beta2_mult = tidy(m2_p1) %>% filter(term == "gov_eff") %>% pull(estimate)

##Auxiliary regression
aux = lm(gov_eff ~ womenparl, data = df)
delta = tidy(aux) %>% filter(term == "womenparl") %>% pull(estimate)
round(beta1_mult + beta2_mult * delta, 4)
round(beta1_biva, 4)
##Both values match, confirming the OVB form
#1.6 Robust standard errors
modelsummary(list(m1_p1,m2_p1)) #print the multiple regression results with default (classical) standard errors.
modelsummary(list(m1_p1,m2_p1),vcov="robust")  #print the multiple regression results with  vcov= "robust")
##We can observe a slight difference  but conclusions typically don’t change with this sample.

#1.7 Presenting results
##Create a table comparing the bivariate and multiple regression models side by side.
modelsummary(list(
  "Bivariate Regression"=m1_p1,
  "Multivariate Regression"=m2_p1),
  vcv="robust", output="gt") %>% 
  gtsave("summary_part1_ass2.png")

##Create a coefficient plot using modelsummary::modelplot() comparing both models.
coeff_plot1<-modelsummary::modelplot(list(
  "Bivariate Regression"=m1_p1,
  "Multivariate Regression"=m2_p1)) %>% 
  ggsave(
  filename = "plotpart1_ass2.png")#Save the plot using ggsave().

##Part 2

#2.1. Data preparation
star <- read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")#Load dataset star
##Use summary and head to visualize
summary(star)
head(star)

##Create a factor variable for classtype with labels: "Small", "Regular", "Regular+Aide".
star$classtype <- factor(star$classtype,
                         levels = c(1, 2, 3),
                         labels = c("Small", "Regular", "Regular+Aide"))
##Create a factor variable for race
star$race <- factor(star$race,
                    levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("White", "Black", "Asian",
                               "Hispanic", "Native American", "Other"))

##Create a binary variable small that equals 1 if classtype == "Small" and 0 otherwise.
star$small <- ifelse(star$classtype == "Small", 1, 0)

#Report the number of observations and the number of non-missing observations for g4reading and g4math.

nrow(star) #number of total observations

star %>% 
    summarise(total_nonna_math = sum(!is.na(g4math)))
star %>% 
  summarise(total_nonna_reading = sum(!is.na(g4reading)))

#2.2. Comparing groups

aggregate(g4reading ~ classtype, data = star, mean, na.rm = TRUE) #Calculate the mean 4th grade reading score by class type.
###The group that scores the highest is the small class. 

m1_p2 <- lm(g4reading ~ small, data = star)# bivariate regression of g4reading on small. 
summary(m1_p2)
###The coefficient suggests that small classes have a small effect on reading outcomes for 4th graders. In is important to note that the effect seems to be small and doesn't seem to be statistically significant.

##Verify that the regression coefficient equals the difference in means between small and regular+aide classes.

mean_small <- mean(star$g4reading[star$small == "1"], na.rm = TRUE)
mean_reg_aide <- mean(star$g4reading[star$small == "0"], na.rm = TRUE)

mean_small - mean_reg_aide
coef(model_reading)["small"]

##Bivariate regression for g4math. Is the pattern similar?

m2_p2<-lm (g4math ~ small, data = star)
summary(m2_p2)

mean_smallmath <- mean(star$g4math[star$small == "1"], na.rm = TRUE)
mean_reg_aidemath <- mean(star$g4math[star$small == "0"], na.rm = TRUE)

mean_smallmath - mean_reg_aidemath
coef(m2_p2)["small"]

###These results suggest that there is a similar pattern observed in math, as students in smaller classes seem to obtain better outcomes. It is importanto to note that the effect seems to be much smaller compared to reading results. 
#2.3. Adding controls
m3_p2<- lm(g4reading ~ small+race+yearssmall, data = star) #multiple regression of g4reading on small, race, and yearssmall.
summary(m3_p2)
### We can observe that the coefficient moves in the opposite direction when adding for controls. This suggests that the previous results were confounded by the effects of race in both the class size and reading scores. In other words, these results suggest that the effect of the student background (using race as a proxy) has a more profound effect on the outcome than the assignment to a smaller class. 
### The coefficient of "yearssmall" suggests that the time spent in small classes also affects positively the reading scores. Though, it is important to note that its coefficient has the opposite direction of the "small" variable.

#2.4. Interactions
m4_p2<-lm(g4reading ~ small * race + yearssmall, data = star) #Create a model with the interaction race*small
summary(m4_p2)
tidy(m4_p2) #printing results using tidy function.
##The estimated effect of a small class for White students is 54.3, while for  Black students is 6.97.
####These results suggest that the effect of being in a small class differ considerably by race, as the effects for "black" and "other" are positive and for "asians" are negative. Nonetheless, these results don't seem to be statistically significant, considering the p-values and the standard errors.  

#2.5. Presenting results
#2.5.Create a table with modelsummary() comparing all your reading score models (bivariate, multiple, interaction), using robust standard errors.

modelsummary(list(
  "Bivariate"=m1_p2,
  "Multivariate with controls"=m3_p2,
  "Multivariate with interaction"=m4_p2), 
  estimate="{estimate}{stars}",
  vcv="robust", 
  output = "gt") %>%  #creating table
  gtsave("summary_part2_ass2.png") #saving table

coeff_plot2<-modelsummary::modelplot(list(
  "Bivariate"=m1_p2,
  "Multivariate with controls"=m3_p2,
  "Multivariate with interaction"=m4_p2),vcov = "robust") %>% #creating coefficient plot
  ggsave(
    filename = "plotpart2_ass2.png")#Save the plot using ggsave().
#2.6 Discussion
## This data shows that the effect of class sizes on student achievement are heterogeneous, specially when considering for other factors such as the time spent on the small class and the student background (the study used the proxy of race). In other words, even though it seems to have positive results at first glance, these are actually mediated by additional factors. 
## These results are more credible than an observational study as they are part of an experiment, where students were randomly assigned to different class sizes. When the random assignment is properly done, the differences on the data should be caused only by the treatment. This is preferred over other types of observational studies where other variables affect the results.
## Nonetheless, it is important to take into consideration that most results were not statistically significant, the standard errors were large, and (in most cases) the magnitudes of the coefficients were small. This is to be expected considering the large proportion of the dataset that has omitted values. 
