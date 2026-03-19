#Assignment 6
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#--------
#Library and general setup
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 6")
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(gt)
library(haven)
library(prodlim)
library(did)
#-----
#PART 1
#-----
#1.1.Data setup and exploration
df=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")
summary(df)
#a)
df = df %>%
  mutate(NJ = ifelse(location == "PA", 0, 1))
table(df$NJ)

df %>% 
  group_by(NJ) %>% 
  summarise(
    mean_wage_before=mean(wageBefore,na.rm=TRUE),
    mean_wage_after=mean(wageAfter,na.rm=TRUE))

#As we can see, there has been a slight increase in the mean starting wage for NJ during the period studied, specially compared to PA (where the wages stagnated).

#b)

means=df%>% 
  group_by(NJ) %>% 
  summarise(
    before= mean(fullBefore,na.rm= TRUE),
    after=mean(fullAfter,na.rm= TRUE),
    change=after-before
  )
means
nj_change=means$change[means$NJ==1]
nj_change
pa_change=means$change[means$NJ==0]
pa_change
DiD=nj_change-pa_change
DiD
#As we can see, during this period NJ experienced a small increase on employment while PA had a decrease in employment. These results alone are not enough to explain if an increase in wages cases an increase in employment.
#c)

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
nrow(df_long)
nrow(df)*2

#The number of rows in df are half as in df_long as each restaurant results were recorded before and after the changes in minimun wages.  

#-----

#1.2. DiD regression
#a) Estimate the DiD regression using fixest:
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(list("Model using Fixest"=m_did),stars = TRUE,
             gof_map = c("nobs","r.squared"),
             output = "gt")
#NJ represents the effect of belonging to NJ. Post is the effect of time. Post*NJ is the treatment effect. It matches de manual calculation. 
#b)
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
tablesummary1_p1=modelsummary(list("Model using Fixest"=m_did, "Model using FE"=m_did_fe),
             stars = TRUE,
             gof_map = c("nobs","r.squared"),
             output = "gt")
gtsave(tablesummary1_p1,
       "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 6/tablesum1_p1.png")
####
#------
#1.3. Wages as a validation check
#a)
df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))
m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
#b)
#----
#-----
#PART 2
#-----

data(mpdta)
summary(mpdta)   
#----
#2.1 Data structure and visualization
#a)
n_distinct(mpdta$countyreal)
n_distinct(mpdta$first.treat)
table(mpdta$first.treat) 
#b)
mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
                         levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))
plot1_p2=ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")
ggsave("plot1_part2_ass6.png", plot1_p2, width = 6, height = 4)
##
#----
#2.2 Naive TWFE vs. Callaway-Sant´anna estimator
#a)
mpdta = mpdta %>%
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))
naive_twfe = feols(lemp ~ treated_post|countyreal+year, data = mpdta)
naive_twfe
coef(naive_twfe)
##The coefficient suggests there is a negative relationship between teen employment and treatment (the increase of the minimum wage). Also, this result is statistically significant
## This model pools together all treatment cohorts together. By doing this, it assumes that treatment effects are the same for all cohorts and that treatment effects are constant over time. For the case of staggered DiD, this model ignores the differences caused by receiving the treatment at different times.
#b)
out <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~1,
  data = mpdta,
  est_method = "reg"
)
summary(out)
overallatt=aggte(out,type = "simple")
summary(overallatt)
#It is similar to the naive TWFE (-0.03654894). Nonetheless, it is important to consider that both reach the number in different ways and that the Callaway-Santanna avoids the forbidden comparisons.
#c)
es <- aggte(out, type = "dynamic")
summary(es)
ggdid(es)
ggsave("plot2_part2_ass6.png")
## For the pretreatment, we observe that leads are close to zero and their confidence intervals include zero. This evidence supports the parallel trends assumption, that meaning that before the treatment the cases were not being affected in a significant way.
## In the case of the post treatment, we can observe that the effect increases overtime. 
group_effects <- aggte(out, type = "group")
group_effects

#-----
#2.3.Pre-testing the parallel trends assumption
#a)
out_boot <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~1,
  data = mpdta,
  est_method = "reg",
  bstrap = TRUE,
  cband = TRUE
)
summary(out_boot)
overallatt_boot=aggte(out_boot,type = "simple")
overallatt_boot
# The p value is  0.16812. This is a high value of p value and in this case means that fails to reject the null hypothesis (treated units do not differ from controls before treatment). In other words, it supports the parallel trends assumption.
#b)
ggdid(out_boot)
ggsave("plot3_part2_ass6.png")
##Yes, al pretreatment ATT are close to zero across cohorts.
#c)

#----
#2.4 Comparing control group specifications
#a)
out_nyt <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~1,
  data = mpdta,
  est_method = "reg",
  control_group = "notyettreated"
)
summary(out_nyt)
overallatt_cs=aggte(out_nyt,type = "simple")
overallatt_cs
#b)
es_nyt <- aggte(out_nyt, type = "dynamic")
ggdid(es_nyt)

ggsave("plot4_part2_ass6.png")
#c)
