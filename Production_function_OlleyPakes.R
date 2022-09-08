
############## Problem Set IO Production Function estimation ###############

############ Agustina Colonna UZH October 2020 #############

rm(list=ls()) 
library(dplyr)
library(tidyverse)
library(foreign)
library(xtable)
library(fixest)
library(stargazer)
library(modelsummary)
library(randomizr)
library(Hmisc)
library(plm)
library(fastDummies)

setwd('/Users/agcolo/Dropbox/PhDCourses_Zurich/Second_year/Empirical_IO/Problem Set Productivity')

df <- read.dta('PS1_Data.dta')


# Exercise 1 --------------------------------------------------------------

variables = colnames(df)[3:length(colnames(df))]

# Function to create descriptive stats 

Table_stats = function(dataf, vbles){
  table =  data.frame(matrix(0,0,6))
  colnames(table) = c('Variable', 'Mean', 'Median', 'Std Dev', 'Min', 'Max')
  for ( i in vbles) {
    table[i,1] = i
    table[i,2] = round(mean(dataf[[i]]),2)
    table[i,3] = round(median(dataf[[i]]),2)
    table[i,4] = round(sd(dataf[[i]]),2)
    table[i,5] = round(min(dataf[[i]]),2)
    table[i,6] = round(max(dataf[[i]]),2)
  }
  return(table)
}

### Full sample ###
table1 = Table_stats(df, variables)

stargazer(table1, out='Table1.tex')

### Only firms sampled all years ###

df2 = df %>% group_by(firm) %>% mutate(sampled = n())  %>% ungroup() %>% filter(sampled == max(sampled))

table2 =  Table_stats(df2, variables)

stargazer(table2, output= 'Table2.tex')
### Only firms exiting sample ###

df3 = df %>% group_by(firm) %>% mutate(sampled = n())  %>% ungroup() %>% filter(sampled != max(sampled))

table3 =  Table_stats(df3, variables)

stargazer(table3, output= 'Table3.tex')
# Exercise 2 --------------------------------------------------------------

pooled = feols(Y ~ A + K + L | year , data = df2)
within = feols(Y ~ A + K + L | year + firm  , data = df2)
between = plm(Y ~ A + K + L + as.factor(year) , data = df2, model = 'between') #age is colinear
random = plm(Y ~ A + K + L + as.factor(year) , data = df2, model = 'random')

# Hausman test
phtest(
  Y ~ A + K + L | year ,
  df,
  model = c("within", "random")) #reject at 5%

cm = c('A' ='Age','K' = 'Capital', 'L'= 'Labor','(Intercept)'= 'Intercept')

listmodels = list('Pooled' = pooled, 'Within' = within, 'Between' = between, 'Random' = random)

modelsummary(listmodels,coef_omit = "[^A|K|L|(Intercept)]", coef_map = cm, output = 'Ex2_reg.tex')


# Exercise 3 --------------------------------------------------------------

dif1 = plm(Y ~ A + K + L + as.factor(year), data = df2, model = 'fd')

# Create 2nd and 3rd differences
for (var in variables) {
  new_var <- paste(var, "dif2", sep = "")
  new_var2 <- paste(var, "dif3", sep = "")
  df2 <- df2 %>% ungroup () %>% group_by(firm) %>%
    mutate({{new_var}} := .data[[var]] - Lag(.data[[var]], 2 ) ,
           {{new_var2}} := .data[[var]] - Lag(.data[[var]], 3 ) )
}

dif2 = feols(Ydif2 ~ Adif2 + Kdif2 + Ldif2 | year , data = df2)
dif3 = feols(Ydif3 ~ Adif3 + Kdif3 + Ldif3 | year , data = df2)

listmodels = list('First Diff' = dif1, 'Second Diff' = dif2, '3rd Diff' = dif3)

modelsummary(listmodels, output = 'Exercise3_reg.tex')

  # Exercise 4 --------------------------------------------------------------

#### a #####

pooled = feols(Y ~ A + K + L | year , data = df)
within = feols(Y ~ A + K + L | year + firm  , data = df) # A is dropped due to collinearity

modelsummary(pooled, within, export = 'Exercise4a_reg.tex')

#### b #####
df = df %>% group_by(firm) %>% mutate(sampled = n() , n_obs_firm = 1:n()) %>%
            mutate(X = ifelse(n_obs_firm == max(n_obs_firm) & n_obs_firm!= 10, 1,0 )) %>% ungroup() %>%
            mutate(Isq = I^2, Ksq = K^2 , Asq = A^2)


probit = glm(X ~ I + A + K +Isq + Ksq + Asq + K*A + K*I + A*I , data=df, family = binomial(link="probit"))

Pr = predict(probit, type = "response")
X_g = Pr = predict(probit, type = "link")

df = df %>% mutate(Xgamma = X_g , IMR_numerator =  dnorm(X_g),
                   IMR_denominator = pnorm(X_g) , IMR = IMR_numerator/IMR_denominator) %>% ungroup %>% group_by(firm) %>% mutate(l_IMR = Lag(IMR))

# Regressions with IMR

pooled_imr = feols(Y ~ A + K + L + l_IMR | year , data = df)

within_imr = feols(Y ~ A + K + L + l_IMR | year + firm  , data = df)

modelsummary(pooled_imr, within_imr, export = 'Exercise4b_reg.tex')


# Exercise 5 --------------------------------------------------------------

#### a  OP 1st stage #####

OP_1 = feols(Y ~ L + I + A + K +Isq + Ksq + Asq + K*A + K*I + A*I + as.factor(year) , data=df )

B_l = OP_1$coefficients[2]

modelsummary(OP_1,
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01),
             output = 'OP_1stage.tex' )


#### b Phi estimation ####

df = df %>% mutate( Phi = OP_1$coefficients[1] + OP_1$coefficients[3]*I + OP_1$coefficients[4]*A + OP_1$coefficients[5]*K +
      OP_1$coefficients[6]*Isq + OP_1$coefficients[7]*Ksq + OP_1$coefficients[8]*Asq + OP_1$coefficients[18]*K*A +
      OP_1$coefficients[19]*K*I + OP_1$coefficients[20]*A*I) %>%
      ungroup() %>% group_by(firm) %>%
      mutate(Phi_lag = Lag(Phi), K_lag = Lag(K), A_lag = Lag(A), X_lag = Lag(IMR_denominator))

df = df %>% dummy_cols(select_columns = 'year')

OP_1$coefficients[5]

#### c  OP 2nd stage #### 
df = df %>% mutate(constant = 1)
OP_2 = nls(Y - B_l*L  ~ b_0 + b_k*K + b_a*A + b_1* (Phi_lag - b_k*(K_lag) - b_a*(A_lag)) +
               b_2* (((Phi_lag) - b_k*(K_lag) - b_a*(A_lag))^2) + 
               fe3*year_3 + fe4*year_4 + fe5*year_5 + fe6*year_6 + fe7*year_7 +
               fe8*year_8 +fe9*year_9 + fe10*year_10, data=df ,
               start=list(b_0=0, b_k = 0.2, b_a = 0.13, b_1 = 0.5,  b_2 = 0.25,
                           fe3 = 1, fe4 = 1, fe5=1, fe6=1, fe7=1,
                          fe8=1, fe9=9, fe10=9))

### d OP 2nd stage correcting for selection ###

OP_2s = nls(Y - B_l*L  ~ b_0 + b_k*K + b_a*A + b_1* (Phi_lag - b_k*(K_lag) - b_a*(A_lag)) +
             b_2* (((Phi_lag) - b_k*(K_lag) - b_a*(A_lag))^2) + 
              b_3 * X_lag  + b_4 * X_lag^2 + b_5 * X_lag * (Phi_lag - b_k*(K_lag) - b_a*(A_lag)) +
             fe3*year_3 + fe4*year_4 + fe5*year_5 + fe6*year_6 + fe7*year_7 +
             fe8*year_8 +fe9*year_9 + fe10*year_10, data=df ,
             start=list(b_0=0, b_k = 0.2, b_a = 0.13, b_1 = 0.5,  b_2 = 0.25,
             b_3 = 3, b_4=1, b_5=0.5,
             fe3 = 1, fe4 = 1, fe5=1, fe6=1, fe7=1,
             fe8=1, fe9=9, fe10=9))


modelsummary(OP_2, OP_2s,
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01),
             output = 'OP_2stage.tex' )

### e ####

op = olley_pakes(data=df, Y ~ L | K | I | A +year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8 + year_9 + year_10, id = 'firm' , time = 'year')

op

df = df %>% mutate(exit = ifelse(n_obs_firm == max(n_obs_firm), 1,0 ))

op2 = olley_pakes(data=df, exit = ~exit,  Y ~ L | K | I | A +year_2 + year_3 + year_4 + year_5 + year_6 + year_7 + year_8 + year_9 + year_10, id = 'firm' , time = 'year')

op2

modelsummary(op, op2,
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01),
             output = 'OP_2stage_opcommand.tex' )

