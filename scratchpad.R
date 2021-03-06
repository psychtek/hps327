#This document was where I dumpmed random stuff and toyed around with code.

#learning to use the sapply function to a list and converting factors to numeric types
pwi <- sapply(well_being_df[,4:10], as.numeric)

personality <- sapply(well_being_df[,11:60], as.numeric)

cognition <- sapply(well_being_df[,64:70], as.numeric)

affect <- sapply(well_being_df[,61:63], as.numeric)

employment <- sapply(well_being_df[,3], as.numeric)

gender <- sapply(well_being_df[,2], as.numeric)

age <- sapply(well_being_df[,1], as.numeric)


attach(well_being_df)
Male <- select(well_being_df, Gender, Age, EmploymentStatus) %>% 
  filter(Gender == 1) #Males

Female <- select(well_being_df, Gender, Age, EmploymentStatus) %>% 
  filter(Gender == 2) #Females

Trans <- select(well_being_df, Gender, Age, EmploymentStatus) %>% 
  filter(Gender == 3) #Transgender

#group by gender and summarise means
select(well_being_df, Gender, Age, EmploymentStatus) %>%
  group_by(Gender) %>%
  summarise_all(funs(mean))

#for each gender summarise the mean, sd, min and max values 1 = Male, 2 = Female and 3 = Transgender
select(well_being_df, Gender, Age) %>%
  mutate(gender = labelled::to_factor(Gender)) %>%
  group_by(gender) %>%
  summarise_all(funs(mean, n = n(), sd, min(.,is.na = FALSE), max(.,is.na = FALSE)))
  

select(well_being_df, Gender) %>%
  mutate(gender = labelled::to_factor(Gender)) %>%
  group_by(gender) %>%
  summarise_all(funs(mean, n = n(), sd, min(.,is.na = FALSE), max(.,is.na = FALSE)))

#Whats our overall mean age of responses? 
select(well_being_df, Age) %>%
  summarise(average = mean(Age))

#What is the average PWI Score for each domain?

colSums(well_being_df[4:10]/169*10)

colMeans(well_being_df[4:10])


Open <- rowSums(select(well_being_df, num_range("Personality", 1:10))) #selecting subsets of the personality measures
Consciencious <- rowSums(select(well_being_df, num_range("Personality", 11:20))) 
Extraversion <- rowSums(select(well_being_df, num_range("Personality", 21:30))) 
Agreeable <- rowSums(select(well_being_df, num_range("Personality", 31:40))) 
Neurotic <- rowSums(select(well_being_df, num_range("Personality", 41:50))) 

well_being_df <- well_being_df %>%
  mutate(Open = rowSums(select(well_being_df, num_range("Personality", 1:10)))) %>%
  mutate(Consciencious = rowSums(select(well_being_df, num_range("Personality", 11:20)))) %>%
  mutate(Extraversion = rowSums(select(well_being_df, num_range("Personality", 21:30))) ) %>%
  mutate(Agreeable = rowSums(select(well_being_df, num_range("Personality", 31:40))) ) %>%
  mutate(Neurotic = rowSums(select(well_being_df, num_range("Personality", 41:50))))

big5 <- data_frame(well_being_df$Open, well_being_df$Consciencious, well_being_df$Extraversion, well_being_df$Agreeable, well_being_df$Neurotic)




my.model2 <- lm(formula = well_being_df$pwi_index ~ 
                 well_being_df$affect + 
                 well_being_df$cognition + 
                 well_being_df$Personality +
                 big5)


library(corrplot)

corrplot(corr, method = "circle")

cv.lm(data = newdf, lm1, m = 3) #cross validation 

# M. Ling invert function
invertItem <- function(x, min, max) {
  if(!is.numeric(x) | !is.numeric(min) | !is.numeric(max)) stop("Inputs are not numeric")
  ifelse(max<min, stop("Maximum value is less than minimum value"), "")
  ifelse(x>max | x < min, stop("Value is out of expected range"), "")
  
  reb <- (min-1)
  (max - reb + 1) - (x - reb) + reb
}


# Gender breakdown
select(well_being_df, Gender) %>% 
  mutate(sex = labelled::to_factor(Gender)) %>% 
  group_by(sex) %>% 
  summarise_all(funs(mean, n(), sd))

descriptivez <- well_being_df %>% 
  summarize(
    R_Square = x$r.squared
    , Ajusted_R = x$adj.r.squared , SIGMA = x$sigma
    , PValue = x$p.value
    , Residual = x$df.residual
  )

descriptivez[, -1] <- printnum(descriptivez[, -1])
apa_table(
  descriptivez, 
  format = "html",
  caption = "Descriptive statistics." , 
  note = "This table was created with apa_table"
)

well_being_df %>% 
  select(
    "PW.Index"
    , "Age"
    , "Gender"
    , "Openness"
    , "Conscientiousness"
    , "Extraversion"
    , "Agreeableness"
    , "Neuroticism"
    , "affect"
    , "self_wants"
    , "self_other"
    , "self_deserves"
    , "self_needs"
    , "self_progress"
    , "self_future"
    , "self_past") %>%
  summarise_all(funs(corr.test))


well_being_df %>%
  select("PW.Index", "HPMood", "self_wants", "self_other") %>%
  summarise_all(funs(corr.test(.data)))

library(car)
my_residuals <- residuals(model3)
 hist( x = my_residuals )           # plot a histogram (similar to Figure 14.3a)
 qqnorm( y = my_residuals )         # draw a QQ plot (similar to Figure 14.3b)
 shapiro.test( x = my_residuals )



hist(PWI) #histogram

#POPULATION PARAMETER CALCULATIONS
zscore <- function(variable, score){
  p_sd <- sd(variable)*sqrt((length(variable)-1)/(length(variable)))#population calculations
  p_mean <- mean(variable)
  z <- (score - p_mean)/p_sd #calculate z score
  return(z)
}

zscore(well_being_df$PW_Index, 97)

plot(model3_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance

abline(h = 4*mean(model3_cooksd, na.rm=T), col="red")  # add cutoff line - scores 4 times greater than mean

text(x=1:length(model3_cooksd)+1
     , y=model3_cooksd
     , labels=ifelse(model3_cooksd>4*mean(model3_cooksd, na.rm=T)
                     ,names(model3_cooksd),"")
     , col="red")  # add labels


Week6Data %>% 
  select(tip, total_bill, sex) %>% 
  gather(key = variable, value = value, -sex) %>% 
  group_by(sex, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(sex, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(Female), unlist(Male))$p.value,
         t_value = t.test(unlist(Female), unlist(Male))$statistic)

# Reliability anaysis
# effect sizes

NEO_effect_size_omega <- omega(well_being_df[c(11:20)])


Neuroticism_alpha <- alpha(well_being_df[c(11:20)], check.keys = TRUE)
Extraversion_alpha <- alpha(well_being_df[c(21:30)], check.keys = TRUE)
Openness_alpha <- alpha(well_being_df[c(31:40)], check.keys = TRUE)
Agree_alpha <- alpha(well_being_df[c(41:50)], check.keys = TRUE)
Conscient_alpha <- alpha(well_being_df[c(51:60)], check.keys = TRUE)

select(well_being_df, num_range("Personality", 1:10)) %>%
  mutate_all(myaplha = omega(.))



xvar2 <- select(well_being_df,  num_range("Personality", 1:50)) %>% 
  mutate(Personality6_R = invertItem(Personality6, 1, 5),
            Personality7_R = invertItem(Personality7, 1, 5),
            Personality8_R = invertItem(Personality8, 1, 5),
            Personality9_R = invertItem(Personality9, 1, 5),
            Personality10_R = invertItem(Personality10, 1, 5),
            Personality25_R = invertItem(Personality26,1,5),
            Personality27_R = invertItem(Personality27,1,5),
            Personality28_R = invertItem(Personality28,1,5),
            Personality29_R = invertItem(Personality29,1,5),
            Personality30_R = invertItem(Personality30,1,5),
            Personality36_R = invertItem(Personality36, 1, 5),
            Personality37_R = invertItem(Personality37, 1, 5),
            Personality38_R = invertItem(Personality38, 1, 5),
            Personality39_R = invertItem(Personality39, 1, 5),
            Personality40_R = invertItem(Personality40, 1, 5),
            Personality46_R = invertItem(Personality46, 1, 5),
            Personality47_R = invertItem(Personality47, 1, 5),
            Personality48_R = invertItem(Personality48, 1, 5),
            Personality49_R = invertItem(Personality49, 1, 5),
            Personality25_R = invertItem(Personality50, 1, 5)) 

            #Personality26_R = invertItem(Personality26,1,5),
well_being_df %>% 
  transmute(Neuroticism = ( # Neuroticism
    Personality1 + 
      Personality2 +
      Personality3 +
      Personality4 +
      Personality5 +
      invertItem(Personality6, 1, 5) +
      invertItem(Personality7, 1, 5) +
      invertItem(Personality8, 1, 5) +
      invertItem(Personality9, 1, 5) +
      invertItem(Personality10, 1, 5)))

  
# apa_lm_4 <- apa_print(model4_explore, standardized = FALSE, est_name = "\\beta")

#step 1
apa_lm_1 <- apa_print(model1) #apa_print function takes the lm object and creates format strings to report the results

apa_table(apa_lm_1$table
          , caption = "Regression Model 1"
) # creates the apa formated table to print to screen

apa_anova1 <- apa_print(anova(model1))

apa_table(apa_anova1$table
          , caption = "ANOVA Model 1"
          , note = "Dependant variable: Personal Wellbeing Index")

#step 2
apa_lm_2 <- apa_print(model2) #apa_print function takes the lm object and creates format strings to report the results

apa_table(apa_lm_2$table
          , caption = "Regression Model 2"
) # creates the apa formated table to print to screen

apa_anova2 <- apa_print(anova(model2))
apa_table(apa_anova2$table
          , caption = "ANOVA Model 2"
          , note = "Dependant variable: Personal Wellbeing Index")

#step 3
apa_lm_3 <- apa_print(model3) #apa_print function takes the lm object and creates format strings to report the results

apa_table(apa_lm_3$table
          , caption = "Regression Model 3"
) # creates the apa formated table to print to screen

apa_anova3 <- apa_print(anova(model3))
apa_table(apa_anova3$table
          , caption = "ANOVA Model3"
          , note = "Dependant variable: Personal Wellbeing Index")

apa_lm_4 <- apa_print(model4_explore) #apa_print function takes the lm object and creates format strings to report the results

apa_table(apa_lm_4$table
          , caption = "Exploratory Model"
) # creates the apa formated table to print to screen

apa_anova_model4 <- apa_print(anova(model4_explore))
apa_table(apa_anova_model4$table
          , caption = "ANOVA Explore Model"
          , note = "Dependant variable: Personal Wellbeing Index"
          , format = "markdown")