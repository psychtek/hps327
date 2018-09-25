library(psych)
library(haven)
library(dplyr)
library(lmSupport)
library(papaja)
library(broom)
library(car)
library(olsrr)
library(xtable)
library(apaTables)
library(knitr)
library(MBESS)

well_being_df <- read_sav("data/Wellbeing_2018_df.sav")

Descriptives <- describe(well_being_df[c(1,2,71:78)], fast = TRUE) #descriptives

neo_reliability <- ci.reliability(select(well_being_df
                    , Personality1
                    , Personality2
                    , Personality3
                    , Personality4
                    , Personality5
                    , Personality6_R
                    , Personality7_R
                    , Personality8_R
                    , Personality9_R
                    , Personality10_R))

ext_reliability <- ci.reliability(select(well_being_df
                    , Personality11
                    , Personality12
                    , Personality13
                    , Personality14
                    , Personality15
                    , Personality16_R
                    , Personality17_R
                    , Personality18_R
                    , Personality19_R
                    , Personality20_R))


opn_reliability <- ci.reliability(select(well_being_df
                    , Personality21
                    , Personality22
                    , Personality23
                    , Personality24
                    , Personality25
                    , Personality26_R
                    , Personality27_R
                    , Personality28_R
                    , Personality29_R
                    , Personality30_R))

agre_reliability <- ci.reliability(select(well_being_df
                     , Personality31
                     , Personality32
                     , Personality33
                     , Personality34
                     , Personality35
                     , Personality36_R
                     , Personality37_R
                     , Personality38_R
                     , Personality39_R
                     , Personality40_R))

conc_reliability <- ci.reliability(select(well_being_df
                     , Personality41
                     , Personality42
                     , Personality43
                     , Personality44
                     , Personality45
                     , Personality46_R
                     , Personality47_R
                     , Personality48_R
                     , Personality49_R
                     , Personality50_R))
#run intercorrelation checks


HPmood_reliability <- ci.reliability(select(well_being_df, Affect1, Affect2, Affect3))

MDT_reliability <- ci.reliability(select(well_being_df
                          , Cognition1
                          , Cognition2
                          , Cognition3
                          , Cognition4
                          , Cognition5
                          , Cognition6
                          , Cognition7))

pwi_reliability <- ci.reliability(select(well_being_df
                          , PWI1
                          , PWI2
                          , PWI3
                          , PWI4
                          , PWI5
                          , PWI6
                          , PWI7))

glrstab <- function(x, export=FALSE) {
  
  r <-corr.test(x)$r	#taking just the correlation matrix; no N, or p
  p <-corr.test(x)$p	#taking the p*s
  
  #define notions for significance levels
  mystars <- ifelse(p < .001, "***"
                    , ifelse(p < .01, "**"
                             , ifelse(p < .05, "*"
                                      , ifelse(p < .10, "+", " "))))
  
  #round r, define new matrix Rnew with the correlations from rnd and paste mystars
  rnd  <- papaja::printnum(r, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!								                     
  Rnew <- matrix(paste(rnd, mystars, sep=""), ncol=ncol(rnd)) 
  
  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnew) <- ''		
  Rnew[upper.tri(Rnew)] <- ''								                	
  
  rownames(Rnew) <- paste(1:ncol(rnd), colnames(rnd), sep=" ")         #define number and name
  colnames(Rnew) <- paste(1:ncol(rnd), "", sep="") 			       #define number
  
  #fun-part: we trim the top half 
  Rnew[upper.tri(Rnew)] <- ''			
  Rnew
  
  Rnew <- cbind(round(describe(x)[,3:4],2), Rnew)		     #describe x, M sD - put them in the matrix
  colnames(Rnew)[1:2] <- c("M","SD")					      		#Beschriftung der neuen Spalten
  Rnew <- Rnew[,1:(ncol(Rnew)-1)]							        	#delete the last column (ugly)
  
  #export to clipboard
  
  if (export==TRUE){
    result<-write.table(Rnew
                        , "clipboard"
                        , sep=";"
                        , row.names=FALSE)
  }
  else result <- Rnew
  return(result)
  
}

corr_Matrix <- well_being_df[c(71:78)] #subset relevant columns
a <- glrstab(corr_Matrix) #the function in action!

rownames(a) <- c(
  "PW_Index"
  , "Neuroticism"
  , "Extraversion"
  , "Openness"
  , "Agreeableness"
  , "Conscientiousness"
  , "HPMood"
  , "MDT"
)

colnames(a)   <- c("$M$", "$SD$", "1", "2", "3", "4", "5", "6", "7")
cor_results <- apa_table(a
          , escape  = FALSE
          , format = "html"
          , caption = "Correlation matrix of the main variables"
          , note    = "Note. M and SD are used to represent mean and standard deviation, respectively. 
          Values in square brackets indicate the 95% confidence interval for each correlation. 
          The confidence interval is a plausible range of population correlations that could have caused 
          the sample correlation (Cumming, 2014). * indicates p < .05. ** indicates p < .01.")


model1 <- lm(PW_Index ~ 
               HPMood
             , data = well_being_df)

model2 <- lm(PW_Index ~ 
               HPMood
             + MDT 
             , data = well_being_df)

model3 <- lm(PW_Index ~  
               HPMood 
             + MDT 
             + Neuroticism
             + Extraversion
             + Openness
             + Agreeableness
             + Conscientiousness
             , data = well_being_df)

model4_explore <- lm(PW_Index ~
                       HPMood
                     + Neuroticism, data = well_being_df)

apa_lm_4 <- apa_print(model4_explore)
apa_anova4 <- apa_print(anova(model4_explore)) 
#apa.reg.table(model1, model2, model3)

# Run an ANOVA to check for sig effect. 
ANOVA_Models <- anova(model1, model2, model3)

H_regression <- apa_print(list(Step1 = model1, Step2 = model2, Step3 = model3), boot_samples = 0) #Hierarchical Regression table

Full_regression_output <- apa.reg.table(model1, model2, model3)

# Run an ANOVA to check for sig effect. 
ANOVA_Models <- anova(model1, model2, model3)

# apa funtion for tables. 
#step 1
apa_lm_1 <- apa_print(model1) #apa_print function takes the lm object and creates format strings to report the results


apa_anova1 <- apa_print(anova(model1))

model_1_fit <- apa_lm_1$full_result$modelfit

#step 2
apa_lm_2 <- apa_print(model2) #apa_print function takes the lm object and creates format strings to report the results



apa_anova2 <- apa_print(anova(model2))
model_2_full <- apa_lm_2$full_result$modelfit

#step 3
apa_lm_3 <- apa_print(model3) #apa_print function takes the lm object and creates format strings to report the results


apa_anova3 <- apa_print(anova(model3))


#Tolerance and vif values
model1_tolerance_vif <- ols_coll_diag(model1)

model2_tolerance_vif <- ols_coll_diag(model2)

model3_tolerance_vif <- ols_coll_diag(model3)


#part and partials

#model1_partials <- ols_correlations(model1)

model2_partials <- ols_correlations(model2)

model3_partials <- ols_correlations(model3)



