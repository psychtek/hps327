# As per the PWI manual: score/number of domains*10 to get a SWB score. 
# Then mutate this to a new column called "PW.Index"
#
select(well_being_df) %>%
mutate(PW.Index = (well_being_df$PWI1 + 
                well_being_df$PWI2 + 
                well_being_df$PWI3 + 
                well_being_df$PWI4 + 
                well_being_df$PWI5 + 
                well_being_df$PWI6 + 
                well_being_df$PWI7)/7*10)

#Personality scores for each trait(O,C,E,A,N) are taken for each group of 10. 
# These are summed and mutated to a new column for each trait.

# Openess
select(well_being_df) %>%
  mutate(Openess =
           (well_being_df$Personality1 + 
  well_being_df$Personality2 +
  well_being_df$Personality3 +
  well_being_df$Personality4 +
  well_being_df$Personality5 +
  well_being_df$Personality6 +
  well_being_df$Personality7 +
  well_being_df$Personality8 +
  well_being_df$Personality9 +
  well_being_df$Personality10))

# Conscientiousness
select(well_being_df) %>%
  mutate(Conscientiousness =
           (well_being_df$Personality11 + 
              well_being_df$Personality12 +
              well_being_df$Personality13 +
              well_being_df$Personality14 +
              well_being_df$Personality15 +
              well_being_df$Personality16 +
              well_being_df$Personality17 +
              well_being_df$Personality18 +
              well_being_df$Personality19 +
              well_being_df$Personality20))

# Extraversion
select(well_being_df) %>%
  mutate(Extraversion =
           (well_being_df$Personality21 + 
              well_being_df$Personality22 +
              well_being_df$Personality23 +
              well_being_df$Personality24 +
              well_being_df$Personality25 +
              well_being_df$Personality26 +
              well_being_df$Personality27 +
              well_being_df$Personality28 +
              well_being_df$Personality29 +
              well_being_df$Personality30))

# Agreeableness
select(well_being_df) %>%
  mutate(Agreeableness =
           (well_being_df$Personality31 + 
              well_being_df$Personality32 +
              well_being_df$Personality33 +
              well_being_df$Personality34 +
              well_being_df$Personality35 +
              well_being_df$Personality36 +
              well_being_df$Personality37 +
              well_being_df$Personality38 +
              well_being_df$Personality39 +
              well_being_df$Personality40))

# Neuroticism
select(well_being_df) %>%
  mutate(Neurticism  =
           (well_being_df$Personality41 + 
              well_being_df$Personality42 +
              well_being_df$Personality43 +
              well_being_df$Personality44 +
              well_being_df$Personality45 +
              well_being_df$Personality46 +
              well_being_df$Personality47 +
              well_being_df$Personality48 +
              well_being_df$Personality49 +
              well_being_df$Personality50))

# Homeostatically Protected Mood as a measuer by Davern., et, al. that asks how people 
# feel about life in general across three domains of "happy", "content" & "excited or alert".
# Here the relevant columns are pulled, summed, divided by the 3 domains and mutated to a new column
# called "affect". 
select(well_being_df) %>%
  mutate(affect =
           (well_being_df$Affect1 +
              well_being_df$Affect2 +
              well_being_df$Affect3))
  

### Multiple Discrepancies Theory (MDT) - Cognition 
# Firstly I mutate to a new column labelled "MDT" and averaged. 
# Then each discrepancy mean is worked out and stored in its respective object. 

# 1. Self Best
# 2. Self Future
# 3. Self Progress
# 4. Self Needs
# 5. Self Deserves
# 6. Self Other
# 7. Self Wants

select(well_being_df) %>% 
  mutate(MDT =
           (well_being_df$Cognition1 +
           well_being_df$Cognition2 +
           well_being_df$Cognition3 +
           well_being_df$Cognition4 +
           well_being_df$Cognition5 +
           well_being_df$Cognition6 +
           well_being_df$Cognition7)/7
           )

select(well_being_df) %>% 
  mutate(self.best = well_being_df$Cognition1) %>%
  mutate(self.future = well_being_df$Cognition2) %>%
  mutate(self.progress = well_being_df$Cognition3) %>%
  mutate(self.needs = well_being_df$Cognition4) %>%
  mutate(self.deserves = well_being_df$Cognition5) %>%
  mutate(self.other = well_being_df$Cognition6) %>%
  mutate(self.wants = well_being_df$Cognition7)
