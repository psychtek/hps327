Getting errors when modelling from different data types. Needed to convert each variable to a group factor. Created a function to make this happen quicker. 

```{r}
#learning to use the sapply function to a list and converting factors to numeric types
pwi <- sapply(well_being_df[,4:10], as.numeric)

personality <- sapply(well_being_df[,11:60], as.numeric)

cognition <- sapply(well_being_df[,64:70], as.numeric)

affect <- sapply(well_being_df[,61:63], as.numeric)

employment <- sapply(well_being_df[,3], as.numeric)

gender <- sapply(well_being_df[,2], as.numeric)

age <- sapply(well_being_df[,1], as.numeric)
#function
create_col_variable <- function(x,y)
  sapply(well_being_df[,x:y], as.numeric) #need to figure out how it could take a blank argument: x <- create_col_variable(,2) for example.

```


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
  
Gender

#Whats our overall mean age of responses? 
select(well_being_df, Age) %>%
  summarise(average = mean(Age))

#What is the average PWI Score for each domain?

colSums(well_being_df[4:10]/169*10)

colMeans(well_being_df[4:10])



resp.case = data.frame(Freq = colSums(well_being_df[4:10])) 
resp = colSums(well_being_df[4:10])/sum(well_being_df[4:10])*100
casez = colSums(well_being_df[4:10])/nrow(well_being_df[4:10])*100
resp.case

#for each employment answer
Employment <- select(well_being_df, EmploymentStatus, Gender) %>% 
  mutate(EmploymentStatus = labelled::to_factor(EmploymentStatus)) %>% # use labelled package 
  group_by(EmploymentStatus) %>% 
  summarise_all(funs(mean, n = n(), sd,min(.,is.na = TRUE), max(.,is.na = TRUE)))
Employment



#
#You can access your columns without "$" but still can use their labels :
 # 
#  rowSums(data[,c("a","b","c")]
#          
#          If your columns are too much and u can't type "a b c d ... z", you can use ascii code of them with one loop :
#          
#          vec <- rep(0,10)
#          
#          for (i in 1:10)
#          {
#          vec[i]<- intToUtf8(64+i)
#          }
          
#          It provides you "A", "B", ... ,"J" ; now u can use rowSums(data[,vec])
          
#          About your last question in your comment, when u use "," in data[] it defines row's index before it and column's index after it, also in data[] you can use a logical values, because of it above codes running correct.
          

well_being_df2 <- well_being_df


well_being_df2 %>% mutate(pwi = rowSums(select(., contains("PWI"))))
# A tibble: 169 x 72
Age Gender  EmploymentStatus PWI1   PWI2   PWI3   PWI4  PWI5  PWI6  PWI7  Personality1 Personality2 Personality3

well_being_df2 %>% 
  mutate(blubb = well_being_df2 %>% 
           rowwise() %>% 
           select(., contains("PWI")) %>% 
           rowSums()
         )


rowSums(select(well_being_df, contains("PWI"))) #this works 

x <- mutate(well_being_df2, x = rowSums(select(well_being_df2, contains("PWI"))) #this works 



well_being_df2 %>%
  mutate(x = rowSums(select(., contains("PWI")))) %>%
  head()

rowSums(select(well_being_df, contains("PWI"))) 

df <- well_being_df2 %>% mutate(PWI_Indexer = rowSums(select(., contains("PWI"))))

mutate(well_being_df, x = rowSums(select(well_being_df, contains("PWI"))))     

# Confidence Interval
confidence.intervals <- function(x){
  alpha <- 0.05 # set 95% CI
  xbar <- mean(x) #sample mean
  s <- sd(x) # standard deviation of x
  n <- length(x) #number of cases
  
  CI <- qt(1-alpha/2, n-1)*s/sqrt(n)
  
  c(xbar - CI, xbar + CI)
}



library(memisc) #memsic is a package for working with survey data files
df <- spss.system.file("WellbeingFINAL.sav") #import the data file into an data.set object to view labels
codebook(df)
description(df)
mydata <- subset(df) #subset works as like as.data.frame


mutate(mydata, x = rowSums(select(mydata, contains("PWI"))))     


#Working out how to subset more effectively 
select(well_being_df, num_range("PWI", 1:7)) #Used to select a custom column 
x <- rowSums(select(well_being_df, num_range("PWI", 1:7))) #sum each row per observation

x <- well_being_df %>% #this works
  mutate(swb = rowSums(select(., num_range("PWI", 1:7))))

Personality <- c("Open", "Consciencious", "Extraversion", "Agreeable", "Neurotic")


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

well_being_df <- well_being_df %>% 
  mutate(pwi_index = rowSums(select(., contains("PWI"))))

rowSums(select(well_being_df, num_range("PWI", 1:7))) #means ###


#playing around with a function that could select rows as needed
myfunction <- function(x, y) {
  well_being_df %>% #this works
  mutate(x = rowSums(select(well_being_df, starts_with(y))))
}


# for each gender summarise the mean, sd, min and max values 1 = Male, 2 = Female and 3 = Transgender

Gender <- select(well_being_df, well_being_df$Gender) %>%
  mutate(Gender_ = labelled::to_factor(well_being_df$Gender)) %>%
  group_by(Gender_) %>%
  summarise_all(funs(mean, n = n(), sd, min(.,is.na = TRUE), max(.,is.na = TRUE)))

Gender #display gender table

