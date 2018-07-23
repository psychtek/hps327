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
  mutate(Gender = labelled::to_factor(Gender) %>%
  group_by(Gender) %>%
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

well_being_df %>% mutate_at(.funs = funs(age = ./Affect1), .vars = 4:10)


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


Gender <- well_being_df$Gender        

#Trying to mutatue PWI and add rows
well_being_df2 %>% mutate(personality = rowSums(well_being_df2[.11:60]))
#Error in mutate_impl(.data, dots) : 
  Evaluation error: Column indexes must be integer, not 0.11, 1.11, 2.11, 
3.11, 4.11, 5.11, 6.11, 7.11, 8.11, 9.11, 10.11, 11.11, 12.11, 13.11, 14.11, 
15.11, 16.11, 17.11, 18.11, 19.11, 20.11, 21.11, 22.11, 23.11, 24.11, 25.11, 
26.11, 27.11, 28.11, 29.11, 30.11, 31.11, 32.11, 33.11, 34.11, 35.11, 36.11, 
37.11, 38.11, 39.11, 40.11, 41.11, 42.11, 43.11, 44.11, 45.11, 46.11, 47.11, 
48.11, 49.11, 50.11, 51.11, 52.11, 53.11, 54.11, 55.11, 56.11, 57.11, 58.11, 59.11.

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


library(memisc) #memsic is a package for working with survey data files
df <- spss.system.file("WellbeingFINAL.sav") #import the data file into an data.set object to view labels
codebook(df)
description(df)
mydata <- subset(df) #subset works as like as.data.frame


mutate(mydata, x = rowSums(select(mydata, contains("PWI"))))     
