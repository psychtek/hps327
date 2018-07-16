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
  filter(Gender == 1)

Female <- select(well_being_df, Gender, Age, EmploymentStatus) %>% 
  filter(Gender == 2)

Trans <- select(well_being_df, Gender, Age, EmploymentStatus) %>% 
  filter(Gender == 3)

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

#What is the average PWI Score?
select(well_being_df, .vars = 4:10) %>%
  grouped_df(well_being_df, )

colSums(well_being_df[4:10])/169*10

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
          