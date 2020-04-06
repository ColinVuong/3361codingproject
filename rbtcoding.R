#Colin Vuong's 3361 Coding Project


#Created on the 29 March 2020
#Background
#Dataset sourced from Bureau of Infrastructure, Transport and Regional Economics (BITRE)
#The data is about the number of random breath testing (RBT) conducted across Australia. 
#It was collected from 2008 to 2018.
#My goal is to see if there was a difference in detection positive RBT testing within each state . 
#I would also like to see if there is a difference when the data was collected earlier and
#more recently collected (i.e. if there was a difference between 2008-2012 and 2013-2018)

#First we need to load tidyverse
library(tidyverse)

#Then we will assign the dataset
rbtdata <- read_csv("bitre_enforcement_data-rbt.csv") #We are assigning the dataset as 'rbtdata'
#We see that there are 7 variables. I am interested in year, state, number of rbt tests and positive detection.
#We need to clean up the data.

#I would like to rename these variables as the following:
rbtdata <- rbtdata %>% 
  rename( 
    year = Year,                     #year the data was collected
    state = State,                   #state where the data was collected
    n_rbt = `RBT conducted`,         #total number of rbt conducted
    n_positiverbt = `Positive RBT`,  #number of rbt readings for BAC that was over the legal limit
    ) %>% 
  select(year, state, n_rbt, n_positiverbt) %>% 
  mutate(
    percrbt = ((n_positiverbt)/(n_rbt)*100)       #I would like to add a new variable, percentage of positive rbt detected
  )
  
#I want to plot per state
nsw1 <- rbtdata %>% 
  filter(state == "NSW", year < 2013) %>% #this collects data from 2008-2012
  summarise(m = mean(percrbt)) %>% #this averages the percentage of positive rbt readings
  mutate(year = "2008-2012") #i will rename the variable here so that it reflects the relevant time range
nsw2 <- rbtdata %>% 
  filter(state == "NSW", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
nswall <- bind_rows(nsw1, nsw2) #I have combined the summarised data from both time ranges

#I shall plot the combined data (nswall) to verify if the above code is accurate
ggplot(
  data = nswall, 
  mapping = aes(
    x = year, y = m
    )) + 
  geom_col()

#Now I would like to the something similar to compare the data in Queensland
qld1 <- rbtdata %>% 
  filter(state == "Qld", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
qld2 <- rbtdata %>% 
  filter(state == "Qld", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
qldall <- bind_rows(qld1, qld2)

#Next I want to start plotting QLD and NSW data, comparing the two side-by-side
nswall <- nswall %>% 
  mutate(state = "NSW") #We add a new variable to signify that the data is from NSW
qldall <- qldall %>% 
  mutate(state = "QLD")

rbtplot <- bind_rows(nswall,qldall) #I am combining the two sets of data

#Now we plot:
ggplot(data = rbtplot, aes(x= state, 
                           y = m, 
                           fill = year
                           )) +
  geom_bar(stat = "identity", 
           position = position_dodge()
           )

#So this gives us a nice comparison of how NSW and QLD are doing in detecting positive rbt's. 
#Next I would want to compare other states (excluding Vic as there were no data collection)
#We can copy and change the codes from NSW and Qld's to achieve this:
#ACT
act1 <- rbtdata %>% 
  filter(state == "ACT", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
act2 <- rbtdata %>% 
  filter(state == "ACT", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
actall <- bind_rows(act1, act2)

#NT
nt1 <- rbtdata %>% 
  filter(state == "NT", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
nt2 <- rbtdata %>% 
  filter(state == "NT", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
ntall <- bind_rows(nt1, nt2)

#SA
sa1 <- rbtdata %>% 
  filter(state == "SA", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
sa2 <- rbtdata %>% 
  filter(state == "SA", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
saall <- bind_rows(sa1, sa2)

#Tas
tas1 <- rbtdata %>% 
  filter(state == "Tas", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
tas2 <- rbtdata %>% 
  filter(state == "Tas", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
tasall <- bind_rows(tas1, tas2)

#WA
wa1 <- rbtdata %>% 
  filter(state == "WA", year < 2013) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2008-2012") 
wa2 <- rbtdata %>% 
  filter(state == "WA", year > 2012) %>% 
  summarise(m = mean(percrbt)) %>% 
  mutate(year = "2013-2018")
waall <- bind_rows(wa1, wa2)

#Then we should add a new column to signify the origin of data
actall <- actall %>% 
  mutate(state = "ACT") 
ntall <- ntall %>% 
  mutate(state = "NT")
saall <- saall %>% 
  mutate(state = "SA")
tasall <- tasall %>% 
  mutate(state = "TAS")
waall <- waall %>% 
  mutate(state = "WA")

#Now we need to add all the states into our variable rbtplot
rbtplot <- bind_rows(nswall, qldall, actall, ntall, saall, tasall, waall)

#Now plotting all the states:
ggplot(data = rbtplot, aes(x= state, 
                           y = m, 
                           fill = year
)) +
  geom_bar(stat = "identity", 
           position = position_dodge()
  ) + labs(
    title = "Trends in positive RBT detection across Australia",
    x = "State",
    y = "Proportion of positive RBT (%)"
  )

