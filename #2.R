


head(mydata_by_country)

#group data by state to find how many patents have each state
mydata_by_state <- mydata %>% 
  group_by(ee_state) %>%
  summarize(count = n())

#order the data in decreasing order
mydata_by_state <- mydata_by_state[order(mydata_by_state$count, decreasing = TRUE), ]


head(mydata_by_state)

#select top 4 states with the number of patents

top4st <- mydata_by_state [2:5,]

head(top4st)

df_st <- data.frame(top4st)

#graph the top 4 states and their patents
ggplot(data=df_st, aes(x=ee_state, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("State") +
  ylab("Number of Patents")+
  geom_text(aes(label= count ), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
