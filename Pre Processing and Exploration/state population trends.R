State_population <- read.csv("STATE POPULATION.csv", header=T)

head(State_population)


State_population$total_suicide_rate <- State_population$Total_suicide*100000/State_population$Total_Population

State_population$male_suicide_rate <- State_population$Male_Suicide*100000/State_population$Male_Population

State_population$female_suicide_rate <- State_population$Female_suicide*100000/State_population$Female_Population


ggplot(State_population, aes(Year)) + 
  geom_line(aes(y = total_suicide_rate))+
  geom_point(y = State_population$total_suicide_rate) +
  facet_wrap(~STATE.UT,scales = "free_y") + labs(x = "Year", y = "Suicide rate")+
  ggtitle("Statewise suicide rate (per 100K)")

ggplot(State_population, aes(Year)) + 
  geom_line(aes(y = total_suicide_rate))+
  geom_point(y = State_population$total_suicide_rate) +
  facet_wrap(~STATE.UT) + labs(x = "Year", y = "Suicide rate")+
  ggtitle("Statewise suicide rate (per 100K)")

ggplot(State_population, aes(Year)) + 
  geom_line(aes(y = total_suicide_rate, colour = "total suicide rate"))+
  geom_point(y = State_population$total_suicide_rate) +
  geom_line(aes(y = male_suicide_rate, colour = "male suicide rate")) + 
  geom_point(y = State_population$male_suicide_rate) +
  geom_line(aes(y = female_suicide_rate, colour = "female suicide rate"))+
  geom_point(y = State_population$female_suicide_rate) +
  facet_wrap(~STATE.UT,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")

agg1 <- aggregate(total_suicide_rate~STATE.UT, State_population, mean)
agg1 <-agg1[order(-agg1$total_suicide_rate),]

ggplot(agg1[1:7,], aes(x=reorder(STATE.UT,-total_suicide_rate), y=total_suicide_rate)) +
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Statewise highest mean suicide rate (per 100K)")+
  labs(x = "State", y = "Mean suicide rate")
ggplot(tail(agg1,7), aes(x=reorder(STATE.UT,total_suicide_rate), y=total_suicide_rate)) +
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Statewise lowest mean suicide rate (per 100K)")+
  labs(x = "State", y = "Mean suicide rate")
