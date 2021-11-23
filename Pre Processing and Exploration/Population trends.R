Indian_population <- read.csv("Indian Population.csv", header=T)

head(ALL_INDIA_TOTAL)


Indian_population$total_suicide_rate <- ALL_INDIA_TOTAL$Grand.Total*100000/Indian_population$Grand.Total

Indian_population$male_suicide_rate <- ALL_INDIA_TOTAL$Total.Male*100000/Indian_population$Total.Male

Indian_population$female_suicide_rate <- ALL_INDIA_TOTAL$Total.Female*100000/Indian_population$Total.Female

Indian_HDI
mean(Indian_population$total_suicide_rate)
trend.test(Indian_population$total_suicide_rate, R=1)

cor(Indian_population$Time, Indian_population$total_suicide_rate, method = "pearson")

ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = total_suicide_rate, colour = "total suicide rate"))+
  geom_point(y = Indian_population$total_suicide_rate) +
  geom_line(linetype="dashed",aes(y = mean(total_suicide_rate), colour = "mean total suicide rate"))+
  ggtitle("Suicide rate over time")+
  labs(x = "Year", y = "Suicide rate")

ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = male_suicide_rate, colour = "male suicide rate")) + 
  geom_point(y = Indian_population$male_suicide_rate) +
  geom_line(linetype="dashed",aes(y = mean(male_suicide_rate), colour = "mean male suicide rate"))+
  geom_line(aes(y = total_suicide_rate, colour = "total suicide rate"))+
  geom_point(y = Indian_population$total_suicide_rate) +
  geom_line(linetype="dashed",aes(y = mean(total_suicide_rate), colour = "mean total suicide rate"))+
  geom_line(aes(y = female_suicide_rate, colour = "female suicide rate"))+
  geom_point(y = Indian_population$female_suicide_rate) +
  geom_line(linetype="dashed",aes(y = mean(female_suicide_rate), colour = "mean female suicide rate"))+
  ggtitle("Suicide rate over time")+
  labs(x = "Year", y = "Suicide rate")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

mean_for_graph <- data.frame(Sex=c("Male", "Female", "Total"),
                     Mean_suicide_rate=c(mean((Indian_population$male_suicide_rate)), mean(Indian_population$female_suicide_rate), mean(Indian_population$total_suicide_rate)))

ggplot(mean_for_graph, aes(x=Sex, y=Mean_suicide_rate)) +
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("National suicide rate (per 100K)")+
  labs(x = "Sex", y = "Mean suicide rate")

Indian_population$male_suicide_rate/Indian_population$female_suicide_rate



