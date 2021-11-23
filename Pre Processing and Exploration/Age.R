Indian_population$upto14_suicide_rate <- (ALL_INDIA_TOTAL$Male.upto.14.years+ALL_INDIA_TOTAL$Female.upto.14.years)*100000/(Indian_population$Male.upto.14.years+Indian_population$Female.upto.14.years)
Indian_population$suicide_rate15_29 <- (ALL_INDIA_TOTAL$Male.15.29.years+ALL_INDIA_TOTAL$Female.15.29.years)*100000/(Indian_population$Male.15.29.years+Indian_population$Female.15.29.years)
Indian_population$suicide_rate30_44 <- (ALL_INDIA_TOTAL$Male.30.44.years+ALL_INDIA_TOTAL$Female.30.44.years)*100000/(Indian_population$Male.30.44.years+Indian_population$Female.30.44.years)
Indian_population$suicide_rate45_59 <- (ALL_INDIA_TOTAL$Male.45.59.years+ALL_INDIA_TOTAL$Female.45.59.years)*100000/(Indian_population$Male.45.59.years+Indian_population$Female.45.59.years)
Indian_population$above60_suicide_rate <- (ALL_INDIA_TOTAL$Male.60.years.and.above+ALL_INDIA_TOTAL$Female.60.years.and.above)*100000/(Indian_population$Male.60.years.and.above+Indian_population$Female.60.years.and.above)


par(mfrow = c(2,2))

plot1<-ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = upto14_suicide_rate))+
  geom_point(y = Indian_population$upto14_suicide_rate) +
  labs(x = "Year", y = "Suicide rate")+
  geom_line(linetype="dashed",aes(y = mean(upto14_suicide_rate)))+
  ggtitle("Upto 14 years age group suicide rate (per 100K)")

plot2<-ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = suicide_rate15_29))+
  geom_point(y = Indian_population$suicide_rate15_29) +
  labs(x = "Year", y = "Suicide rate")+
  geom_line(linetype="dashed",aes(y = mean(suicide_rate15_29)))+
  ggtitle("15-29 years age group suicide rate (per 100K)")

plot3<-ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = suicide_rate30_44))+
  geom_point(y = Indian_population$suicide_rate30_44) +
  labs(x = "Year", y = "Suicide rate")+
  geom_line(linetype="dashed",aes(y = mean(suicide_rate30_44)))+
  ggtitle("30-44 years age group suicide rate (per 100K)")


plot4<-ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = suicide_rate45_59))+
  geom_point(y = Indian_population$suicide_rate45_59) +
  labs(x = "Year", y = "Suicide rate")+
  geom_line(linetype="dashed",aes(y = mean(suicide_rate45_59)))+
  ggtitle("45-59 years age group suicide rate (per 100K)")

plot5<-ggplot(Indian_population, aes(Time)) + 
  geom_line(aes(y = above60_suicide_rate))+
  geom_point(y = Indian_population$above60_suicide_rate) +
  labs(x = "Year", y = "Suicide rate")+
  geom_line(linetype="dashed",aes(y = mean(above60_suicide_rate)))+
  ggtitle("Above 60 years age group suicide rate (per 100K)")

plot_grid(plot1, plot2, plot3, plot4, plot5)


mean_for_graph2 <- data.frame(age_group=c("Upto 14 years", "15-29", "30-44","45-59", "Above 60"),
                             Mean_suicide_rate=c(mean((Indian_population$upto14_suicide_rate)), mean(Indian_population$suicide_rate15_29), mean(Indian_population$suicide_rate30_44),mean(Indian_population$suicide_rate45_59),mean(Indian_population$above60_suicide_rate)))

ggplot(mean_for_graph2, aes(x=reorder(age_group,-Mean_suicide_rate), y=Mean_suicide_rate)) +
  geom_bar(stat="identity")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Suicide rate (per 100K)")+
  labs(x = "Age Group", y = "Mean suicide rate")
