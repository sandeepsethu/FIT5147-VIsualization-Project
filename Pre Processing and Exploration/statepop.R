State_population <- read.csv("./STATE POPULATION.csv", header=T)


State_population$total_suicide_rate <- State_population$Total_suicide*100000/State_population$Total_Population

State_population$male_suicide_rate <- State_population$Male_Suicide*100000/State_population$Male_Population

State_population$female_suicide_rate <- State_population$Female_suicide*100000/State_population$Female_Population

write.csv(State_population,"STATE POPULATION.csv",row.names = FALSE)
