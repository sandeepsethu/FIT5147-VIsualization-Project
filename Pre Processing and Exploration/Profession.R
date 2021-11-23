Profession_2001_12<- read.csv("Profession 2001-12.csv", header=T)
Profession_2013<- read.csv("Profession 2013.csv", header=T)
Profession_2014<- read.csv("Profession 2014.csv", header=T)

unique(Profession_2001_12[c("STATE.UT")])
unique(Profession_2013[c("STATE.UT")])
unique(Profession_2014[c("STATE.UT")])
Profession_2014$STATE.UT <- toupper(Profession_2014$STATE.UT)
Profession_2014$STATE.UT[Profession_2014$STATE.UT== "D&N HAVELI"]<- "D & N HAVELI"
Profession_2014$STATE.UT[Profession_2014$STATE.UT== "DELHI UT"]<- "DELHI (UT)"
Profession_2014$STATE.UT[Profession_2014$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"

Profession <- rbind(Profession_2001_12,Profession_2013,Profession_2014)

head(Profession)
unique(Profession[c("CAUSE")])

Profession$CAUSE[Profession$CAUSE== "Housewife"]<- "House Wife"
Profession$CAUSE[Profession$CAUSE== "Govt. Servants (Total)"]<- "Service (Government)"
Profession$CAUSE[Profession$CAUSE== "Private Sector Enterprises"]<- "Service (Private)"
Profession$CAUSE[Profession$CAUSE== "Student"]<- "Students"
Profession$CAUSE[Profession$CAUSE== "Unemployed Persons"]<- "Unemployed"
Profession$CAUSE[Profession$CAUSE== "Self-employed (Business activity)"]<- "Business (Total)"
Profession$CAUSE[Profession$CAUSE== "Farming/Agriculture Activity"]<- "Agriculture (Total)"
Profession$CAUSE[Profession$CAUSE== "Retired Person"]<- "Retired Persons"
Profession$CAUSE[Profession$CAUSE== "Profession/Salaried Persons (Total)"]<- "Total Salaried"
Profession$CAUSE[Profession$CAUSE== "Self-employed Persons (Total)"]<- "Total Self-employed"
Profession<-Profession[!(Profession$CAUSE=="Agricultural Labourers" | Profession$CAUSE=="Total Self-employed" |Profession$CAUSE=="Total Salaried" |Profession$CAUSE=="Farmers (Sub Total)" | Profession$CAUSE=="Central/UT Govt. Servants" | Profession$CAUSE=="State Govt. Servants" | Profession$CAUSE=="Other Statutory Body/etc." | Profession$CAUSE=="Vendor" | Profession$CAUSE=="Tradesmen"),]

ALL_INDIA_PROF_TOTAL <- Profession %>% filter(STATE.UT== "TOTAL (ALL INDIA)", CAUSE=="Professional Activity"| CAUSE=="Public Sector Undertaking"| CAUSE== "Unemployed"|CAUSE== "House Wife"| CAUSE=="Retired Persons" |CAUSE== "Unemployed" | CAUSE=="Students" | CAUSE=="Agriculture (Total)" | CAUSE=="Total Salaried" | CAUSE=="Total Self-employed" |CAUSE=="Service (Government)"|CAUSE=="Service (Private)")

p <- ggplot(data = ALL_INDIA_PROF_TOTAL, aes(x = Year)) + geom_line(aes(y = Total.Male, color = "Male"))+ geom_line(aes(y = Total.Female, color="Female")) + geom_line(aes(y = Grand.Total, color="Total"))
p + facet_wrap(~CAUSE) + labs(x = "Year", y = "Total number of suicides")+
  ggtitle("Profession wise number of suicides")

p + facet_wrap(~CAUSE,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")



profession <- Profession %>% filter(CAUSE=="Professional Activity"| CAUSE=="Public Sector Undertaking"| CAUSE== "Unemployed"|CAUSE== "House Wife"| CAUSE=="Retired Persons" |CAUSE== "Unemployed" | CAUSE=="Students" | CAUSE=="Agriculture (Total)" | CAUSE=="Total Salaried" | CAUSE=="Total Self-employed" |CAUSE=="Service (Government)"|CAUSE=="Service (Private)")

profession<- profession[-c(16:22)]

profession$Total.upto.14.years <- profession$Male.upto.14.years+profession$Female.upto.14.years
profession$Total.15.29.years <- profession$Male.15.29.years+profession$Female.15.29.years
profession$Total.30.44.years <- profession$Male.30.44.years+profession$Female.30.44.years
profession$Total.45.59.years <- profession$Male.45.59.years+profession$Female.45.59.years
profession$Total.60.years.and.above <- profession$Male.60.years.and.above+profession$Female.60.years.and.above
profession$Total <- profession$Total.Male+profession$Total.Female
write.csv(profession,"Profession.csv",row.names = FALSE)
