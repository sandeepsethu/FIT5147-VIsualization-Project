Social_2001_12<- read.csv("Social Status 2001-12.csv", header=T)
Social_2013<- read.csv("Social Status 2013.csv", header=T)
Social_2014<- read.csv("Social Status 2014.csv", header=T)

unique(Social_2001_12[c("STATE.UT")])
unique(Social_2013[c("STATE.UT")])
unique(Social_2014[c("STATE.UT")])
Social_2014$STATE.UT <- toupper(Social_2014$STATE.UT)
Social_2014$STATE.UT[Social_2014$STATE.UT== "D&N HAVELI"]<- "D & N HAVELI"
Social_2014$STATE.UT[Social_2014$STATE.UT== "DELHI UT"]<- "DELHI (UT)"
Social_2014$STATE.UT[Social_2014$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"

Social <- rbind(Social_2001_12,Social_2013,Social_2014)

head(Social)

unique(Social[c("CAUSE")])
Social$CAUSE[Social$CAUSE== "Un-Married"]<- "Never Married"
Social$CAUSE[Social$CAUSE== "Seperated"]<- "Separated"


ALL_INDIA_STATUS_TOTAL <- Social %>% filter(STATE.UT== "TOTAL (ALL INDIA)", !(CAUSE=="Others"|CAUSE=="Status not known"|CAUSE=="Total"))


p <- ggplot(data = ALL_INDIA_STATUS_TOTAL, aes(x = Year)) + geom_line(aes(y = Male), color = "darkred") + geom_line(aes(y = Female), color="steelblue") + geom_line(aes(y = Total), color="black")  
p + facet_wrap(~CAUSE) + labs(x = "Year", y = "Total number of suicides")

p + facet_wrap(~CAUSE,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")



Social <- Social %>% filter(!CAUSE=="Total")
Social <- read.csv("./social.csv", header=T)
Social<- Social[-c(7:9)]
Social$Total <- Social$Male+Social$Female

write.csv(Social, "social.csv", row.names = FALSE)

