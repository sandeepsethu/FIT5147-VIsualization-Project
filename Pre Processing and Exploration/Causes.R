Causes_2001_12<- read.csv("Causes 2001-12.csv", header=T)
Causes_2013<- read.csv("Causes 2013.csv", header=T)
Causes_2014<- read.csv("Causes 2014.csv", header=T)

unique(Causes_2001_12[c("STATE.UT")])
unique(Causes_2013[c("STATE.UT")])
unique(Causes_2014[c("STATE.UT")])

Causes_2014$STATE.UT <- toupper(Causes_2014$STATE.UT)
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "JAMMU AND KASHMIR"]<- "JAMMU & KASHMIR"
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "A AND N ISLANDS"]<- "A & N ISLANDS"
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "DANDN HAVELI"]<- "D & N HAVELI"
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "DAMAN AND DIU"]<- "DAMAN & DIU"
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "DELHI UT"]<- "DELHI (UT)"
Causes_2014$STATE.UT[Causes_2014$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"

Causes <- rbind(Causes_2001_12,Causes_2013,Causes_2014)

Causes$CAUSE <- toupper(Causes$CAUSE)
unique(Causes[c("CAUSE")])
#removing redundant aggregate
Causes<-Causes[!(Causes$CAUSE=="TOTAL ILLNESS"),]
#clustering different causes
Causes$CAUSE[Causes$CAUSE== "BANKRUPTCY OR SUDDEN CHANGE IN ECONOMIC" | Causes$CAUSE=="BANKRUPTCY OR SUDDEN CHANGE IN ECONOMIC STATUS" | Causes$CAUSE=="BANKRUPTCY OR INDEBTEDNESS" | Causes$CAUSE=="POVERTY" | Causes$CAUSE=="PROPERTY DISPUTE" | Causes$CAUSE=="UNEMPLOYMENT" | Causes$CAUSE=="PROFESSIONAL/CAREER PROBLEM"]<- "ECONOMIC ISSUES"
Causes$CAUSE[Causes$CAUSE=="CANCELLATION/NON-SETTLEMENT OF MARRIAGE" | Causes$CAUSE=="SUSPECTED/ILLICIT RELATION" | Causes$CAUSE=="EXTRA MARITAL AFFAIRS" | Causes$CAUSE=="MARRIAGE RELATED ISSUES" | Causes$CAUSE=="NON SETTLEMENT OF MARRIAGE" | Causes$CAUSE=="DIVORCE" | Causes$CAUSE=="NOT HAVING CHILDREN(BARRENNESS/IMPOTENCY" | Causes$CAUSE=="NOT HAVING CHILDREN (BARRENNESS/IMPOTENCY" | Causes$CAUSE=="NOT HAVING CHILDREN (BARRENNESS/IMPOTENCY)" | Causes$CAUSE=="IMPOTENCY/INFERTILITY"] <-"MARITAL ISSUES"
Causes$CAUSE[Causes$CAUSE=="CANCER"| Causes$CAUSE=="PARALYSIS" | Causes$CAUSE=="TOTAL ILLNESS" | Causes$CAUSE=="OTHER PROLONGED ILLNESS" | Causes$CAUSE=="ILLNESS (AIDS/STD)" | Causes$CAUSE=="ILLNESS" | Causes$CAUSE=="AIDS/STD"] <- "HEALTH ISSUES"
Causes$CAUSE[Causes$CAUSE=="DOWRY RELATED ISSUES"] <- "DOWRY DISPUTE"
Causes$CAUSE[Causes$CAUSE=="OTHER FAMILY PROBLEMS"] <- "FAMILY PROBLEMS"
Causes$CAUSE[Causes$CAUSE=="PHYSICAL ABUSE (RAPE; ETC.)"] <- "PHYSICAL ABUSE"
Causes$CAUSE[Causes$CAUSE=="OTHER CAUSES (PLEASE SPECITY)" | Causes$CAUSE=="OTHER CAUSES (PLEASE SPECIFY)" | Causes$CAUSE=="OTHER CAUSES"] <- "OTHERS"
Causes$CAUSE[Causes$CAUSE=="IDEOLOGICAL CAUSES/HERO WORSHIPPING"] <- "HERO WORSHIP"
Causes$CAUSE[Causes$CAUSE=="FALL IN SOCIAL REPUTATION"] <- "SOCIETAL ISSUES"
Causes$CAUSE[Causes$CAUSE=="FAILURE IN EXAMINATION"] <- "EXAM FAILURE"
Causes$CAUSE[Causes$CAUSE=="DRUG ABUSE/ADDICTION"] <- "DRUG ABUSE"
Causes$CAUSE[Causes$CAUSE=="INSANITY/MENTAL ILLNESS"] <- "MENTAL HEALTH"

#aggregating the clusters
aggregate_causes <- aggregate(.~STATE.UT+Year+CAUSE, Causes, sum)


unique(Causes[c("CAUSE")])

ALL_INDIA_CAUSES_TOTAL <- aggregate_causes %>% filter(STATE.UT== "TOTAL (ALL INDIA)",!CAUSE=="TOTAL")

aggregate_causes %>% filter(STATE.UT== "TOTAL (ALL INDIA)", Year==2001)
p <- ggplot(data = ALL_INDIA_CAUSES_TOTAL, aes(x = Year)) + geom_line(aes(y = Total.Male, color = "Male")) + geom_line(aes(y = Total.Female, color="Female")) + geom_line(aes(y = Grand.Total, color="Total"))  
p + facet_wrap(~CAUSE) + labs(x = "Year", y = "Total number of suicides")  + ggtitle("Number of suicides per different cause")

p + facet_wrap(~CAUSE,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")

write.csv(aggregate_causes,"agg_causes.csv")

random <- Causes[15386:15391,]
aggregate(.~STATE.UT+Year+CAUSE, random, sum)


Causes<-Causes[!(Causes$CAUSE=="TOTAL"),]

Causes<-aggregate(.~STATE.UT+Year+CAUSE, Causes, sum)
write.csv(Causes, "causes.csv")
