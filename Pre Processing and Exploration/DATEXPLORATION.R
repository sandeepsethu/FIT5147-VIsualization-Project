Education_2001_12<- read.csv("Educational Status 2001-12.csv", header=T)
Education_2001_12$Transgender=0
Education_2001_12 <- Education_2001_12[, colnames(Education_2001_12)[c(1:5,7,6)]]
Education_colnames <- c(names(Education_2001_12))
Education_colnames 
Education_2013<- read.csv("Educational Status 2013.csv", header=T)
Education_2013$Transgender=0
Education_2013 <- Education_2013[, colnames(Education_2013)[c(1:5,7,6)]]
colnames(Education_2013) <- Education_colnames
Education_2014<- read.csv("Educational Status 2014.csv", header=T)
colnames(Education_2014) <- Education_colnames


unique(Education_2001_12[c("STATE.UT")])
unique(Education_2013[c("STATE.UT")])
Education_2013$STATE.UT <- toupper(Education_2013$STATE.UT)
Education_2013$STATE.UT[Education_2013$STATE.UT== "JAMMU AND KASHMIR"]<- "JAMMU & KASHMIR"
Education_2013$STATE.UT[Education_2013$STATE.UT== "ANDAMAN AND NICOBAR"]<- "A & N ISLANDS"
Education_2013$STATE.UT[Education_2013$STATE.UT== "DADRA AND NAGAR HAVELI"]<- "D & N HAVELI"
Education_2013$STATE.UT[Education_2013$STATE.UT== "DAMAN AND DIU"]<- "DAMAN & DIU"
Education_2013$STATE.UT[Education_2013$STATE.UT== "DELHI"]<- "DELHI (UT)"
Education_2013$STATE.UT[Education_2013$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"

unique(Education_2014[c("STATE.UT")])
Education_2014$STATE.UT <- toupper(Education_2014$STATE.UT)
Education_2014$STATE.UT[Education_2014$STATE.UT== "JAMMU AND KASHMIR"]<- "JAMMU & KASHMIR"
Education_2014$STATE.UT[Education_2014$STATE.UT== "ANDAMAN AND NICOBAR"]<- "A & N ISLANDS"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DADRA AND NAGAR HAVELI"]<- "D & N HAVELI"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DAMAN AND DIU"]<- "DAMAN & DIU"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DELHI"]<- "DELHI (UT)"
Education_2014$STATE.UT[Education_2014$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"


Education <- rbind(Education_2001_12,Education_2013,Education_2014)



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


ALL_INDIA_TOTAL <- Profession %>% filter(STATE.UT== "TOTAL (ALL INDIA)",CAUSE=="Total")

SUMMARIZED_TOTAL <- Profession %>% filter(CAUSE=="Total")

plot(ALL_INDIA_TOTAL$Year,ALL_INDIA_TOTAL$Grand.Total,type="b")


p <- ggplot(data = SUMMARIZED_TOTAL, aes(x = Year, y = Grand.Total)) + geom_point()
p + facet_wrap(~STATE.UT,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")

Causes$CAUSE <- toupper(Causes$CAUSE)
unique(Causes[c("CAUSE")])

Causes$CAUSE[Causes$CAUSE== "BANKRUPTCY OR SUDDEN CHANGE IN ECONOMIC" | Causes$CAUSE=="BANKRUPTCY OR SUDDEN CHANGE IN ECONOMIC STATUS"]<- "BANKRUPTCY OR INDEBTEDNESS"
Causes$CAUSE[Causes$CAUSE=="SUSPECTED/ILLICIT RELATION"] <- "EXTRA MARITAL AFFAIRS"
Causes$CAUSE[Causes$CAUSE=="CANCELLATION/NON-SETTLEMENT OF MARRIAGE" | Causes$CAUSE=="MARRIAGE RELATED ISSUES" | Causes$CAUSE=="NON SETTLEMENT OF MARRIAGE"] <-"DIVORCE"
Causes$CAUSE[Causes$CAUSE=="NOT HAVING CHILDREN(BARRENNESS/IMPOTENCY" | Causes$CAUSE=="NOT HAVING CHILDREN (BARRENNESS/IMPOTENCY" | Causes$CAUSE=="NOT HAVING CHILDREN (BARRENNESS/IMPOTENCY)"] <-"IMPOTENCY/INFERTILITY"
Causes$CAUSE[Causes$CAUSE=="ILLNESS (AIDS/STD)"] <- "AIDS/STD"
Causes$CAUSE[Causes$CAUSE=="PARALYSIS" | Causes$CAUSE=="TOTAL ILLNESS" | Causes$CAUSE=="OTHER PROLONGED ILLNESS"] <- "ILLNESS"
Causes$CAUSE[Causes$CAUSE=="DOWRY RELATED ISSUES"] <- "DOWRY DISPUTE"
Causes$CAUSE[Causes$CAUSE=="OTHER FAMILY PROBLEMS"] <- "FAMILY PROBLEMS"
Causes$CAUSE[Causes$CAUSE=="PHYSICAL ABUSE (RAPE; ETC.)"] <- "PHYSICAL ABUSE (RAPE,INCEST ETC.)"
Causes$CAUSE[Causes$CAUSE=="OTHER CAUSES (PLEASE SPECITY)" | Causes$CAUSE=="OTHER CAUSES (PLEASE SPECIFY)" | Causes$CAUSE=="OTHER CAUSES" | Causes$CAUSE=="CAUSES NOT KNOWN"] <- "OTHERS"


Causes %>% filter(STATE.UT== "TOTAL (ALL INDIA)",CAUSE=="Total")



