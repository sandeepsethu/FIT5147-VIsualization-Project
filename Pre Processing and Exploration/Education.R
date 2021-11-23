#reading csv files for 2001-12,2013,2014
Education_2001_12<- read.csv("Educational Status 2001-12.csv", header=T)
Education_2013<- read.csv("Educational Status 2013.csv", header=T)
Education_2014<- read.csv("Educational Status 2014.csv", header=T)

#encoding for Transgender people
Education_2001_12$Transgender=0
Education_2013$Transgender=0

#reordering the columns
Education_2001_12 <- Education_2001_12[, colnames(Education_2001_12)[c(1:5,7,6)]]
Education_2013 <- Education_2013[, colnames(Education_2013)[c(1:5,7,6)]]

#getting column names from Education status 2001-12 dataframe
Education_colnames <- c(names(Education_2001_12))

#setting column names for 2013,2014 dataframes
colnames(Education_2013) <- Education_colnames
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
#changing all the names of states to upper case
Education_2014$STATE.UT <- toupper(Education_2014$STATE.UT)

#changing value of row/name of states based on condition
Education_2014$STATE.UT[Education_2014$STATE.UT== "JAMMU AND KASHMIR"]<- "JAMMU & KASHMIR"
Education_2014$STATE.UT[Education_2014$STATE.UT== "ANDAMAN AND NICOBAR"]<- "A & N ISLANDS"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DADRA AND NAGAR HAVELI"]<- "D & N HAVELI"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DAMAN AND DIU"]<- "DAMAN & DIU"
Education_2014$STATE.UT[Education_2014$STATE.UT== "DELHI"]<- "DELHI (UT)"
Education_2014$STATE.UT[Education_2014$STATE.UT== "TOTAL (UTS)"]<- "TOTAL (UTs)"


Education <- rbind(Education_2001_12,Education_2013,Education_2014)

head(Education)

unique(Education[c("CAUSE")])

#changing value of row/educational status based on condition
Education$CAUSE[Education$CAUSE== "Primary (upto class-5)"]<- "Primary"
Education$CAUSE[Education$CAUSE== "Middle (upto class-8)"]<- "Middle"
Education$CAUSE[Education$CAUSE== "Matriculate/Secondary (upto class-10)"]<- "Matriculate/Secondary"
Education$CAUSE[Education$CAUSE== "Hr. Secondary/Intermediate/Pre-Universit" | Education$CAUSE== "Higher Secondary/ Intermediate/ Pre-University (upto class-12)"]<- "Hr. Secondary/Intermediate/Pre-University"
Education$CAUSE[Education$CAUSE== "Diploma/ITI/Certificate"]<- "Diploma"
Education$CAUSE[Education$CAUSE== "Graduate and above"]<- "Graduate"
Education$CAUSE[Education$CAUSE== "Professionals (MBA; etc.)"]<- "Post Graduate and Above"



ALL_INDIA_EDU_TOTAL <- Education %>% filter(STATE.UT== "TOTAL (ALL INDIA)", !CAUSE== "Total", !CAUSE=="Status not known")


p <- ggplot(data = ALL_INDIA_EDU_TOTAL, aes(x = Year)) + geom_line(aes(y = Male, color = "Male"))+ geom_line(aes(y = Female, color="Female")) + geom_line(aes(y = Total, color="Total"))
p + facet_wrap(~CAUSE) + labs(x = "Year", y = "Total number of suicides")+
  ggtitle("Number of suicides per education level")

p <- ggplot(data = ALL_INDIA_EDU_TOTAL, aes(x = Year, y = Total)) + geom_point()
p + facet_wrap(~CAUSE,scales = "free_y") + labs(x = "Year", y = "Total number of suicides")

education <- Education %>% filter(!CAUSE== "Total", !CAUSE=="Status not known")

education<- education[-c(6:8)]


education$Total <- education$Male+education$Female
write.csv(education,"Education.csv",row.names = FALSE)
