####Sensitivity and specificity of rapid tests#####

#False negative rate
merged.data %>%
  filter(Grouping=="POC Test" & Lab.Result.Interpretation=="NEGATIVE" & `Rapid Order Confirmatory PCR Ordered`==1) -> POC.neg

table(POC.neg$`Rapid Order Confirmatory PCR Result`, useNA = "ifany") #6.1%

#%symptomatic
table(POC.neg$`Rapid Order Confirmatory PCR Result`,POC.neg$`Rapid Order Patient Symptomatic`, useNA = "ifany")
quantile(POC.neg$Age[POC.neg$`Rapid Order Confirmatory PCR Result`=="POSITIVE"])

IQR(POC.neg$Age)

#False negative rate by month
POC.neg %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
5414/ 74275

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))

3138/29176
POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
7135/79232

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
4362/34793

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
5992/70335

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
3682/31780


POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))

8858/94470

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))

5683/45954


POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
5333/74360

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
3362/41113

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))

1289/49258

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
803/29559

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
428/38108

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
281/21043

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
730/31537

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.neg %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="POSITIVE")%>%
  summarise(n=n_distinct(PatientID))
485/17636


## False positive rate 
merged.data %>%
  filter(Grouping=="POC Test" & Lab.Result.Interpretation=="POSITIVE" & `Rapid Order Confirmatory PCR Ordered`==1) -> POC.pos

table(POC.pos$`Rapid Order Confirmatory PCR Result`, POC.pos$`Rapid Order Patient Symptomatic`, useNA = "ifany") #11.9%
quantile(POC.pos$Age[POC.pos$`Rapid Order Confirmatory PCR Result`=="NEGATIVE"])

#False positive rate by month
POC.pos %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
379/ 2953

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2020-12-01" & Adjusted.Visit.Date <= "2020-12-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
42/980


POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
410/3618

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-01-01" & Adjusted.Visit.Date <= "2021-01-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
57/1136

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
286/2565

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-02-01" & Adjusted.Visit.Date <= "2021-02-28" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
49/889

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))

328/3430

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes") %>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-03-01" & Adjusted.Visit.Date <= "2021-03-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes") %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
62/1343

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
160/1706

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-04-01" & Adjusted.Visit.Date <= "2021-04-30" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
28/672

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))

59/415

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-05-01" & Adjusted.Visit.Date <= "2021-05-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
17/184



POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
62/283

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-06-01" & Adjusted.Visit.Date <= "2021-06-30" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))

22/141

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" )%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" ) %>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
30/513

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" )%>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  summarise(n=n_distinct(PatientID))

POC.pos %>%
  filter(Adjusted.Visit.Date >= "2021-07-01" & Adjusted.Visit.Date <= "2021-07-31" ) %>%
  filter(`Rapid Order Patient Symptomatic`=="Yes")%>%
  filter(`Rapid Order Confirmatory PCR Result`=="NEGATIVE")%>%
  summarise(n=n_distinct(PatientID))
9/347


#median age for all POC negative and POS
quantile(merged.data$Age[merged.data$Grouping=="POC Test" & merged.data$Lab.Result.Interpretation=="POSITIVE"])

quantile(merged.data$Age[merged.data$Grouping=="POC Test" & merged.data$Lab.Result.Interpretation=="NEGATIVE"])
