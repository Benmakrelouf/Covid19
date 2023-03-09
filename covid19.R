library(readxl)
#### Epidemiological Dataset COVID Algeria.
covid19 <- read_excel("MAMP/htdocs/Formation U.d'Oran 1/Panel Covid19.Tipasa.xlsx", 
                        sheet = "Covid.positif")
str(covid19)

#### Code all variables as factors.
covid19=lapply(covid19, as.factor)
str(covid19)
#### Code the variable "age" as numeric.
covid19<- transform(covid19,Age = as.numeric(Age))

#### Remove rows with missing values.
covid19=na.omit(covid19)
str(covid19)

attach(covid19)
#### Using GLM to model the variable "S.Finale".
modelCovid=glm(S.Finale~Sexe+HTA+Diabète, data=covid19, family = binomial())
summary(modelCovid)


#### The odds ratio can be interpreted, 
# The probability of a success changes exp(cB_1) times for each unit increase c of x
# exp(coef(modelCovid))

#### values of Y predicted by the model.
fit = predict(modelCovid, covid19)
fit

#Probabilities of events.  
fit_prob <- exp(fit)/(1+exp(fit))
fit_prob

Proba=data.frame(S.Finale,Sexe,HTA,Diabète,fit_prob)
Proba

library(dplyr) ## Apply a filter to the data. 
Proba %>%
  filter(S.Finale=="Guéri"  & Sexe == "M" & HTA=="0" & Diabète=="0")

## probability of dying with diabetes knowing sex and hypertension. 
Proba %>%filter(S.Finale=="décédé" & Diabète=="1") 

