library(tidyverse)
library(car)
library(olsrr)
library(effects)

# Original model
hospital <- read_csv(here::here("Hospitals.csv"))
hosp_orig <- lm(LenStay ~ Age+InfRisk+Culture+XRay+
                  School+Region+Beds+Census+Nurses+Services, 
                data=hospital)
summary(hosp_orig)
residualPlots(hosp_orig,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(hosp_orig,envelope=FALSE,pch=16,id=FALSE)
shapiro.test(rstudent(hosp_orig))
ols_vif_tol(hosp_orig)
influenceIndexPlot(hosp_orig,vars="Cook",id=FALSE)
qf(0.5,13,100)


# Model with transformed/new variables
hospital$bedprop <- hospital$Beds/hospital$Census
hospital$nurseprop <- hospital$Nurses/hospital$Census
hosp1 <- lm(log(LenStay) ~ Age+InfRisk+log(Culture)+XRay+School+
              Region+bedprop+Census+nurseprop+Services,
            data=hospital)
summary(hosp1)
residualPlots(hosp1,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(hosp1,envelope=FALSE,pch=16,id=FALSE)
shapiro.test(rstudent(hosp1))
ols_vif_tol(hosp1)
influenceIndexPlot(hosp1,vars="Cook",id=FALSE)


# Model with significant interactions
hosp_int <- lm(log(LenStay) ~ Age+InfRisk+log(Culture)+XRay+School+
                 Region+bedprop+Census+nurseprop+Services+InfRisk*Census,
               data=hospital)
summary(hosp_int)
residualPlots(hosp_int,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(hosp_int,envelope=FALSE,pch=16,id=FALSE)
shapiro.test(rstudent(hosp_int))


# Best Model
best.hosp <- ols_step_forward_aic(hosp_int)
best.hosp[["predictors"]]
hosp_best <- lm(log(LenStay) ~ Age+Region+XRay+nurseprop+Census+InfRisk+InfRisk*Census,
              data=hospital)
summary(hosp_best)

new_hosp_best <- lm(log(LenStay) ~ Region+XRay+nurseprop+Census+InfRisk+InfRisk*Census,
                    data=hospital)
summary(new_hosp_best)
Anova(new_hosp_best)
residualPlots(new_hosp_best,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(new_hosp_best,envelope=FALSE,pch=16,id=FALSE)
shapiro.test(rstudent(new_hosp_best))
ols_vif_tol(new_hosp_best)
influenceIndexPlot(new_hosp_best,vars="Cook",id=FALSE)
qf(0.5,9,104)
anova(new_hosp_best,hosp_int)
