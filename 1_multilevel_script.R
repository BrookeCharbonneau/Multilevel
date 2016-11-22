library(apaTables)
library(tidyverse)
library(nlme)

#load data
indiv.data <- read_csv("bh1996.csv")
# View(indiv.data)


#let the computer know how to group your data
indiv.grouped <- group_by(indiv.data, GRP)

#create columns with mean hours and n of the group
group.data <- indiv.grouped %>% summarise(HRS.GRP=mean(HRS, na.rm=TRUE), N.GRP=n())
# View(group.data)

#combine two data files for multilevel analysis
multilevel.data <- full_join(indiv.data, group.data, by="GRP")
#View(multilevel.data)
#have some colums at individual level and some at group level (repeated for each individual)


#compare these two - how well mean of wellbeing predicts everyone's scores (ignoring groups) AND
#take into account mean of wellbeing of EACH GROUP when predicting everyone's wellbeing scores AND
Intercept.Model.Ignoring.Groups <- gls(WBEING ~ 1, data = multilevel.data)
Intercept.Model.With.Groups <- lme(WBEING ~ 1, random = ~ 1|GRP, data = multilevel.data)

#are those different? (ie. anova) - do they predict wellbeing scores better
anova(Intercept.Model.Ignoring.Groups, Intercept.Model.With.Groups)
#better fit when take group into account - logLike decreasing, chi squared value of 188, significant

#how much do they matter?
VarCorr(Intercept.Model.With.Groups)
#4 percent of variability in individual wellbeing scores is due to workgroup that you're in
#calculated by effect/(effect + error) = 0.03580077/(0.03580077+0.78949727) = 0.0433792
#most variation at individual level, but group does also tend to matter a bit


#how can we explain this variability?
#fixed effects
Model.1 <- lme(WBEING ~ HRS + HRS.GRP, random=~1|GRP, data=multilevel.data)
summary(Model.1) #gives fixed effect
#every hour you work as individual, wellbeing goes down by 0.04
#every hour your work group works, wellbeing goes down by 0.17 (HRS + HRS.GRP)

#random effects
VarCorr(Model.1)
#variance in group mean is .014 (with overall group mean of 4.74)
#average within group variability is .78
