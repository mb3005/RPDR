
library(tidyverse)

#to correct for parsing failures; R was reading as "logical" class for columns: bottom3, bottom4, bottom5, bottom 6

RPDR <- read_csv(file.path("Data", "combine.csv"),
                        col_names = TRUE,
                        col_types = cols(
                              bottom3 = col_character(),
                              bottom4 = col_character(),
                              bottom5 = col_character(),
                              bottom6 = col_character()
                          ))
class(RPDR$bottom3)
class(RPDR$bottom4)
class(RPDR$bottom5)
class(RPDR$bottom6)

age <- glm(OUTCOME_LOSS ~ age + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(age)

hometown_city <- glm(OUTCOME_LOSS ~ hometown_city + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(hometown_city)

hometown_state <- glm(OUTCOME_LOSS ~ hometown_state + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(hometown_state)

Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Wig_Removed)

#getting a warning message after running glm: glm.fit: fitted probabilities numerically 0 or 1 occurred
Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Death_Drop)

Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Outfit_Reveal)

Quality_of_outfit <- glm(OUTCOME_LOSS ~ Quality_of_outfit + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Quality_of_outfit)

Merle <- glm(OUTCOME_LOSS ~ Merle + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Merle)

Michelle <- glm(OUTCOME_LOSS ~ Michelle + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Michelle)

Santino <- glm(OUTCOME_LOSS ~ Santino + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Santino)

Ross <- glm(OUTCOME_LOSS ~ Ross + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Ross)

Carson <- glm(OUTCOME_LOSS ~ Carson + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Carson)

Do_they_know_words <- glm(OUTCOME_LOSS ~ Do_they_know_words + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Do_they_know_words)

Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1 + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Outfit_Reveal_1)

Gender <- glm(OUTCOME_LOSS ~ Gender + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Gender)

Race <- glm(OUTCOME_LOSS ~ Race + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Race)

Body_Type <- glm(OUTCOME_LOSS ~ Body_Type + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Body_Type)

Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Type_Queen)

Sewing <- glm(OUTCOME_LOSS ~ Sewing + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Sewing)

Dancing <- glm(OUTCOME_LOSS ~ Dancing + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Dancing)

Singing <- glm(OUTCOME_LOSS ~ Singing + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Singing)

Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Lip_Sync_Ass)

Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship + as.factor(Episode), family=binomial(link='logit'), data=RPDR)
summary(Expressed_Hardship)



