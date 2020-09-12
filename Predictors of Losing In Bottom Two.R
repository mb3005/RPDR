library(tidyverse)

#to correct for parsing failures; R was reading as "logical" class for columns: bottom3, bottom4, bottom5, bottom 6

Bottom_Two <- read_csv(file.path("Data", "Bottom_Two_Data.csv"),
                 col_names = TRUE,
                 col_types = cols(
                   bottom3 = col_character(),
                   bottom4 = col_character(),
                   bottom5 = col_character(),
                   bottom6 = col_character(),
                   minicw3 = col_character()
                 ))
class(Bottom_Two$bottom3)
class(Bottom_Two$bottom4)
class(Bottom_Two$bottom5)
class(Bottom_Two$bottom6)

Age <- glm(OUTCOME_LOSS ~ age, family=binomial(link='logit'), data=Bottom_Two)
summary(Age)

Hometown_City <- glm(OUTCOME_LOSS ~ hometown_city, family=binomial(link='logit'), data=Bottom_Two)
summary(Hometown_City)

Hometown_State <- glm(OUTCOME_LOSS ~ hometown_state, family=binomial(link='logit'), data=Bottom_Two)
summary(Hometown_State)

Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed, family=binomial(link='logit'), data=Bottom_Two)
summary(Wig_Removed)

Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop, family=binomial(link='logit'), data=Bottom_Two)
summary(Death_Drop)

Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal, family=binomial(link='logit'), data=Bottom_Two)
summary(Outfit_Reveal)

Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_of_outfit, family=binomial(link='logit'), data=Bottom_Two)
summary(Quality_Of_Outfit)

Merle <- glm(OUTCOME_LOSS ~ Merle, family=binomial(link='logit'), data=Bottom_Two)
summary(Merle)

Michelle <- glm(OUTCOME_LOSS ~ Michelle, family=binomial(link='logit'), data=Bottom_Two)
summary(Michelle)

Santino <- glm(OUTCOME_LOSS ~ Santino, family=binomial(link='logit'), data=Bottom_Two)
summary(Santino)

Ross <- glm(OUTCOME_LOSS ~ Ross, family=binomial(link='logit'), data=Bottom_Two)
summary(Ross)

Carson <- glm(OUTCOME_LOSS ~ Carson, family=binomial(link='logit'), data=Bottom_Two)
summary(Carson)

Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_they_know_words, family=binomial(link='logit'), data=Bottom_Two)
summary(Do_They_Know_Words)

Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1, family=binomial(link='logit'), data=Bottom_Two)
summary(Outfit_Reveal_1)

Gender <- glm(OUTCOME_LOSS ~ Gender, family=binomial(link='logit'), data=Bottom_Two)
summary(Gender)

Race <- glm(OUTCOME_LOSS ~ Race, family=binomial(link='logit'), data=Bottom_Two)
summary(Race)

Body_Type <- glm(OUTCOME_LOSS ~ Body_Type, family=binomial(link='logit'), data=Bottom_Two)
summary(Body_Type)

Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen, family=binomial(link='logit'), data=Bottom_Two)
summary(Type_Queen)

Sewing <- glm(OUTCOME_LOSS ~ Sewing, family=binomial(link='logit'), data=Bottom_Two)
summary(Sewing)

Dancing <- glm(OUTCOME_LOSS ~ Dancing, family=binomial(link='logit'), data=Bottom_Two)
summary(Dancing)

Singing <- glm(OUTCOME_LOSS ~ Singing, family=binomial(link='logit'), data=Bottom_Two)
summary(Singing)

Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass, family=binomial(link='logit'), data=Bottom_Two)
summary(Lip_Sync_Ass)

Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship, family=binomial(link='logit'), data=Bottom_Two)
summary(Expressed_Hardship)

