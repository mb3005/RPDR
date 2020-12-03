library(tidyverse)

#to correct for parsing failures; R was reading as "logical" class for columns: bottom3, bottom4, bottom5, bottom 6

Bottom_Two <- read_csv(file.path("Data Files", "Bottom_Two.csv"))

Bottom_Two <- Bottom_Two %>%
  mutate_all(as.factor)

Bottom_Two <- Bottom_Two %>%
  mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.character)


Bottom_Two <- Bottom_Two %>%
  mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.numeric)

str(Bottom_Two)

Bottom_Two$DOB <- as.Date(as.numeric(as.character(Bottom_Two$DOB)),origin="1960-01-01")
Bottom_Two$Airdate <- as.Date(Bottom_Two$Airdate,format="%Y-%m-%d")


#recoding factor levels

Bottom_Two$Gender <- Bottom_Two$Gender %>%
  fct_recode("Cis" = "0", "Trans" = "1")
levels(Bottom_Two$Gender)
Bottom_Two$Gender

Bottom_Two$Race <- Bottom_Two$Race %>%
  fct_recode('White'='0', 'Latin'='2', 'Asian'='3', 'Black'='1', 'Other'='4')
levels(Bottom_Two$Race)
Bottom_Two$Race

Bottom_Two$Quality_Of_Outfit <- Bottom_Two$Quality_Of_Outfit %>%
  fct_recode('Boot'='0', 'Toot'='1', 'Supertoot'='2')
levels(Bottom_Two$Quality_Of_Outfit)
Bottom_Two$Quality_Of_Outfit

Bottom_Two$Body_Type <- Bottom_Two$Body_Type %>%
  fct_recode('Not Big'='0', 'Thick n Juicy'='1', 'Chunky Yet Funky'='2')
levels(Bottom_Two$Body_Type)
Bottom_Two$Body_Type

Bottom_Two$Type_Queen <- Bottom_Two$Type_Queen %>%
  fct_recode('Comedy'='0', 'Pageant'='1', 'Look'='2', 'Other'='3')
levels(Bottom_Two$Type_Queen)
Bottom_Two$Type_Queen

#setting consistent reference levels

Bottom_Two$OUTCOME_LOSS <- relevel(Bottom_Two$OUTCOME_LOSS, ref = "0")

Bottom_Two$Hometown_State <- relevel(Bottom_Two$Hometown_State, ref = "New York")

Bottom_Two$Hometown_City <- relevel(Bottom_Two$Hometown_City, ref = "New York")

Bottom_Two$Quality_of_outfit <- relevel(Bottom_Two$Quality_Of_Outfit, ref = "Boot")

Bottom_Two$Wig_Removed <- relevel(Bottom_Two$Wig_Removed, ref = "0")

Bottom_Two$Death_Drop <- relevel(Bottom_Two$Death_Drop, ref = "0")

Bottom_Two$Outfit_Reveal <- relevel(Bottom_Two$Outfit_Reveal, ref = "0")

Bottom_Two$Merle <- relevel(Bottom_Two$Merle, ref = "0")

Bottom_Two$Michelle <- relevel(Bottom_Two$Michelle, ref = "0")

Bottom_Two$Ross <- relevel(Bottom_Two$Ross, ref = "0")

Bottom_Two$Carson <- relevel(Bottom_Two$Carson, ref = "0")

Bottom_Two$Do_they_know_words <- relevel(Bottom_Two$Do_They_Know_Words, ref = "0")

Bottom_Two$Outfit_Reveal_1 <- relevel(Bottom_Two$Outfit_Reveal_1, ref = "0")

Bottom_Two$Sewing <- relevel(Bottom_Two$Sewing, ref = "0")

Bottom_Two$Dancing <- relevel(Bottom_Two$Dancing, ref = "0")

Bottom_Two$Singing <- relevel(Bottom_Two$Singing, ref = "0")

Bottom_Two$Lip_Sync_Ass <- relevel(Bottom_Two$Lip_Sync_Ass, ref = "0")

Bottom_Two$Expressed_Hardship <- relevel(Bottom_Two$Expressed_Hardship, ref = "0")

########################## UNIT BIVARIATE MODELS ###########################

summary(Age <- glm(OUTCOME_LOSS ~ Age, family=binomial(link='logit'), data=Bottom_Two))

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City, family=binomial(link='logit'), data=Bottom_Two))

summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State, family=binomial(link='logit'), data=Bottom_Two))

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed, family=binomial(link='logit'), data=Bottom_Two))

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop, family=binomial(link='logit'), data=Bottom_Two))

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal, family=binomial(link='logit'), data=Bottom_Two))

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_Of_Outfit, family=binomial(link='logit'), data=Bottom_Two))

summary(Merle <- glm(OUTCOME_LOSS ~ Merle, family=binomial(link='logit'), data=Bottom_Two))

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle, family=binomial(link='logit'), data=Bottom_Two))

summary(Santino <- glm(OUTCOME_LOSS ~ Santino, family=binomial(link='logit'), data=Bottom_Two))

summary(Ross <- glm(OUTCOME_LOSS ~ Ross, family=binomial(link='logit'), data=Bottom_Two))

summary(Carson <- glm(OUTCOME_LOSS ~ Carson, family=binomial(link='logit'), data=Bottom_Two))

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words, family=binomial(link='logit'), data=Bottom_Two))

summary(Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1, family=binomial(link='logit'), data=Bottom_Two))

summary(Gender <- glm(OUTCOME_LOSS ~ Gender, family=binomial(link='logit'), data=Bottom_Two))

summary(Race <- glm(OUTCOME_LOSS ~ Race, family=binomial(link='logit'), data=Bottom_Two))

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type, family=binomial(link='logit'), data=Bottom_Two))

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen, family=binomial(link='logit'), data=Bottom_Two))

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing, family=binomial(link='logit'), data=Bottom_Two))

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing, family=binomial(link='logit'), data=Bottom_Two))

summary(Singing <- glm(OUTCOME_LOSS ~ Singing, family=binomial(link='logit'), data=Bottom_Two))

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass, family=binomial(link='logit'), data=Bottom_Two))

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship, family=binomial(link='logit'), data=Bottom_Two))

