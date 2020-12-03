
library(tidyverse)

#to correct for parsing failures; R was reading as "logical" class many different
# columns

RPDR <- read_csv(file.path("Data Files", "combined - 04112020.csv"),
                          col_names = TRUE,
                          col_types = cols(
                             Lip_Sync_Number = col_factor(),
                             Bottom3 = col_factor(),
                             Bottom4 = col_factor(),
                             Bottom5 = col_factor(),
                             Bottom6 = col_factor()
                          ))

RPDR <- RPDR %>%
    mutate_all(as.factor)

RPDR <- RPDR %>%
        mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                       'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                       'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
                        as.character)

RPDR <- RPDR %>%
  mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.numeric)
str(RPDR)

RPDR$DOB <- as.Date(as.numeric(as.character(RPDR$DOB)),origin="1960-01-01")
RPDR$Airdate <- as.Date(RPDR$Airdate,format="%Y-%m-%d")

#recoding factor levels
RPDR$Gender <- RPDR$Gender %>%
    fct_recode("Cis" = "0", "Trans" = "1")
levels(RPDR$Gender)
RPDR$Gender

RPDR$Race <- RPDR$Race %>%
    fct_recode('White'='0', 'Latin'='2', 'Asian'='3', 'Black'='1', 'Other'='4')
levels(RPDR$Race)
RPDR$Race

RPDR$Quality_Of_Outfit <- RPDR$Quality_Of_Outfit %>%
    fct_recode('Boot'='0', 'Toot'='1', 'Supertoot'='2')
levels(RPDR$Quality_Of_Outfit)
RPDR$Quality_Of_Outfit

RPDR$Body_Type <- RPDR$Body_Type %>%
    fct_recode('Not Big'='0', 'Thick n Juicy'='1', 'Chunky Yet Funky'='2')
levels(RPDR$Body_Type)
RPDR$Body_Type

RPDR$Type_Queen <- RPDR$Type_Queen %>%
    fct_recode('Comedy'='0', 'Pageant'='1', 'Look'='2', 'Other'='3')
levels(RPDR$Type_Queen)
RPDR$Type_Queen

#setting consistent reference levels
RPDR$OUTCOME_LOSS <- relevel(RPDR$OUTCOME_LOSS, ref = "0")

RPDR$Hometown_State <- relevel(RPDR$Hometown_State, ref = "New York")

RPDR$Hometown_City <- relevel(RPDR$Hometown_City, ref = "New York")

RPDR$Quality_of_outfit <- relevel(RPDR$Quality_Of_Outfit, ref = "Boot")

RPDR$Wig_Removed <- relevel(RPDR$Wig_Removed, ref = "0")

RPDR$Death_Drop <- relevel(RPDR$Death_Drop, ref = "0")

RPDR$Outfit_Reveal <- relevel(RPDR$Outfit_Reveal, ref = "0")

RPDR$Merle <- relevel(RPDR$Merle, ref = "0")

RPDR$Michelle <- relevel(RPDR$Michelle, ref = "0")

RPDR$Ross <- relevel(RPDR$Ross, ref = "0")

RPDR$Carson <- relevel(RPDR$Carson, ref = "0")

RPDR$Do_they_know_words <- relevel(RPDR$Do_They_Know_Words, ref = "0")

RPDR$Outfit_Reveal_1 <- relevel(RPDR$Outfit_Reveal_1, ref = "0")

RPDR$Sewing <- relevel(RPDR$Sewing, ref = "0")

RPDR$Dancing <- relevel(RPDR$Dancing, ref = "0")

RPDR$Singing <- relevel(RPDR$Singing, ref = "0")

RPDR$Lip_Sync_Ass <- relevel(RPDR$Lip_Sync_Ass, ref = "0")

RPDR$Expressed_Hardship <- relevel(RPDR$Expressed_Hardship, ref = "0")


########################### UNIT BIVARIATE MODELS ###########################


summary(Age <- glm(OUTCOME_LOSS ~ Age + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_of_outfit + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Merle <- glm(OUTCOME_LOSS ~ Merle + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Santino <- glm(OUTCOME_LOSS ~ Santino + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Ross <- glm(OUTCOME_LOSS ~ Ross + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Carson <- glm(OUTCOME_LOSS ~ Carson + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1 + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Gender <- glm(OUTCOME_LOSS ~ Gender + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Race <- glm(OUTCOME_LOSS ~ Race + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Singing <- glm(OUTCOME_LOSS ~ Singing + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass + as.factor(Episode), family=binomial(link='logit'), data=RPDR))

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship + as.factor(Episode), family=binomial(link='logit'), data=RPDR))


