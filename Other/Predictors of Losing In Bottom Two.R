library(tidyverse)

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

#collapsing factor levels
Bottom_Two$Hometown_City <- Bottom_Two$Hometown_City %>%
  fct_collapse('New York' = c('New York', 'Brooklyn', 'The Bronx'),
               'Other' = c('Albuquerque', 'Atlanta', 'Austin', 'Azusa',
                           'Back Swamp', 'Bayamon', 'Bedford', 'Boston', 'Carolina',
                           'Cayey', 'Cherry Hill', 'Chicago', 'Cincinnati',
                           'Cleveland', 'Columbus', 'Dallas', 'Dayton', 'Denver',
                           'Dorado', 'Echo Park', 'Elmwood Park', 'Falls Church',
                           'Fort Lauderdale', 'Gainesville', 'Gloucester', 'Greenville',
                           'Guaynabo', 'Harlem', 'Hudson', 'Indianapolis', 'Iowa City',
                           'Johnson City', 'Kansas City', 'Las Vegas', 'Long Beach',
                           'Los Angeles', 'Manati', 'Mesquite', 'Milwaukee', 'Minneapolis',
                           'Mira Loma', 'Nashville', 'New Orleans', 'Norwalk', 'Orlando',
                           'Owensboro', 'Pittsburgh', 'Queens', 'Raleigh', 'Redlands', 'Riverdale',
                           'Riverside', 'Rochester', 'San Diego', 'San Francisco', 'San Juan',
                           'Savannah', 'Seattle', 'Shreveport', 'South Beach', 'Southlake', 'St. Petersburg',
                           'Tallahassee', 'Tampa', 'Tucson', 'Van Nuys', 'West Hollywood',
                           'Worcester'))

levels(Bottom_Two$Hometown_City)

#setting consistent reference levels

Bottom_Two$OUTCOME_LOSS <- relevel(Bottom_Two$OUTCOME_LOSS, ref = "0")

#Bottom_Two$Hometown_State <- relevel(Bottom_Two$Hometown_State, ref = "New York")

Bottom_Two$Hometown_City <- relevel(Bottom_Two$Hometown_City, ref = "New York")

Bottom_Two$Gender <- relevel(Bottom_Two$Gender, ref = "Cis")

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

summary(Null <- glm(OUTCOME_LOSS ~ 1, family=binomial(link='logit'), data=Bottom_Two))

summary(Age <- glm(OUTCOME_LOSS ~ Age, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Age, test="LRT")

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Hometown_City, test="LRT")

#Hometown_State causes colinearity - will no longer be including in the model
#summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State, family=binomial(link='logit'), data=Bottom_Two))
#anova(Null, Hometown_State, test="LRT")

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Wig_Removed, test="LRT")

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Death_Drop, test="LRT")

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Outfit_Reveal, test="LRT")

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_Of_Outfit, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Quality_Of_Outfit, test="LRT")

summary(Merle <- glm(OUTCOME_LOSS ~ Merle, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Merle, test="LRT")

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Michelle, test="LRT")

summary(Santino <- glm(OUTCOME_LOSS ~ Santino, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Santino, test="LRT")

summary(Ross <- glm(OUTCOME_LOSS ~ Ross, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Ross, test="LRT")

summary(Carson <- glm(OUTCOME_LOSS ~ Carson, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Carson, test="LRT")

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Do_They_Know_Words, test="LRT")

summary(Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Outfit_Reveal_1, test="LRT")

summary(Gender <- glm(OUTCOME_LOSS ~ Gender, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Gender, test="LRT")

summary(Race <- glm(OUTCOME_LOSS ~ Race, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Race, test="LRT")

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Body_Type, test="LRT")

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Type_Queen, test="LRT")

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Sewing, test="LRT")

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Dancing, test="LRT")

summary(Singing <- glm(OUTCOME_LOSS ~ Singing, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Singing, test="LRT")

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Lip_Sync_Ass, test="LRT")

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship, family=binomial(link='logit'), data=Bottom_Two))
anova(Null, Expressed_Hardship, test="LRT")

################         Multivariate Model Comparisons        #################

summary(Full_Model <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Ross + Carson + Do_They_Know_Words +
    Gender + Race + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

summary(Model_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Ross + Carson + Do_They_Know_Words +
    Gender + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_1, Full_Model, test="LRT")   #Race excluded from Model 1

summary(Model_2 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Ross + Carson + Do_They_Know_Words +
    Gender + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_2, Model_1, test="LRT")   #Body_Type excluded from Model 2

summary(Model_3 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Carson + Do_They_Know_Words +
    Gender + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_3, Model_2, test="LRT")   #Ross excluded from Model 3

summary(Model_4 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Do_They_Know_Words +
    Gender + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_4, Model_3, test="LRT")   #Carson excluded from Model 4

summary(Model_5 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Do_They_Know_Words +
    Gender + Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_5, Model_4, test="LRT")   #Dancing excluded from Model 5

summary(Model_6 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Do_They_Know_Words +
    Gender + Sewing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

anova(Model_6, Model_5, test="LRT")   #Singing excluded from Model 6

Delta_Coef_1 <- abs((coef(Model_6)-coef(Full_Model))/coef(Full_Model))
round(Delta_Coef_1,3)

############       Re-evaluation Of Initial Predictors    #############

# Variables excluded in second evaluation in order: Age, Hometown_City, Wig_Removed, Death_Drop, Merle, Michelle,
#                                                   Santino, Type_Queen, Race, Body_Type, Ross, Carson, Dancing, Singing

# preliminary main effects model
summary(Model_6 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Do_They_Know_Words +
    Gender + Sewing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))

############     Examining Interactions Among Covariates     ############

# no significant interactions

summary(final_model_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Quality_Of_Outfit + Do_They_Know_Words +
    Gender + Sewing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two))


