library(tidyverse)

Bottom_Two_2 <- read_csv(file.path("Data Files", "Bottom_Two.csv"))

Bottom_Two_2 <- Bottom_Two_2 %>%
  mutate_all(as.factor)

Bottom_Two_2 <- Bottom_Two_2 %>%
  mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.character)

Bottom_Two_2 <- Bottom_Two_2 %>%
  mutate_at(vars('Age','Numqueens', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.numeric)

str(Bottom_Two_2)

Bottom_Two_2$DOB <- as.Date(as.numeric(as.character(Bottom_Two_2$DOB)),origin="1960-01-01")
Bottom_Two_2$Airdate <- as.Date(Bottom_Two_2$Airdate,format="%Y-%m-%d")


#recoding factor levels

Bottom_Two_2$Gender <- Bottom_Two_2$Gender %>%
  fct_recode("Cis" = "0", "Trans" = "1")
levels(Bottom_Two_2$Gender)
Bottom_Two_2$Gender

Bottom_Two_2$Race <- Bottom_Two_2$Race %>%
  fct_recode('White'='0', 'Latin'='2', 'Asian'='3', 'Black'='1', 'Other'='4')
levels(Bottom_Two_2$Race)
Bottom_Two_2$Race

Bottom_Two_2$Quality_Of_Outfit <- Bottom_Two_2$Quality_Of_Outfit %>%
  fct_recode('Boot'='0', 'Toot'='1', 'Supertoot'='2')
levels(Bottom_Two_2$Quality_Of_Outfit)
Bottom_Two_2$Quality_Of_Outfit

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_recode('Not Big'='0', 'Thick n Juicy'='1', 'Chunky Yet Funky'='2')
levels(Bottom_Two_2$Body_Type)
Bottom_Two_2$Body_Type

Bottom_Two_2$Type_Queen <- Bottom_Two_2$Type_Queen %>%
  fct_recode('Comedy'='0', 'Pageant'='1', 'Look'='2', 'Other'='3')
levels(Bottom_Two_2$Type_Queen)
Bottom_Two_2$Type_Queen

#collapsing factor levels
Bottom_Two_2$Hometown_City <- Bottom_Two_2$Hometown_City %>%
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

levels(Bottom_Two_2$Hometown_City)

#setting consistent reference levels

Bottom_Two_2$OUTCOME_LOSS <- relevel(Bottom_Two_2$OUTCOME_LOSS, ref = "0")

#Bottom_Two_2$Hometown_State <- relevel(Bottom_Two_2$Hometown_State, ref = "New York")

Bottom_Two_2$Hometown_City <- relevel(Bottom_Two_2$Hometown_City, ref = "New York")

Bottom_Two_2$Gender <- relevel(Bottom_Two_2$Gender, ref = "Cis")

Bottom_Two_2$Body_Type <- relevel(Bottom_Two_2$Body_Type, ref = "Not Big")

Bottom_Two_2$Quality_of_outfit <- relevel(Bottom_Two_2$Quality_Of_Outfit, ref = "Boot")

Bottom_Two_2$Wig_Removed <- relevel(Bottom_Two_2$Wig_Removed, ref = "0")

Bottom_Two_2$Death_Drop <- relevel(Bottom_Two_2$Death_Drop, ref = "0")

Bottom_Two_2$Outfit_Reveal <- relevel(Bottom_Two_2$Outfit_Reveal, ref = "0")

Bottom_Two_2$Merle <- relevel(Bottom_Two_2$Merle, ref = "0")

Bottom_Two_2$Michelle <- relevel(Bottom_Two_2$Michelle, ref = "0")

Bottom_Two_2$Ross <- relevel(Bottom_Two_2$Ross, ref = "0")

Bottom_Two_2$Carson <- relevel(Bottom_Two_2$Carson, ref = "0")

Bottom_Two_2$Do_they_know_words <- relevel(Bottom_Two_2$Do_They_Know_Words, ref = "0")

Bottom_Two_2$Outfit_Reveal_1 <- relevel(Bottom_Two_2$Outfit_Reveal_1, ref = "0")

Bottom_Two_2$Sewing <- relevel(Bottom_Two_2$Sewing, ref = "0")

Bottom_Two_2$Dancing <- relevel(Bottom_Two_2$Dancing, ref = "0")

Bottom_Two_2$Singing <- relevel(Bottom_Two_2$Singing, ref = "0")

Bottom_Two_2$Lip_Sync_Ass <- relevel(Bottom_Two_2$Lip_Sync_Ass, ref = "0")

Bottom_Two_2$Expressed_Hardship <- relevel(Bottom_Two_2$Expressed_Hardship, ref = "0")


########################## UNIT BIVARIATE MODELS ###########################

summary(Null <- glm(OUTCOME_LOSS ~ 1, family=binomial(link='logit'), data=Bottom_Two_2))

summary(Age <- glm(OUTCOME_LOSS ~ Age, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Age, test="LRT")

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Hometown_City, test="LRT")

#Hometown_State causes colinearity - will no longer be including in the model
#summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State, family=binomial(link='logit'), data=Bottom_Two_2))
#anova(Null, Hometown_State, test="LRT")

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Wig_Removed, test="LRT")

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Death_Drop, test="LRT")

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Outfit_Reveal, test="LRT")

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_Of_Outfit, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Quality_Of_Outfit, test="LRT")

summary(Merle <- glm(OUTCOME_LOSS ~ Merle, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Merle, test="LRT")

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Michelle, test="LRT")

summary(Santino <- glm(OUTCOME_LOSS ~ Santino, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Santino, test="LRT")

summary(Ross <- glm(OUTCOME_LOSS ~ Ross, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Ross, test="LRT")

summary(Carson <- glm(OUTCOME_LOSS ~ Carson, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Carson, test="LRT")

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Do_They_Know_Words, test="LRT")

summary(Outfit_Reveal_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal_1, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Outfit_Reveal_1, test="LRT")

summary(Gender <- glm(OUTCOME_LOSS ~ Gender, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Gender, test="LRT")

summary(Race <- glm(OUTCOME_LOSS ~ Race, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Race, test="LRT")

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Body_Type, test="LRT")

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Type_Queen, test="LRT")

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Sewing, test="LRT")

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Dancing, test="LRT")

summary(Singing <- glm(OUTCOME_LOSS ~ Singing, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Singing, test="LRT")

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Lip_Sync_Ass, test="LRT")

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship, family=binomial(link='logit'), data=Bottom_Two_2))
anova(Null, Expressed_Hardship, test="LRT")

################         Multivariate Model Comparisons        #################

summary(Full_Model <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
    Gender + Race + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_1 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
    Gender + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_1, Full_Model, test="LRT")   #Race excluded from Model 1

summary(Model_2 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
    Gender + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_2, Model_1, test="LRT")   #Body_Type excluded from Model 2

summary(Model_3 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
    Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_3, Model_2, test="LRT")   #Gender excluded from Model 3

summary(Model_4 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
    Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_4, Model_3, test="LRT")   #Ross excluded from Model 4

summary(Model_5 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_5, Model_4, test="LRT")   #Dancing excluded from Model 5

summary(Model_6 <- glm(OUTCOME_LOSS ~ Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,
    family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_6, Model_5, test="LRT")   #Outfit_Reveal excluded from Model 6

#  Beta Comparisons differ > 20% for Singing

summary(Model_7 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_8 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Dancing + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_9 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Dancing + Ross + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_10 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Dancing + Ross + Gender + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_11 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Dancing + Ross + Gender + Body_Type + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_12 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Dancing + Ross + Gender + Body_Type + Race + Carson + Do_They_Know_Words +
    Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,family = binomial(link='logit'), data = Bottom_Two_2))

Delta_Coef_1 <- abs((coef(Model_6)-coef(Full_Model))/coef(Full_Model))
round(Delta_Coef_1,3)

############       Re-evaluation Of Initial Predictors        #############

# Variables excluded in second evaluation in order: Age, Hometown_City, Wig_Removed, Death_Drop, Merle, Michelle,
#                                                   Santino, Type_Queen, Race, Body_Type, Ross, Carson, Dancing, Singing

# Beta comparisons showed there was no point in which coefficients did not change by  more than 20%.
# As a result, we will be collapsing the multilevel variable Body Type as there was a zero cell

table(Bottom_Two_2$Body_Type, Bottom_Two_2$Gender)
table(Bottom_Two_2$Race, Bottom_Two_2$Gender)

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_collapse('Not Big' = c('Not Big'),
               'Bodacious' = c('Thick n Juicy', 'Chunky Yet Funky'))

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_recode('Not Big'='0', 'Bodacious'='1')
levels(Bottom_Two_2$Body_Type)

Bottom_Two_2$Body_Type <- relevel(Bottom_Two_2$Body_Type, ref = "0")

Bottom_Two_2$Body_Type

###########      Multivariate Model Comparisons with Collapsed Body Type       #############

summary(Full_Model_2 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
          Gender + Race + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_13 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
          Gender + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_13, Full_Model_2, test="LRT")   # Race excluded from Model 13

summary(Model_14 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
          Gender + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_14, Model_13, test="LRT")      # Body Type excluded from Model 14

summary(Model_15 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
          Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_15, Model_14, test="LRT")      # Gender excluded from Model 15

summary(Model_16 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
          Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_16, Model_15, test="LRT")     # Ross excluded from Model 16

summary(Model_17 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
          Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_17, Model_16, test="LRT")    # Dancing excluded from Model 17

summary(Model_18 <- glm(OUTCOME_LOSS ~ Carson + Do_They_Know_Words +
          Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship,
          family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_18, Model_17, test="LRT")    # Outfit Reveal excluded from Model 18

# all p values equal or < 0.25 in Model_18

#  Beta Comparisons differ > 20% for Singing

