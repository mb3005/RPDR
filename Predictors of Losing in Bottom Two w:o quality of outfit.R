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

############# Collapsing Body Type  ###############

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_collapse('Not Big' = c('Not Big'),
               'Bodacious' = c('Thick n Juicy', 'Chunky Yet Funky'))

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_recode('Not Big'='0', 'Bodacious'='1')
levels(Bottom_Two_2$Body_Type)

Bottom_Two_2$Body_Type <- relevel(Bottom_Two_2$Body_Type, ref = "0")

Bottom_Two_2$Body_Type

##############    Collapsing Race    #################

Bottom_Two_2$Race <- Bottom_Two_2$Race %>%
  fct_collapse('White' = c('White'),
               'Non White' = c('Latin','Asian','Black','Other'))

Bottom_Two_2$Body_Type <- Bottom_Two_2$Body_Type %>%
  fct_recode('Non White'='0', 'White'='1')
levels(Bottom_Two_2$Race)

Bottom_Two_2$Race <- relevel(Bottom_Two_2$Race, ref = "0")

Bottom_Two_2$Race

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
                       Race + Body_Type + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
                       family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_1, Full_Model, test="LRT")   #Gender excluded from Model 1

summary(Model_2 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
                       Race + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
                       family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_2, Model_1, test="LRT")   #Body_Type excluded from Model 2

summary(Model_3 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words
                       + Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship,
                       family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_3, Model_2, test="LRT")   #Race excluded from Model 3

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

# Beta Comparisons showed Beta estimate for singing changed >20%

summary(Model_7 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Carson + Do_They_Know_Words +
                         Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_8 <- glm(OUTCOME_LOSS ~ Dancing + Outfit_Reveal + Carson + Do_They_Know_Words +
                         Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_9 <- glm(OUTCOME_LOSS ~ Ross + Dancing + Outfit_Reveal + Carson + Do_They_Know_Words +
                         Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

# Adding back in Outfit Reveal, Dancing and then Ross yielded beta comparisons changing <20%
# Ross retained in the model


###########      Backward Elimination Round 2     ###########

summary(Model_10 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
                         Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_10, Model_9, test="LRT")      # Dancing excluded from Model 10

summary(Model_11 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Carson + Do_They_Know_Words +
                          Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_11, Model_10, test="LRT")      # Outfit Reveal met significance criteria; retained in the model

summary(Model_12 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Singing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_12, Model_11, test="LRT")      # Carson excluded from Model 12

summary(Model_13 <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_13, Model_12, test="LRT")     # Singing excluded from Model 13

# All variables meet pr(>|z|) < 0.25 threshold
# Beta comparisons showed the beta estimate for Ross changing > 20%; confounding variable

summary(Model_14 <- glm(OUTCOME_LOSS ~ Singing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_15 <- glm(OUTCOME_LOSS ~ Carson + Singing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

summary(Model_16 <- glm(OUTCOME_LOSS ~ Dancing + Carson + Singing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

# Adding in Singing,Carson and then Dancing yielded beta estimate comparisons changing < 20%
# Dancing retained in the model

summary(Model_16 <- glm(OUTCOME_LOSS ~ Dancing + Carson + Singing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

###########      Backward Elimination Round 3     ###########


summary(Model_17 <- glm(OUTCOME_LOSS ~ Dancing + Carson + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_17, Model_16, test="LRT")    # Singing excluded from Model 17


summary(Model_18 <- glm(OUTCOME_LOSS ~ Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_18, Model_17, test="LRT")     # Carson excluded from Model 18

# all variables meet pr(>|z|) < 0.25 threshold or were retained
# Beta comparisons showed the beta estimate for Ross changing > 20%; confounding variable

summary(Model_19 <- glm(OUTCOME_LOSS ~ Carson + Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

# Adding Carson back in the model yielded beta estimate comparisons changing < 20%
# Carson retained in Model 19

############       Re-evaluation Of Initial Predictors       #############

# Variables excluded in second evaluation in order: Age, Hometown_City, Wig_Removed, Death_Drop, Merle, Michelle,
#                                                   Santino, Gender, Race, Body_Type, Type_Queen, Singing,
#


# preliminary main effects model
summary(Model_20 <- glm(OUTCOME_LOSS ~ Age + Carson + Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))

anova(Model_20, Model_19, test="LRT")     # Age retained

############     Testing Linearity of Continuous Variables     ############


summary(Model_20 <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass:Expressed_Hardship + Age + Carson + Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))


Age_Ordered <- Bottom_Two_2 %>%      # age arranged from smallest to largest
    arrange(Age)

Age_Range <- as.factor(cut(Age_Ordered$Age, breaks=c(20,25,30,35,40,41,45,Inf)))

table(Age_Range)

Age_Range <- relevel(Age_Range, ref = "(45,Inf]")
levels(Age_Range)

summary(Model_20 <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass:Expressed_Hardship + Age_Range + Carson + Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                          Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2))


##########      collapsing 41+ age groups      ###########

Age_Ordered <- Bottom_Two_2 %>%      # age arranged from smallest to largest
  arrange(Age)

Age_Range <- as.factor(cut(Age_Ordered$Age, breaks=c(20,25,30,35,40,Inf)))

table(Age_Range)

Age_Range <- relevel(Age_Range, ref = "(40,Inf]")
levels(Age_Range)

summary(final_model) <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass:Expressed_Hardship + Age_Range + Carson + Dancing + Outfit_Reveal + Ross + Do_They_Know_Words +
                       Sewing + Lip_Sync_Ass + Expressed_Hardship, family = binomial(link='logit'), data = Bottom_Two_2)

