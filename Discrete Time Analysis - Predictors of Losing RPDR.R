library(tidyverse)

Bottom_2_Interval <- read_csv(file.path("Data Files", "Bottom_Two_Interval.csv"))

Bottom_2_Interval <- Bottom_2_Interval %>%
  mutate_all(as.factor)

view(Bottom_2_Interval)

Bottom_2_Interval <- Bottom_2_Interval %>%
  mutate_at(vars('Age','Episode', 'Bottom_Two_Interval', 'Numqueens', 'Sumqueen', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.character)

Bottom_2_Interval <- Bottom_2_Interval %>%
  mutate_at(vars('Age','Episode', 'Bottom_Two_Interval', 'Numqueens', 'Sumqueen', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.numeric)

str(Bottom_2_Interval)

Bottom_2_Interval$DOB <- as.Date(as.numeric(as.character(Bottom_2_Interval$DOB)),origin="1960-01-01")
Bottom_2_Interval$Airdate <- as.Date(Bottom_2_Interval$Airdate,format="%Y-%m-%d")


# recoding factor levels

Bottom_2_Interval$Gender <- Bottom_2_Interval$Gender %>%
  fct_recode("Cis" = "0", "Trans" = "1")
levels(Bottom_2_Interval$Gender)
Bottom_2_Interval$Gender

Bottom_2_Interval$Race <- Bottom_2_Interval$Race %>%
  fct_recode('White'='0', 'Latin'='2', 'Asian'='3', 'Black'='1', 'Other'='4')
levels(Bottom_2_Interval$Race)
Bottom_2_Interval$Race

Bottom_2_Interval$Quality_Of_Outfit <- Bottom_2_Interval$Quality_Of_Outfit %>%
  fct_recode('Boot'='0', 'Toot'='1', 'Supertoot'='2')
levels(Bottom_2_Interval$Quality_Of_Outfit)
Bottom_2_Interval$Quality_Of_Outfit

Bottom_2_Interval$Body_Type <- Bottom_2_Interval$Body_Type %>%
  fct_recode('Not Big'='0', 'Thick n Juicy'='1', 'Chunky Yet Funky'='2')
levels(Bottom_2_Interval$Body_Type)
Bottom_2_Interval$Body_Type

Bottom_2_Interval$Type_Queen <- Bottom_2_Interval$Type_Queen %>%
  fct_recode('Comedy'='0', 'Pageant'='1', 'Look'='2', 'Other'='3')
levels(Bottom_2_Interval$Type_Queen)
Bottom_2_Interval$Type_Queen

#collapsing factor levels
Bottom_2_Interval$Hometown_City <- Bottom_2_Interval$Hometown_City %>%
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

levels(Bottom_2_Interval$Hometown_City)

Bottom_2_Interval$Hometown_State <- Bottom_2_Interval$Hometown_State %>%
  fct_collapse('New York' = c('New York'),
               'Other' = c('Arizona', 'California', 'Colorado', 'Florida', 'Georgia',
                           'Illinois', 'Indiana', 'Iowa', 'Kentucky', 'Louisiana', 'Massachusetts',
                           'Michigan', 'Minnesota', 'Missouri', 'Nevada', 'New Jersey', 'New Mexico',
                           'North Carolina', 'Ohio', 'Pennsylvania', 'Puerto Rico', 'Tennessee', 'Texas',
                           'Virginia', 'Washington', 'Wisconsin'))

levels(Bottom_2_Interval$Hometown_State)


Bottom_2_Interval$OUTCOME_LOSS <- relevel(Bottom_2_Interval$OUTCOME_LOSS, ref = "0")

Bottom_2_Interval$Hometown_State <- relevel(Bottom_2_Interval$Hometown_State, ref = "New York")

Bottom_2_Interval$Hometown_City <- relevel(Bottom_2_Interval$Hometown_City, ref = "New York")

Bottom_2_Interval$Gender <- relevel(Bottom_2_Interval$Gender, ref = "Cis")

Bottom_2_Interval$Body_Type <- relevel(Bottom_2_Interval$Body_Type, ref = "Not Big")

Bottom_2_Interval$Quality_of_outfit <- relevel(Bottom_2_Interval$Quality_Of_Outfit, ref = "Boot")

Bottom_2_Interval$Wig_Removed <- relevel(Bottom_2_Interval$Wig_Removed, ref = "0")

Bottom_2_Interval$Death_Drop <- relevel(Bottom_2_Interval$Death_Drop, ref = "0")

Bottom_2_Interval$Outfit_Reveal <- relevel(Bottom_2_Interval$Outfit_Reveal, ref = "0")

Bottom_2_Interval$Merle <- relevel(Bottom_2_Interval$Merle, ref = "0")

Bottom_2_Interval$Michelle <- relevel(Bottom_2_Interval$Michelle, ref = "0")

Bottom_2_Interval$Ross <- relevel(Bottom_2_Interval$Ross, ref = "0")

Bottom_2_Interval$Carson <- relevel(Bottom_2_Interval$Carson, ref = "0")

Bottom_2_Interval$Do_they_know_words <- relevel(Bottom_2_Interval$Do_They_Know_Words, ref = "0")

Bottom_2_Interval$Outfit_Reveal_1 <- relevel(Bottom_2_Interval$Outfit_Reveal_1, ref = "0")

Bottom_2_Interval$Sewing <- relevel(Bottom_2_Interval$Sewing, ref = "0")

Bottom_2_Interval$Dancing <- relevel(Bottom_2_Interval$Dancing, ref = "0")

Bottom_2_Interval$Singing <- relevel(Bottom_2_Interval$Singing, ref = "0")

Bottom_2_Interval$Lip_Sync_Ass <- relevel(Bottom_2_Interval$Lip_Sync_Ass, ref = "0")

Bottom_2_Interval$Expressed_Hardship <- relevel(Bottom_2_Interval$Expressed_Hardship, ref = "0")

############# Collapsing Body Type  ###############

Bottom_2_Interval$Body_Type <- Bottom_2_Interval$Body_Type %>%
  fct_collapse('Not Big' = c('Not Big'),
               'Bodacious' = c('Thick n Juicy', 'Chunky Yet Funky'))

Bottom_2_Interval$Body_Type <- Bottom_2_Interval$Body_Type %>%
  fct_recode('Not Big'='0', 'Bodacious'='1')
levels(Bottom_2_Interval$Body_Type)

Bottom_2_Interval$Body_Type <- relevel(Bottom_2_Interval$Body_Type, ref = "0")

Bottom_2_Interval$Body_Type

##############    Collapsing Race    #################

Bottom_2_Interval$Race <- Bottom_2_Interval$Race %>%
  fct_collapse('White' = c('White'),
               'Non-White' = c('Latin','Asian','Black','Other'))

Bottom_2_Interval$Body_Type <- Bottom_2_Interval$Body_Type %>%
  fct_recode('Non-White'='0', 'White'='1')
levels(Bottom_Two_2$Race)

Bottom_2_Interval$Race <- relevel(Bottom_2_Interval$Race, ref = "0")

Bottom_2_Interval$Race

########   Step 1: Univariate Analysis    #########

summary(Null <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))

summary(Age <- glm(OUTCOME_LOSS ~ Age + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Age, test="LRT")

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Hometown_City, test="LRT")

summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Hometown_State, test="LRT")

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Wig_Removed, test="LRT")

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Death_Drop, test="LRT")

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Outfit_Reveal, test="LRT")

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_Of_Outfit + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Quality_Of_Outfit, test="LRT")

summary(Merle <- glm(OUTCOME_LOSS ~ Merle + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Merle, test="LRT")

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Michelle, test="LRT")

summary(Santino <- glm(OUTCOME_LOSS ~ Santino + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Santino, test="LRT")

summary(Ross <- glm(OUTCOME_LOSS ~ Ross + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Ross, test="LRT")

summary(Carson <- glm(OUTCOME_LOSS ~ Carson + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Carson, test="LRT")

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Do_They_Know_Words, test="LRT")

summary(Gender <- glm(OUTCOME_LOSS ~ Gender + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Gender, test="LRT")

summary(Race <- glm(OUTCOME_LOSS ~ Race + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Race, test="LRT")

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Body_Type, test="LRT")

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Type_Queen, test="LRT")

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Sewing, test="LRT")

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Dancing, test="LRT")

summary(Singing <- glm(OUTCOME_LOSS ~ Singing + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Singing, test="LRT")

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Lip_Sync_Ass, test="LRT")

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship + Episode + Bottom_Two_Interval, family=binomial(link='cloglog'), data=Bottom_2_Interval))
anova(Null, Expressed_Hardship, test="LRT")

######     Step 2:   Multivariate Model Comparisons : Backward Elimination Round 1      #######


summary(Full_Model <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Outfit_Reveal + Do_They_Know_Words +
                          Sewing + Dancing + Singing + Expressed_Hardship,
                          family = binomial(link='cloglog'), data = Bottom_2_Interval))

summary(Model_1 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                         Sewing + Dancing + Singing + Expressed_Hardship,
                         family = binomial(link='cloglog'), data = Bottom_2_Interval))

anova(Model_1, Full_Model, test="LRT")   # p value 0.2321 Outfit Reveal excluded from Model 1

summary(Model_2 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                       Sewing + Singing + Expressed_Hardship,
                       family = binomial(link='cloglog'), data = Bottom_2_Interval))

anova(Model_2, Model_1, test="LRT")   # p value 0.2004 Dancing excluded from Model 2

summary(Model_3 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                       Sewing + Expressed_Hardship,
                       family = binomial(link='cloglog'), data = Bottom_2_Interval))

anova(Model_3, Model_2, test="LRT")   # p value 0.1103 Singing excluded from Model 3

# All covariates show meeting significance criteria p < 0.05
# All covariates met criteria of Beta estimates changing no more than 20% compared to Full Model


############   Step 3:    Re-evaluation Of Initial Predictors       #############

# Variables re-evaluated: Age, Hometown_City, Hometown_State, Wig_Removed, Death_Drop,
#             Gender, Race, Body_Type, Type_Queen
# None retained in the model

#summary(Model_4 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
#                      Sewing + Expressed_Hardship,
#                      family = binomial(link='cloglog'), data = Bottom_2_Interval))

#anova(Model_4, Model_3, test="LRT")

# preliminary main effects model

summary(Model_3 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                       Sewing + Expressed_Hardship,
                       family = binomial(link='cloglog'), data = Bottom_2_Interval))

############     Step 4: Testing Linearity of Continuous Variables     ############

# No continuous variables (Episode and Bottom Two Interval do not count)

# Main effects model
summary(Model_3 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                       Sewing + Expressed_Hardship,
                       family = binomial(link='cloglog'), data = Bottom_2_Interval))


##########     Step 5: Assessing First Order Interactions        ##########

#summary(Model_4 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
#                       Sewing + Expressed_Hardship + Sewing:Do_They_Know_Words,
#                       family = binomial(link='cloglog'), data = Bottom_2_Interval))

#anova(Model_4, Model_3, test="LRT")

# no interactions were significant

# preliminary final model

summary(Model_3 <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Do_They_Know_Words +
                       Sewing + Expressed_Hardship,
                       family = binomial(link='cloglog'), data = Bottom_2_Interval))

confint(Model_3)


















