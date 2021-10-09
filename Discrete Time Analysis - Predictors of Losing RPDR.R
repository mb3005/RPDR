library(tidyverse)

RPDR <- read_csv(file.path("Data Files", "Bottom_Two_Interval.csv"))

RPDR <- RPDR %>%
  mutate_all(as.factor)

view(RPDR)

RPDR <- RPDR %>%
  mutate_at(vars('Age','Episode', 'Bottom_Two_Interval', 'Numqueens', 'Sumqueen', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.character)

RPDR <- RPDR %>%
  mutate_at(vars('Age','Episode', 'Bottom_Two_Interval', 'Numqueens', 'Sumqueen', 'Numcontests', 'Perc_High', 'Perc_Win', 'Perc_Winhigh', 'Perc_Low',
                 'Perc_Btm', 'Perc_Lowbtm', 'Num_High', 'Num_Win', 'Num_Winhigh',
                 'Num_Btm', 'Num_Low', 'Num_Lowbtm', 'Db_Score', 'Points', 'PPE'),
            as.numeric)

str(RPDR)

RPDR$DOB <- as.Date(as.numeric(as.character(RPDR$DOB)),origin="1960-01-01")
RPDR$Airdate <- as.Date(RPDR$Airdate,format="%Y-%m-%d")


# recoding factor levels

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

#collapsing factor levels
RPDR$Hometown_City <- RPDR$Hometown_City %>%
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

levels(RPDR$Hometown_City)

RPDR$Hometown_State <- RPDR$Hometown_State %>%
  fct_collapse('New York' = c('New York'),
               'Other' = c('Arizona', 'California', 'Colorado', 'Florida', 'Georgia',
                           'Illinois', 'Indiana', 'Iowa', 'Kentucky', 'Louisiana', 'Massachusetts',
                           'Michigan', 'Minnesota', 'Missouri', 'Nevada', 'New Jersey', 'New Mexico',
                           'North Carolina', 'Ohio', 'Pennsylvania', 'Puerto Rico', 'Tennessee', 'Texas',
                           'Virginia', 'Washington', 'Wisconsin'))

levels(RPDR$Hometown_State)


RPDR$OUTCOME_LOSS <- relevel(RPDR$OUTCOME_LOSS, ref = "0")

RPDR$Hometown_State <- relevel(RPDR$Hometown_State, ref = "New York")

RPDR$Hometown_City <- relevel(RPDR$Hometown_City, ref = "New York")

RPDR$Gender <- relevel(RPDR$Gender, ref = "Cis")

RPDR$Body_Type <- relevel(RPDR$Body_Type, ref = "Not Big")

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

############# Collapsing Body Type  ###############

RPDR$Body_Type <- RPDR$Body_Type %>%
  fct_collapse('Not Big' = c('Not Big'),
               'Bodacious' = c('Thick n Juicy', 'Chunky Yet Funky'))

RPDR$Body_Type <- RPDR$Body_Type %>%
  fct_recode('Not Big'='0', 'Bodacious'='1')
levels(RPDR$Body_Type)

RPDR$Body_Type <- relevel(RPDR$Body_Type, ref = "0")

RPDR$Body_Type

##############    Collapsing Race    #################

RPDR$Race <- RPDR$Race %>%
  fct_collapse('White' = c('White'),
               'Non-White' = c('Latin','Asian','Black','Other'))

RPDR$Body_Type <- RPDR$Body_Type %>%
  fct_recode('Non-White'='0', 'White'='1')
levels(Bottom_Two_2$Race)

RPDR$Race <- relevel(RPDR$Race, ref = "0")

RPDR$Race

########   Step 1: Univariate Analysis    #########

summary(Null <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))

summary(Age <- glm(OUTCOME_LOSS ~ Age + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Age, test="LRT")

summary(Hometown_City <- glm(OUTCOME_LOSS ~ Hometown_City + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Hometown_City, test="LRT")

summary(Hometown_State <- glm(OUTCOME_LOSS ~ Hometown_State + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Hometown_State, test="LRT")

summary(Wig_Removed <- glm(OUTCOME_LOSS ~ Wig_Removed + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Wig_Removed, test="LRT")

summary(Death_Drop <- glm(OUTCOME_LOSS ~ Death_Drop + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Death_Drop, test="LRT")

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Outfit_Reveal, test="LRT")

summary(Quality_Of_Outfit <- glm(OUTCOME_LOSS ~ Quality_Of_Outfit + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Quality_Of_Outfit, test="LRT")

summary(Merle <- glm(OUTCOME_LOSS ~ Merle + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Merle, test="LRT")

summary(Michelle <- glm(OUTCOME_LOSS ~ Michelle + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Michelle, test="LRT")

summary(Santino <- glm(OUTCOME_LOSS ~ Santino + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Santino, test="LRT")

summary(Ross <- glm(OUTCOME_LOSS ~ Ross + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Ross, test="LRT")

summary(Carson <- glm(OUTCOME_LOSS ~ Carson + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Carson, test="LRT")

summary(Do_They_Know_Words <- glm(OUTCOME_LOSS ~ Do_They_Know_Words + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Do_They_Know_Words, test="LRT")

summary(Outfit_Reveal <- glm(OUTCOME_LOSS ~ Outfit_Reveal + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Outfit_Reveal, test="LRT")

summary(Gender <- glm(OUTCOME_LOSS ~ Gender + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Gender, test="LRT")

summary(Race <- glm(OUTCOME_LOSS ~ Race + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Race, test="LRT")

summary(Body_Type <- glm(OUTCOME_LOSS ~ Body_Type + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Body_Type, test="LRT")

summary(Type_Queen <- glm(OUTCOME_LOSS ~ Type_Queen + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Type_Queen, test="LRT")

summary(Sewing <- glm(OUTCOME_LOSS ~ Sewing + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Sewing, test="LRT")

summary(Dancing <- glm(OUTCOME_LOSS ~ Dancing + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Dancing, test="LRT")

summary(Singing <- glm(OUTCOME_LOSS ~ Singing + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Singing, test="LRT")

summary(Lip_Sync_Ass <- glm(OUTCOME_LOSS ~ Lip_Sync_Ass + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Lip_Sync_Ass, test="LRT")

summary(Expressed_Hardship <- glm(OUTCOME_LOSS ~ Expressed_Hardship + Episode + Bottom_Two_Interval, family=binomial(link='logit'), data=RPDR))
anova(Null, Expressed_Hardship, test="LRT")

######     Step 2:   Multivariate Model Comparisons : Backward Elimination Round 1      #######


summary(Full_Model <- glm(OUTCOME_LOSS ~ Episode + Bottom_Two_Interval + Outfit_Reveal + Do_They_Know_Words +
                          Sewing + Dancing + Singing + Lip_Sync_Ass + Expressed_Hardship +
                          family = binomial(link='logit'), data = RPDR))



