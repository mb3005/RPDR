
library(tidyverse)

#to correct for parsing failures; incorrect "logical" class for columns: bottom3, bottom4, bottom5, bottom 6

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



