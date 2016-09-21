library(tidyverse)
# Load Raw Data File with Corrections for Values Not Indicated
raw_data <- read_csv(file="rawData.csv",na=c("","NA","-999","-888"))

# Handling Categorical Variables to Factors
categorical_variables <- select(raw_data, group, gender)

## Factor for Group
categorical_variables$group <- as.factor(categorical_variables$group)

## Factor for Gender
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)

# Create sets of s cale items to deal with out of range values)
affective_commitment_items <- select (raw_data, AC1, AC2, AC3, AC4, AC5)
agreeableness_items <- select (raw_data, A1, A2, A3, A4, A5)
extroversion_items <- select (raw_data, E1, E2, E3, E4, E5)

# Inspect variable items
psych::describe(extroversion_items)
psych::describe(agreeableness_items)

# Correcting for outliers in Agreeableness Scale
is_bad_value <- agreeableness_items<1 | agreeableness_items>5
agreeableness_items[is_bad_value] <- NA

# Inspect variable items
psych::describe(affective_commitment_items)

# Correcting for outliers in Affective Commitment Scale
is_bad_value <- affective_commitment_items<1 | affective_commitment_items>7
affective_commitment_items[is_bad_value] <- NA

# Correcting for Reverse-Key Items
agreeableness_items <- mutate(agreeableness_items,A5=6-A5)
affective_commitment_items <- mutate(affective_commitment_items,AC4=8-AC4)
affective_commitment_items <- mutate(affective_commitment_items,AC5=8-AC5)

#Scale scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)$scores
extroversion <- psych::alpha(as.data.frame(extroversion_items),check.keys=FALSE)$scores
affective_commitment <- psych::alpha(as.data.frame(affective_commitment_items),check.keys=FALSE)$scores

#Data Frame for Analytic Data
analytic_data <- cbind(categorical_variables,agreeableness,extroversion,affective_commitment)

#Save analytic data as .Rdata to preserve factor labels
save(analytic_data,file="study1_analytic_data.RData")

#Save as CSV to preserve labels and factor labels (tidyverse)
write_csv(analytic_data,path="study1_analytic_data.csv")

#Save as SAV aka SPSS data file (haven)
library(haven)
write_sav(analytic_data,path="study1_analytic_data.sav")
