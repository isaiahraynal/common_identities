###################
### IMPORT DATA ###
###################

setwd("~/Desktop/Dissertation/Common Identities - Journal Submission")
library(readr)
cloudresearch <- read.csv("cloudresearch_data.csv.csv")
#cloudresearch <- subset(cloudresearch, consent_submission == "Submit") # remove people who did not consent to the survey
#cloudresearch <- subset(cloudresearch, Finished == "TRUE") # remove people who didn't complete survey

### Reshape data ###
library(dplyr)
library(tidyr)

#rename variables
cloudresearch <- cloudresearch %>% 
  rename(ft_dem_politicians = ft_1,
         ft_dem_voters = ft_2,
         ft_rep_politicians = ft_3,
         ft_rep_voters = ft_4,
         ft_white_collar = ft_5,
         ft_blue_collar = ft_6,
         order = FL_47_DO,
         profile_binary1 = X1_profile_binary,
         profile_binary2 = X2_profile_binary,
         profile_binary3 = X3_profile_binary,
         profile_binary4 = X4_profile_binary,
         profile_binary5 = X5_profile_binary,
         profile_binary6 = X6_profile_binary,
         profile_binary7 = X7_profile_binary,
         profile_binary8 = X8_profile_binary,
         profile_binary9 = X9_profile_binary,
         profile_binary10 = X10_profile_binary,
         profile_likert1 = X1_profile_likert_1,
         profile_likert2 = X2_profile_likert_1,
         profile_likert3 = X3_profile_likert_1,
         profile_likert4 = X4_profile_likert_1,
         profile_likert5 = X5_profile_likert_1,
         profile_likert6 = X6_profile_likert_1,
         profile_likert7 = X7_profile_likert_1,
         profile_likert8 = X8_profile_likert_1,
         profile_likert9 = X9_profile_likert_1,
         profile_likert10 = X10_profile_likert_1,
         randomoccupation1 = randomocc1,
         randomoccupation2 = randomocc2,
         randomoccupation3 = randomocc3,
         randomoccupation4 = randomocc4,
         randomoccupation5 = randomocc5,
         randomoccupation6 = randomocc6,
         randomoccupation7 = randomocc7,
         randomoccupation8 = randomocc8,
         randomoccupation9 = randomocc9,
         randomoccupation10 = randomocc10,
         randompersonality1 = randomperson1,
         randompersonality2 = randomperson2,
         randompersonality3 = randomperson3,
         randompersonality4 = randomperson4,
         randompersonality5 = randomperson5,
         randompersonality6 = randomperson6,
         randompersonality7 = randomperson7,
         randompersonality8 = randomperson8,
         randompersonality9 = randomperson9,
         randompersonality10 = randomperson10,
         sharedfamily1 = selectionStatus_family1,
         sharedfamily2 = selectionStatus_family2,
         sharedfamily3 = selectionStatus_family3,
         sharedfamily4 = selectionStatus_family4,
         sharedfamily5 = selectionStatus_family5,
         sharedfamily6 = selectionStatus_family6,
         sharedfamily7 = selectionStatus_family7,
         sharedfamily8 = selectionStatus_family8,
         sharedfamily9 = selectionStatus_family9,
         sharedfamily10 = selectionStatus_family10,
         sharedoccupation1 = selectionStatus_occ1,
         sharedoccupation2 = selectionStatus_occ2,
         sharedoccupation3 = selectionStatus_occ3,
         sharedoccupation4 = selectionStatus_occ4,
         sharedoccupation5 = selectionStatus_occ5,
         sharedoccupation6 = selectionStatus_occ6,
         sharedoccupation7 = selectionStatus_occ7,
         sharedoccupation8 = selectionStatus_occ8,
         sharedoccupation9 = selectionStatus_occ9,
         sharedoccupation10 = selectionStatus_occ10,
         sharedparty1 = selectionStatus_party1,
         sharedparty2 = selectionStatus_party2,
         sharedparty3 = selectionStatus_party3,
         sharedparty4 = selectionStatus_party4,
         sharedparty5 = selectionStatus_party5,
         sharedparty6 = selectionStatus_party6,
         sharedparty7 = selectionStatus_party7,
         sharedparty8 = selectionStatus_party8,
         sharedparty9 = selectionStatus_party9,
         sharedparty10 = selectionStatus_party10,
         sharedpersonality1 = selectionStatus_person1,
         sharedpersonality2 = selectionStatus_person2,
         sharedpersonality3 = selectionStatus_person3,
         sharedpersonality4 = selectionStatus_person4,
         sharedpersonality5 = selectionStatus_person5,
         sharedpersonality6 = selectionStatus_person6,
         sharedpersonality7 = selectionStatus_person7,
         sharedpersonality8 = selectionStatus_person8,
         sharedpersonality9 = selectionStatus_person9,
         sharedpersonality10 = selectionStatus_person10,
         sharedhobby1 = selectionStatus_hobby1,
         sharedhobby2 = selectionStatus_hobby2,
         sharedhobby3 = selectionStatus_hobby3,
         sharedhobby4 = selectionStatus_hobby4,
         sharedhobby5 = selectionStatus_hobby5,
         sharedhobby6 = selectionStatus_hobby6,
         sharedhobby7 = selectionStatus_hobby7,
         sharedhobby8 = selectionStatus_hobby8,
         sharedhobby9 = selectionStatus_hobby9,
         sharedhobby10 = selectionStatus_hobby10)

#restructure data so unit of analysis is the profile

# Create individual long format dataframes for each variable group, retaining participantId
profile_binary_long <- cloudresearch %>%
  select(participantId, starts_with("profile_binary")) %>%
  pivot_longer(
    cols = starts_with("profile_binary"),
    names_to = "profile",
    names_pattern = "profile_binary(\\d+)",
    values_to = "profile_binary"
  ) %>%
  mutate(profile = as.integer(profile))

profile_likert_long <- cloudresearch %>%
  select(participantId, starts_with("profile_likert")) %>%
  pivot_longer(
    cols = starts_with("profile_likert"),
    names_to = "profile",
    names_pattern = "profile_likert(\\d+)",
    values_to = "profile_likert"
  ) %>%
  mutate(profile = as.integer(profile))

randomfamily_long <- cloudresearch %>%
  select(participantId, starts_with("randomfamily")) %>%
  pivot_longer(
    cols = starts_with("randomfamily"),
    names_to = "profile",
    names_pattern = "randomfamily(\\d+)",
    values_to = "randomfamily"
  ) %>%
  mutate(profile = as.integer(profile))

randomparty_long <- cloudresearch %>%
  select(participantId, starts_with("randomparty")) %>%
  pivot_longer(
    cols = starts_with("randomparty"),
    names_to = "profile",
    names_pattern = "randomparty(\\d+)",
    values_to = "randomparty"
  ) %>%
  mutate(profile = as.integer(profile))

randomhobby_long <- cloudresearch %>%
  select(participantId, starts_with("randomhobby")) %>%
  pivot_longer(
    cols = starts_with("randomhobby"),
    names_to = "profile",
    names_pattern = "randomhobby(\\d+)",
    values_to = "randomhobby"
  ) %>%
  mutate(profile = as.integer(profile))

randomoccupation_long <- cloudresearch %>%
  select(participantId, starts_with("randomoccupation")) %>%
  pivot_longer(
    cols = starts_with("randomoccupation"),
    names_to = "profile",
    names_pattern = "randomoccupation(\\d+)",
    values_to = "randomoccupation"
  ) %>%
  mutate(profile = as.integer(profile))

randompersonality_long <- cloudresearch %>%
  select(participantId, starts_with("randompersonality")) %>%
  pivot_longer(
    cols = starts_with("randompersonality"),
    names_to = "profile",
    names_pattern = "randompersonality(\\d+)",
    values_to = "randompersonality"
  ) %>%
  mutate(profile = as.integer(profile))

sharedfamily_long <- cloudresearch %>%
  select(participantId, starts_with("sharedfamily")) %>%
  pivot_longer(
    cols = starts_with("sharedfamily"),
    names_to = "profile",
    names_pattern = "sharedfamily(\\d+)",
    values_to = "sharedfamily"
  ) %>%
  mutate(profile = as.integer(profile))

sharedparty_long <- cloudresearch %>%
  select(participantId, starts_with("sharedparty")) %>%
  pivot_longer(
    cols = starts_with("sharedparty"),
    names_to = "profile",
    names_pattern = "sharedparty(\\d+)",
    values_to = "sharedparty"
  ) %>%
  mutate(profile = as.integer(profile))

sharedoccupation_long <- cloudresearch %>%
  select(participantId, starts_with("sharedoccupation")) %>%
  pivot_longer(
    cols = starts_with("sharedoccupation"),
    names_to = "profile",
    names_pattern = "sharedoccupation(\\d+)",
    values_to = "sharedoccupation"
  ) %>%
  mutate(profile = as.integer(profile))

sharedpersonality_long <- cloudresearch %>%
  select(participantId, starts_with("sharedpersonality")) %>%
  pivot_longer(
    cols = starts_with("sharedpersonality"),
    names_to = "profile",
    names_pattern = "sharedpersonality(\\d+)",
    values_to = "sharedpersonality"
  ) %>%
  mutate(profile = as.integer(profile))

sharedhobby_long <- cloudresearch %>%
  select(participantId, starts_with("sharedhobby")) %>%
  pivot_longer(
    cols = starts_with("sharedhobby"),
    names_to = "profile",
    names_pattern = "sharedhobby(\\d+)",
    values_to = "sharedhobby"
  ) %>%
  mutate(profile = as.integer(profile))

# Join all the long dataframes together by participantId and profile
data <- profile_binary_long %>%
  left_join(profile_likert_long, by = c("participantId", "profile")) %>%
  left_join(randomfamily_long, by = c("participantId", "profile")) %>%
  left_join(randomparty_long, by = c("participantId", "profile")) %>%
  left_join(randomhobby_long, by = c("participantId", "profile")) %>%
  left_join(randomoccupation_long, by = c("participantId", "profile")) %>%
  left_join(randompersonality_long, by = c("participantId", "profile")) %>%
  left_join(sharedfamily_long, by = c("participantId", "profile")) %>%
  left_join(sharedparty_long, by = c("participantId", "profile")) %>%
  left_join(sharedoccupation_long, by = c("participantId", "profile")) %>%
  left_join(sharedpersonality_long, by = c("participantId", "profile")) %>%
  left_join(sharedhobby_long, by = c("participantId", "profile"))

######################
###  DATA CLEANING ###
######################

### MAKE SELECTION STATUS NUMERICAL ###

# This is the variable that says whether the attribute shown in the profile was shared or unshared with the respondent

data$sharedfamily[data$sharedfamily == "Selected"] <- 1
data$sharedfamily[data$sharedfamily == "Unselected"] <- 0

data$sharedparty[data$sharedparty == "Selected"] <- 1
data$sharedparty[data$sharedparty == "Unselected"] <- 0

data$sharedhobby[data$sharedhobby == "Selected"] <- 1
data$sharedhobby[data$sharedhobby == "Unselected"] <- 0

data$sharedoccupation[data$sharedoccupation == "Selected"] <- 1
data$sharedoccupation[data$sharedoccupation == "Unselected"] <- 0

data$sharedpersonality[data$sharedpersonality == "Selected"] <- 1
data$sharedpersonality[data$sharedpersonality == "Unselected"] <- 0

data$sharedfamily <- as.numeric(data$sharedfamily)
data$sharedparty <- as.numeric(data$sharedparty)
data$sharedhobby <- as.numeric(data$sharedhobby)
data$sharedoccupation <- as.numeric(data$sharedoccupation)
data$sharedpersonality <- as.numeric(data$sharedpersonality)

# Number of shared attributes
# The purpose of this is to have a variable that says how many of the attributes shown in the profile were shared with the respondent
data$shared <- data$sharedfamily + data$sharedparty + data$sharedhobby + data$sharedoccupation + data$sharedpersonality
data$shared_char <- as.character(data$shared) #indicator

#not including partisanship
data$shared4 <- data$sharedfamily + data$sharedhobby + data$sharedoccupation + data$sharedpersonality

### MAKE OUTCOME VARIABLES NUMERIC ###

# There were both a binary (yes/no) and likert (1-5) outcome questions
# Whether the respondent would connect with the person and how likely

data$profile_binary[data$profile_binary == "Yes"] <- 1
data$profile_binary[data$profile_binary == "No"] <- 0
data$profile_binary <- as.numeric(data$profile_binary)

data$profile_likert <- as.numeric(data$profile_likert)

################
### ANALYSIS ###
################

library(stargazer)
library(webshot)

covariate_labels12 <- c("shared" = "Shared Attributes",
                        "partisanship1" = "Partisanship",
                        "Shared Attributes x Partisanship")

covariate_labels12b <- c("shared_char1" = "1 Shared Attribute",
                         "shared_char2" = "2 Shared Attributes",
                         "shared_char3" = "3 Shared Attributes",
                         "shared_char4" = "4 Shared Attributes",
                         "shared_char5" = "5 Shared Attributes",
                         "partisanship1" = "Partisanship",
                         "shared_char1:partisanship1" = "1 Shared Attribute x Partisanship",
                         "shared_char2:partisanship1" = "2 Shared Attributes x Partisanship",
                         "shared_char3:partisanship1" = "3 Shared Attributes x Partisanship",
                         "shared_char4:partisanship1" = "4 Shared Attributes x Partisanship",
                         "shared_char5:partisanship1" = "5 Shared Attributes x Partisanship")

covariate_labels12c <- c("shared4" = "Shared Non-Political Attributes",
                         "partisanship1" = "Partisanship",
                         "shared4:partisanship1" = "Shared Non-Political Attributes x Partisanship")

covariate_labels34 <- c("sharedfamily" = "Shared Birth Order",
                        "sharedoccupation" = "Shared Occupation",
                        "sharedhobby" = "Shared Hobby",
                        "sharedpersonality" = "Shared Personality",
                        "sharedparty" = "Shared Party",
                        "partisanship1" = "Partisanship",
                        "sharedfamily:partisanship1" = "Shared Birth Order x Partisanship",
                        "sharedoccupation:partisanship1" = "Shared Occupation x Partisanship",
                        "sharedhobby:partisanship1" = "Shared Hobby x Partisanship",
                        "sharedpersonality:partisanship1" = "Shared Personality x Partisanship",
                        "sharedparty:partisanship1" = "Shared Party x Partisanship")

covariate_labels56 <- c("sharedparty" = "Shared Partisanship",
                        "shared4" = "Shared Non-Political Attributes",
                        "sharedparty:shared4" = "Shared Partisanship x Shared Non-Political Attributes")

outcome_labels <- c("profile_binary" = "Desire to Connect",
                    "profile_likert" = "Likelihood of Connecting")


### WITH PARTISAN INTERACTION ###
data_merged <- left_join(data, cloudresearch, by = 'participantId')

data_merged$partisanship[data_merged$partisanship == 'Democrat'] <- 0
data_merged$partisanship[data_merged$partisanship == 'Republican'] <- 1


mod1 <- glm(profile_binary ~ shared + partisanship + shared * partisanship, data = data_merged, family = binomial)
mod2 <- lm(profile_likert ~ shared + partisanship + shared * partisanship, data = data_merged)
list_mod12 <- list(mod1, mod2)
library(sandwich)
#se <- sqrt(diag(vcov(mod1))) #normal standard error
se1_cluster <- sqrt(diag(vcovCL(mod1, cluster = data_merged$participantId))) #clustered
se2_cluster <- sqrt(diag(vcovCL(mod2, cluster = data_merged$participantId)))
se12 <- list(se1_cluster, se2_cluster)
stargazer(list_mod12, type='html', se = se12, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12_interaction.html',
          covariate.labels = covariate_labels12,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


data_merged$shared_char <- as.character(data_merged$shared)
mod1b <- glm(profile_binary ~ shared_char + partisanship + shared_char * partisanship, data = data_merged, family = binomial)
mod2b <- lm(profile_likert ~ shared_char + partisanship + shared_char * partisanship, data = data_merged)
list_mod12b <- list(mod1b, mod2b)
se1b_cluster <- sqrt(diag(vcovCL(mod1b, cluster = data_merged$participantId)))
se2b_cluster <- sqrt(diag(vcovCL(mod2b, cluster = data_merged$participantId)))
se12b <- list(se1b_cluster, se2b_cluster)
stargazer(list_mod12b, type='html', se = se12b, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12b_interaction.html',
          covariate.labels = covariate_labels12b,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

mod1c <- glm(profile_binary ~ shared4 + partisanship + shared4 * partisanship, data = data_merged, family = binomial)
mod2c <- lm(profile_likert ~ shared4 + partisanship + shared4 * partisanship, data = data_merged)
list_mod12c <- list(mod1c, mod2c)
se1c_cluster <- sqrt(diag(vcovCL(mod1c, cluster = data_merged$participantId)))
se2c_cluster <- sqrt(diag(vcovCL(mod2c, cluster = data_merged$participantId)))
se12c <- list(se1c_cluster, se2c_cluster)
stargazer(list_mod12c, type='html', se = se12c, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12c_interaction.html',
          covariate.labels = covariate_labels12c,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


mod3 <- glm(profile_binary ~ sharedfamily + sharedoccupation + sharedhobby + sharedpersonality + sharedparty + partisanship +
                    sharedfamily * partisanship + sharedoccupation * partisanship + sharedhobby * partisanship + sharedpersonality * partisanship + sharedparty * partisanship, 
                 data = data_merged, family = binomial)
mod4 <- lm(profile_likert ~ sharedfamily + sharedoccupation + sharedhobby + sharedpersonality + sharedparty + partisanship +
                   sharedfamily * partisanship + sharedoccupation * partisanship + sharedhobby * partisanship + sharedpersonality * partisanship + sharedparty * partisanship,
                 data = data_merged)
list_mod34 <- list(mod3, mod4)
se3_cluster <- sqrt(diag(vcovCL(mod3, cluster = data_merged$participantId)))
se4_cluster <- sqrt(diag(vcovCL(mod4, cluster = data_merged$participantId)))
se34 <- list(se3_cluster, se4_cluster)
stargazer(list_mod34, type='html', se = se34, digits=3,
          dep.var.labels= outcome_labels, out = 'mod34_interaction.html',
          covariate.labels = covariate_labels34,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


### WITHOUT PARTISAN INTERACTION
model1 <- glm(profile_binary ~ shared, data = data, family = binomial)
m1 <- glm.cluster(profile_binary ~ shared, cluster = 'participantId', data = data, family = binomial)
model2 <- lm(profile_likert ~ shared, data = data)
list_model12 <- list(model1, model2)
library(sandwich)
#se <- sqrt(diag(vcov(mod1))) #normal standard error
se1_cluster <- sqrt(diag(vcovCL(model1, cluster = data$participantId))) #clustered
se2_cluster <- sqrt(diag(vcovCL(model2, cluster = data$participantId)))
se12 <- list(se1_cluster, se2_cluster)
stargazer(list_model12, type='html', se = se12, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12_original.html',
          covariate.labels = covariate_labels12,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

model1b <- glm(profile_binary ~ shared_char, data = data, family = binomial)
model2b <- lm(profile_likert ~ shared_char, data = data)
list_model12b <- list(model1b, model2b)
se1b_cluster <- sqrt(diag(vcovCL(model1b, cluster = data$participantId)))
se2b_cluster <- sqrt(diag(vcovCL(model2b, cluster = data$participantId)))
se12b <- list(se1b_cluster, se2b_cluster)
stargazer(list_model12b, type='html', se = se12b, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12b_original.html',
          covariate.labels = covariate_labels12b,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

model1c <- glm(profile_binary ~ shared4, data = data, family = binomial)
model2c <- lm(profile_likert ~ shared4, data = data)
list_model12c <- list(model1c, model2c)
se1c_cluster <- sqrt(diag(vcovCL(model1c, cluster = data$participantId)))
se2c_cluster <- sqrt(diag(vcovCL(model2c, cluster = data$participantId)))
se12c <- list(se1c_cluster, se2c_cluster)
stargazer(list_model12c, type='html', se = se12c, digits=3,
          dep.var.labels= outcome_labels, out = 'mod12c_original.html',
          covariate.labels = covariate_labels12c,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

model3 <- glm(profile_binary ~ sharedfamily + sharedoccupation + sharedhobby + sharedpersonality + sharedparty, 
            data = data, family = binomial)
model4 <- lm(profile_likert ~ sharedfamily + sharedoccupation + sharedhobby + sharedpersonality + sharedparty,
           data = data)
list_model34 <- list(model3, model4)
se3_cluster <- sqrt(diag(vcovCL(model3, cluster = data$participantId)))
se4_cluster <- sqrt(diag(vcovCL(model4, cluster = data$participantId)))
se34 <- list(se3_cluster, se4_cluster)
stargazer(list_model34, type='html', se = se34, digits=3,
          dep.var.labels= outcome_labels, out = 'mod34_original.html',
          covariate.labels = covariate_labels34,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

model5 <- glm(profile_binary ~ sharedparty + shared4 + sharedparty * shared4,
                   data = data, family = binomial)
model6 <-lm(profile_likert ~ sharedparty + shared4 + sharedparty * shared4,
                 data = data, family = binomial)
list_model56 <- list(model5, model6)
se5_cluster <- sqrt(diag(vcovCL(model5, cluster = data$participantId)))
se6_cluster <- sqrt(diag(vcovCL(model6, cluster = data$participantId)))
se56 <- list(se5_cluster, se6_cluster)
stargazer(list_model56, type='html', se = se56, digits=3,
          dep.var.labels= outcome_labels, out = 'mod56_original.html',
          covariate.labels = covariate_labels56,
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

# Indicator for 1, 2, 3, 4, 5
data$shared <- as.character(data$shared)
data$shared4 <- as.character(data$shared4)
summary(glm(data$profile_binary ~ data$shared, family = binomial))
summary(lm(data$profile_likert ~ data$shared))
summary(glm(data$profile_binary ~ data$shared4, family = binomial))
summary(lm(data$profile_likert ~ data$shared4))

###############################################
### DEMOGRAPHICS AND DESCRIPTIVE STATISTICS ###
###############################################

## FROM CLOUDRESEARCH

platformdata <- read.csv("platform_data.csv")
platformdata <- subset(platformdata, Status == "Approved") # only keep people who completed and who I accepted
platformdata <- subset(platformdata, CountryOfResidence == "United States") #only keep US

platformdata$Age[platformdata$Age == "18"] <- "<20"
platformdata$Age[platformdata$Age == "19"] <- "<20"
platformdata$Age[platformdata$Age == "20"] <- "20s"
platformdata$Age[platformdata$Age == "21"] <- "20s"
platformdata$Age[platformdata$Age == "22"] <- "20s"
platformdata$Age[platformdata$Age == "23"] <- "20s"
platformdata$Age[platformdata$Age == "24"] <- "20s"
platformdata$Age[platformdata$Age == "25"] <- "20s"
platformdata$Age[platformdata$Age == "26"] <- "20s"
platformdata$Age[platformdata$Age == "27"] <- "20s"
platformdata$Age[platformdata$Age == "28"] <- "20s"
platformdata$Age[platformdata$Age == "29"] <- "20s"
platformdata$Age[platformdata$Age == "30"] <- "30s"
platformdata$Age[platformdata$Age == "31"] <- "30s"
platformdata$Age[platformdata$Age == "32"] <- "30s"
platformdata$Age[platformdata$Age == "33"] <- "30s"
platformdata$Age[platformdata$Age == "34"] <- "30s"
platformdata$Age[platformdata$Age == "35"] <- "30s"
platformdata$Age[platformdata$Age == "36"] <- "30s"
platformdata$Age[platformdata$Age == "37"] <- "30s"
platformdata$Age[platformdata$Age == "38"] <- "30s"
platformdata$Age[platformdata$Age == "39"] <- "30s"
platformdata$Age[platformdata$Age == "40"] <- "40s"
platformdata$Age[platformdata$Age == "41"] <- "40s"
platformdata$Age[platformdata$Age == "42"] <- "40s"
platformdata$Age[platformdata$Age == "43"] <- "40s"
platformdata$Age[platformdata$Age == "44"] <- "40s"
platformdata$Age[platformdata$Age == "45"] <- "40s"
platformdata$Age[platformdata$Age == "46"] <- "40s"
platformdata$Age[platformdata$Age == "47"] <- "40s"
platformdata$Age[platformdata$Age == "48"] <- "40s"
platformdata$Age[platformdata$Age == "49"] <- "40s"
platformdata$Age[platformdata$Age == "50"] <- "50s"
platformdata$Age[platformdata$Age == "51"] <- "50s"
platformdata$Age[platformdata$Age == "52"] <- "50s"
platformdata$Age[platformdata$Age == "53"] <- "50s"
platformdata$Age[platformdata$Age == "54"] <- "50s"
platformdata$Age[platformdata$Age == "55"] <- "50s"
platformdata$Age[platformdata$Age == "56"] <- "50s"
platformdata$Age[platformdata$Age == "57"] <- "50s"
platformdata$Age[platformdata$Age == "58"] <- "50s"
platformdata$Age[platformdata$Age == "59"] <- "50s"
platformdata$Age[platformdata$Age == "60"] <- "60s"
platformdata$Age[platformdata$Age == "61"] <- "60s"
platformdata$Age[platformdata$Age == "62"] <- "60s"
platformdata$Age[platformdata$Age == "63"] <- "60s"
platformdata$Age[platformdata$Age == "64"] <- "60s"
platformdata$Age[platformdata$Age == "65"] <- "60s"
platformdata$Age[platformdata$Age == "66"] <- "60s"
platformdata$Age[platformdata$Age == "67"] <- "60s"
platformdata$Age[platformdata$Age == "68"] <- "60s"
platformdata$Age[platformdata$Age == "69"] <- "60s"
platformdata$Age[platformdata$Age == "70"] <- "70s"
platformdata$Age[platformdata$Age == "71"] <- "70s"
platformdata$Age[platformdata$Age == "72"] <- "70s"
platformdata$Age[platformdata$Age == "73"] <- "70s"
platformdata$Age[platformdata$Age == "74"] <- "70s"
platformdata$Age[platformdata$Age == "75"] <- "70s"
platformdata$Age[platformdata$Age == "76"] <- "70s"
platformdata$Age[platformdata$Age == "77"] <- "70s"
platformdata$Age[platformdata$Age == "78"] <- "70s"
platformdata$Age[platformdata$Age == "80"] <- "80s"
platformdata$Age[platformdata$Age == "83"] <- "80s"
table(platformdata$Age)

table(platformdata$Education)
table(platformdata$Sex)
table(platformdata$OccupationField)
table(platformdata$RelationshipMaritalStatus)
table(platformdata$PoliticalParty)
table(platformdata$Ethnicity)
table(platformdata$Gender)
table(platformdata$HouseholdIncome)
table(platformdata$Race)
table(platformdata$EmploymentStatus)

## FROM MY EXPERIMENT
table(cloudresearch$partisanship)
table(cloudresearch$ideology)
table(cloudresearch$gender)
table(cloudresearch$gender_4_TEXT)
table(cloudresearch$race)
table(cloudresearch$ethnicity)
table(cloudresearch$education)
table(cloudresearch$geography)
table(cloudresearch$social_media)
table(cloudresearch$dating_apps)
table(cloudresearch$siblings)
table(cloudresearch$children)
table(cloudresearch$grandchildren)
table(cloudresearch$hobbies_interests)
table(cloudresearch$occupation)
table(cloudresearch$personality)

summary(cloudresearch$ft_dem_politicians)
summary(cloudresearch$ft_dem_voters)
summary(cloudresearch$ft_rep_politicians)
summary(cloudresearch$ft_rep_voters)
summary(cloudresearch$ft_white_collar)
summary(cloudresearch$ft_blue_collar)
