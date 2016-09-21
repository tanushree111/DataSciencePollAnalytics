setwd("~/Documents/data science/data sets/2016_presidential_election")
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

#options(max.print=25000)
#setwd("~/Documents/NEU/DataScience/Project/2016_presidential_election")
# Read the primary data
# it will have the following information
# 1. state: state where the primary or caucus was held
# 2. state_abbreviation: two letter state abbreviation
# 3. county: county where the results come from
# 4. fips: FIPS county code
# 5. party: Democrat or Republican
# 6. candidate: name of the candidate
# 7. votes: number of votes the candidate received in the corresponding state and county (may be missing)
# 8. fraction_votes: fraction of votes the president received in the corresponding state, county, and primary
primary_all <- read.csv("primary_results.csv", stringsAsFactors = FALSE,na.strings=c(""))
# check if there are duplicates
primary_all[duplicated(primary_all),]

# "AL" "AZ" "AR" "CO" "FL" "GA" "ID" "IL" "IA" "KY" "LA" "ME" "MA" "MI" "MS" "MO" "NE" "NV" "NH" "NC" "OH" "OK" "SC" "TN" "TX" "UT" "VT" "VA"
# find the states out of 52 which has a voting record. only the above state has a voting rec.
voting_states = unique(primary_all$state_abbreviation)

winner_per_state_county <-  primary_all %>%
    #filter(candidate == 'Hillary Clinton' | candidate == 'Donald Trump') %>%
    group_by(state_abbreviation, county) %>%
    summarise(winner = candidate[which.max(fraction_votes)],
              vote_frac = max(fraction_votes),
              votes = max(votes)) %>%
    mutate(county = toupper(gsub(" county", "", county)))


winner_per_state_county <- select(winner_per_state_county,state_abbreviation,county,winner)

# Read the demographic data[we will be using only the below 19 predictors out of 52 in dataset]
# it will have the following information
# 1.  PST045214	Population, 2014 estimate
# 2.  AGE295214	Persons under 18 years, percent, 2014
# 3.  AGE775214	Persons 65 years and over, percent, 2014
# 4.  SEX255214	Female persons, percent, 2014
# 5.  RHI125214	White alone, percent, 2014 [same as 11, so can be skipped]
# 6.  RHI225214	Black or African American alone, percent, 2014
# 7.  RHI325214	American Indian and Alaska Native alone, percent, 2014
# 8.  RHI425214	Asian alone, percent, 2014
# 9.  RHI525214	Native Hawaiian and Other Pacific Islander alone, percent, 2014
#10.  RHI725214	Hispanic or Latino, percent, 2014
#11.  RHI825214	White alone, not Hispanic or Latino, percent, 2014
#12.  EDU635213	High school graduate or higher, percent of persons age 25+, 2009-2013
#13.  EDU685213	Bachelor's degree or higher, percent of persons age 25+, 2009-2013
#14.  VET605213	Veterans, 2009-2013
#15.  INC910213	Per capita money income in past 12 months (2013 dollars), 2009-2013
#16.  INC110213	Median household income, 2009-2013
#17.  PVY020213	Persons below poverty level, percent, 2009-2013
#18.  LND110210	Land area in square miles, 2010
#19.  POP060210	Population per square mile, 2010

# Read the demographic data[54 demographic description for county state combo]
demographics_all <- read.csv("county_facts.csv", stringsAsFactors = FALSE,na.strings=c(""))
demographics_all[duplicated(demographics_all),]

demographics <- demographics_all %>%
    select(state_abbreviation = state_abbreviation,
           county = area_name,
           population_2014 = PST045214,
           age_under_18 = AGE295214,
           age_over_65 = AGE775214,
           female = SEX255214,
           white = RHI825214, 
           black = RHI225214,
           asian = RHI425214,
           hispanic = RHI725214,
           degree_grad = EDU635213,
           degree_bach = EDU685213,
           veteran = VET605213,
           income = INC910213,
           median_house_income = INC110213,
           below_proverty = PVY020213,
           popluation_density = POP060210) %>%
    mutate(county = (gsub(" Parish", "", county, ignore.case = T))) %>%
    mutate(county = (gsub("SUFFOLK CITY", "SUFFOLK", county, ignore.case = T))) %>%
    mutate(county = (gsub("VIRGINIA BEACH CITY", "VIRGINIA BEACH", county, ignore.case = T))) %>%
    mutate(county = (gsub("NEWPORT NEWS CITY", "NEWPORT NEWS", county, ignore.case = T))) %>%
    mutate(county = (gsub("DEWITT", "DE WITT", county, ignore.case = T))) %>%
    mutate(county = (gsub("HAMPTON CITY", "HAMPTON", county, ignore.case = T))) %>%
    mutate(county = (gsub("NORFOLK CITY", "NORFOLK", county, ignore.case = T))) %>%
    mutate(county = toupper(gsub(" County", "", county, ignore.case = T))) 

demographics_votes_states <- filter(demographics, state_abbreviation %in% voting_states)

#****************************DATA CLEANING STEP*****************************
#***************************************************************************

nrow(unique(demographics_votes_states))
nrow(demographics_votes_states)

nrow(unique(winner_per_state_county))
nrow(winner_per_state_county)

# NA values for state abbreviation
sapply(demographics_votes_states,function(x) sum(is.na(x)))
sapply(winner_per_state_county,function(x) sum(is.na(x)))

sapply(demographics_votes_states, function(x) length(unique(x)))

#filter(winner_per_state_county, county == "NEWPORT NEWS") 
# View(filter(demographics_votes_states, state_abbreviation == "VA")) 

win_rec_demographics <- inner_join(winner_per_state_county, demographics_votes_states, by = c("state_abbreviation","county"))
temp_winner <- winner_per_state_county %>% select(state_abbreviation,county)
demo <- demographics_votes_states %>% select(state_abbreviation,county)
#setdiff(temp_winner,demo)

sapply(winner_per_state_county, function(x) length(unique(x)))
sapply(win_rec_demographics, function(x) length(unique(x)))

nrow(winner_per_state_county)
nrow(win_rec_demographics)


win_rec_demographics_DH <- win_rec_demographics %>% filter(winner == 'Hillary Clinton' | winner == 'Donald Trump')
nrow(filter(winner_per_state_county,winner == 'Hillary Clinton' | winner == 'Donald Trump'))
nrow(win_rec_demographics_DH)

# 0 - Donald Trump 1 - Hillary
win_rec_demographics_DH$winnerF <- factor(win_rec_demographics_DH$winner,levels=c("Donald Trump","Hillary Clinton"))
contrasts(win_rec_demographics_DH$winnerF)

#levels(win_records$winnerF) <- c(0,1)
#levels(win_records$winnerF)
table(win_rec_demographics_DH$winnerF)
head(win_rec_demographics_DH)

#county_extra_inD = sort(setdiff(demographics_votes_states$county, winner_per_state_county$county))
#temp <- demographics_votes_states %>% select(state_abbreviation,county) %>% filter(county %in% county_extra_inD)
#arrange(temp,county)
#filter(winner_per_state_county,county %in% county_extra_inD)
#sort(setdiff(winner_per_state_county$county, demographics_votes_states$county))
#filter(demographics_votes_states,county == "BEDFORD" | county == "BEDFORD CITY")

#WAY1 -USE AMELIA
library(Amelia)
missmap(win_rec_demographics_DH, main = "Missing values vs observed")


#WAY2  - USE COMPLETE.CASES
win_rec_demographics_DH[!complete.cases(win_rec_demographics_DH),]
#demographics[!complete.cases(demographics),]

# STEP 1. get some summary of data and stats 

# columns summary 
str(win_rec_demographics_DH)
head(win_rec_demographics_DH)

# Number of columns
ncol(win_rec_demographics_DH)

# Number of rows
nrow(win_rec_demographics_DH)

# unique records
sapply(win_rec_demographics_DH, function(x) length(unique(x)))


# STEP 2: Check if there is any missing values in the above dataset



#Seems both the dataset has no null values


attach(mtcars)
par(mfrow=c(2,2))



# population_2014 - log transformation required
boxplot(win_rec_demographics_DH$population_2014, main="Population, 2014 estimate")
boxplot(log(win_rec_demographics_DH$population_2014), main="Population, 2014 estimate")
hist(win_rec_demographics_DH$population_2014, main="Population, 2014 estimate")
hist(log(win_rec_demographics_DH$population_2014), main="Population, 2014 estimate")

# age_under_18 - not required
boxplot(win_rec_demographics_DH$age_under_18, main="Persons under 18 years, percent, 2014") 
boxplot(log(win_rec_demographics_DH$age_under_18), main="Persons under 18 years, percent, 2014") 
hist(win_rec_demographics_DH$age_under_18, main="Persons under 18 years, percent, 2014") 
hist(log(win_rec_demographics_DH$age_under_18), main="Persons under 18 years, percent, 2014") 

#age_over_65 - log transformation required
boxplot(win_rec_demographics_DH$age_over_65, main="Persons 65 years and over, percent, 2014")
boxplot(log(win_rec_demographics_DH$age_over_65), main="Persons 65 years and over, percent, 2014") 
hist(win_rec_demographics_DH$age_over_65, main="Persons 65 years and over, percent, 2014")
hist(log(win_rec_demographics_DH$age_over_65), main="Persons 65 years and over, percent, 2014") 

# female - tricky
boxplot(win_rec_demographics_DH$female, main="Female persons, percent, 2014")
boxplot(sqrt(win_rec_demographics_DH$female), main="Female persons, percent, 2014")
hist(win_rec_demographics_DH$female, main="Female persons, percent, 2014")
hist(log(win_rec_demographics_DH$female), main="Female persons, percent, 2014")

# white - tricky
boxplot(demographics$white, main="White alone")
boxplot(log(demographics$white), main="White alone")
hist(demographics$white, main="White alone")
hist(log(demographics$white), main="White alone")

#black - log transformation can be done
boxplot(demographics$black, main="Black or African American alone") 
boxplot(log(demographics$black), main="Black or African American alone")
hist(demographics$black, main="Black or African American alone") 
hist(log(demographics$black), main="Black or African American alone")

# asian - log transformation can be done
boxplot(demographics$asian, main="Asian alone, percent, 2014")
boxplot(log(demographics$asian), main="Asian alone, percent, 2014")
hist(demographics$asian, main="Asian alone, percent, 2014")
hist(log(demographics$asian), main="Asian alone, percent, 2014")

#hispanic - log transformation req
boxplot(demographics$hispanic, main="Hispanic or Latino") 
boxplot(log(demographics$hispanic), main="Hispanic or Latino")
hist(demographics$hispanic, main="Hispanic or Latino") 
hist(log(demographics$hispanic), main="Hispanic or Latino")

#degree_grad
boxplot(demographics$degree_grad, main="High school graduate or higher,") 
boxplot(log(demographics$degree_grad), main="High school graduate or higher,")
hist(demographics$degree_grad, main="High school graduate or higher,") 
hist(log(demographics$degree_grad), main="High school graduate or higher,")

#degree_bach - log trans
boxplot(demographics$degree_bach, main="Bachelor's degree or higher")
boxplot(log(demographics$degree_bach), main="Bachelor's degree or higher")
hist(demographics$degree_bach, main="Bachelor's degree or higher")
hist(log(demographics$degree_bach), main="Bachelor's degree or higher")

# veteran - log transform
boxplot(demographics$veteran, main="Veterans, 2009-2013")
boxplot(log(demographics$veteran), main="Veterans, 2009-2013")
hist(demographics$veteran, main="Veterans, 2009-2013")
hist(log(demographics$veteran), main="Veterans, 2009-2013")

#income - log transform
boxplot(demographics$income, main="Per capita money income in past 12 months") 
boxplot(log(demographics$income), main="Per capita money income in past 12 months") 
hist(demographics$income, main="Per capita money income in past 12 months") 
hist(log(demographics$income), main="Per capita money income in past 12 months") 

#median_house_income - log transform
boxplot(demographics$median_house_income, main="Median household income") 
boxplot(log(demographics$median_house_income), main="Median household income") 
hist(demographics$median_house_income, main="Median household income") 
hist(log(demographics$median_house_income), main="Median household income")

#below_proverty
boxplot(demographics$below_proverty, main="Persons below poverty level")
boxplot(log(demographics$below_proverty), main="Persons below poverty level")
hist(demographics$below_proverty, main="Persons below poverty level")
hist(log(demographics$below_proverty), main="Persons below poverty level")

# popluation_density - log transform
boxplot(demographics$popluation_density, main="Population per square mile, 2010") 
boxplot(log(demographics$popluation_density), main="Population per square mile, 2010") 
hist(demographics$popluation_density, main="Population per square mile, 2010") 
hist(log(demographics$popluation_density), main="Population per square mile, 2010") 



# scatterplot 
attach(mtcars)
par(mfrow=c(1,1))
# corelation and scatter plot
pairs(win_rec_demographics_DH[,c(5:10)])
library(stats)
library(corrplot)
cor_mat <- cor(win_rec_demographics_DH[sapply(win_rec_demographics_DH, is.numeric)])
corrplot(cor_mat,method="circle")
#corrplot(cor_mat,method="number")
#corrplot(cor_mat,method="number")
#corrplot.mixed(cor_mat)
#scatterplotMatrix(win_rec_demographics_DH[sapply(win_rec_demographics_DH, is.numeric)])


train <- win_rec_demographics_DH[sample(1:nrow(win_rec_demographics_DH), 1000, replace=FALSE),]
test <- win_rec_demographics_DH[1001:1402,]

# get the smaple size
smp_size <- floor(0.75 * nrow(win_rec_demographics_DH))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(win_rec_demographics_DH)), size = smp_size)

train <- win_rec_demographics_DH[train_ind, ]
test <- win_rec_demographics_DH[-train_ind, ]

library(stats)
library(corrplot)
cor_mat1 <- cor(train[sapply(train, is.numeric)])
corrplot(cor_mat1,method="circle")

#population_2014/age_over_65/black/asian/hispanic/degree_bach/income/median_house_income/popluation_density

library(BMA)
model_bic<- bic.glm(winnerF ~ 
                     population_2014 + 
                     age_under_18 +
                     age_over_65 +
                     female +
                     white + 
                     black +
                     asian +
                     hispanic +
                     degree_grad + 
                     degree_bach +
                     veteran +
                     income + 
                     median_house_income +
                     below_proverty +
                     popluation_density,
                    glm.family=binomial, data=train) 
summary(model_bic)



model_aic<- glm(winnerF ~ 
                  population_2014 + 
                  age_under_18 +
                  age_over_65 +
                  female +
                  white + 
                  black +
                  asian +
                  hispanic +
                  degree_grad + 
                  degree_bach +
                  veteran +
                  income + 
                  median_house_income +
                  below_proverty +
                  popluation_density,
                  family=binomial, data=train) 

summary(model_aic)


step.aic <- step(model_aic)


backwards <- step(model_full,trace=0) 
formula(backwards)

model_reduced1 <- glm(winnerF ~ white + female + degree_grad,family=binomial, data=train)
summary(model_reduced1)
anova(fit, test="Chisq")

