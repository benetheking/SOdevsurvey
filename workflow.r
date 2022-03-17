## setup
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)
library(workflows)
library(tune)
library(gbm)

## ideas and inspirations for later:
## SOPartFreq has a lot of NAs, but might be useful - 
## if I could do an impute step, it would help greatly!!!
## maybe do the WF with CARET ???

###############################################
## get data
getwd()
setwd("C:/Users/benedikt.kreutzer/Desktop/emet")
dat1 <- read_csv("survey_results_public.csv")
## remove "comptotal" and "ResponseId"
dat1 <- dat1 %>% select(-CompTotal, -ResponseId)
## overview
dat1 %>% names()
dat1 %>% glimpse()

##########
enter_max_salary <- 350000 ## this is kinda...big!
##########

## target var: "ConvertedCompYearly"
dat1$ConvertedCompYearly # target var has NAs - remove them
d2 <- dat1 %>% 
  filter(!is.na(ConvertedCompYearly)) %>% 
  filter(ConvertedCompYearly < enter_max_salary )

## look at NAs
d2 %>% map_df(., ~sum(is.na(.x))) %>% t() %>% as.data.frame() %>% arrange(desc(V1))

## let's focus on the most promising variables - good for prediction, and not too many NAs
d3 <- d2 %>% select(ConvertedCompYearly, Country, Currency, YearsCode, YearsCodePro, 
                    EdLevel, Age, OrgSize, CompFreq, MainBranch, 
                    Employment, SOAccount, Gender, SurveyLength, Age1stCode, 
                    OpSys, #NEWSOSites, #NEWStuck, DevType, LearnCode,
                    LanguageHaveWorkedWith, ToolsTechHaveWorkedWith, DatabaseHaveWorkedWith, 
                    LanguageWantToWorkWith, NEWCollabToolsWantToWorkWith)
## had to drop the variables c( LanguageWouldLikeWorkedWith, Employment, SOAccount)
## the would require too much feature engineering...

d3 %>% select(15:17) %>% map(.x=., ~table(.x))
d3 %>% count(SOPartFreq) %>% arrange(desc(n))

## after looking at the variables, let's do some filtering!
d4 <- d3 %>% filter(#Age != "Under 18 years old", Age != "Prefer not to say", Age != "65 years or older",
                    OrgSize != "I don't know", OrgSize != "Just me - I am a freelancer, sole proprietor, etc.")

d4 %>% select(2:4) %>% map(.x=., ~table(.x))
d4 %>% count(Currency) %>% arrange(desc(n))

################################################
### Feature Engineering
## do a country filter for most numerous countries
countr_filt <- d4 %>% select(Country) %>% 
  count(Country) %>% arrange(desc(n)) %>% 
  filter(n>50) %>% 
  select(Country) %>% pull()

d5 <- d4 %>% 
  filter(Country %in% countr_filt) %>% 
  separate(OrgSize, into = "OrgSize", sep=" ", extra = "drop") %>% 
  separate(Age, into = "Age", sep ="-", extra = "drop") %>% 
  mutate(Currency     = str_sub(Currency, 1, 3),
         Currency     = ifelse(Currency == "EUR" | Currency == "USD" | Currency == "INR" | Currency == "GBP", Currency, "OTH"),
         Country      = str_sub(Country, 1, 5),
         EdLevel      = str_sub(EdLevel, 1, 5),
        # YearsCode    = as.numeric(YearsCode),
        # YearsCodePro = as.numeric(YearsCodePro),
        # Age          = as.numeric(Age) + 4,
         OrgSize      = as.numeric(str_replace(OrgSize, pattern = ",", replace ="")))

d5 %>% select(ConvertedCompYearly, YearsCode, YearsCodePro, Age) %>% cor()
d5 %>% select(OrgSize) %>% table() %>% sort()

d6 <- d5 %>% 
  mutate(lang_have = str_count(LanguageHaveWorkedWith, pattern = ";") +1,
         lang_want = str_count(LanguageWantToWorkWith, pattern = ";") +1, 
         tool_have = str_count(ToolsTechHaveWorkedWith, pattern = ";") +1,
         coll_want = str_count(NEWCollabToolsWantToWorkWith, pattern = ";") +1,
         dbase_have = str_count(DatabaseHaveWorkedWith, pattern = ";") +1  ) %>% 
  separate(LanguageHaveWorkedWith , into = c("Lang1", "Lang2", "Lang3"), sep = ";", extra = "drop") %>% 
  separate(ToolsTechHaveWorkedWith, into = c("Tool1", "Tool2"), sep = ";", extra = "drop") %>% 
  
  select(-DatabaseHaveWorkedWith, -LanguageWantToWorkWith, -NEWCollabToolsWantToWorkWith) %>% 
  
  replace_na(replace = list(lang_have = 0, lang_want = 0, tool_have = 0, coll_want = 0, dbase_have = 0,
             EdLevel = "none", Lang1 = "none", Lang2 = "none", Lang3 = "none", 
             Tool1 = "none", Tool2 = "none",
             YearsCode = "0", YearsCodePro = "0", Age = "0", OpSys = "none", Age1stCode = "none")) %>% 
  filter(!is.na(SOAccount), !is.na(SurveyLength), !is.na(Employment), !is.na(Gender)) %>% 
  
  filter(Currency != "OTH")

d6 %>% map(~sum(is.na(.)))
d6 %>% select(SurveyLength) %>% table() %>% sort()

d6 %>% filter(is.na(YearsCode))

#d6 <- d5 %>% 
#  separate(LanguageHaveWorkedWith , into = c("Lang1", "Lang2", "Lang3"), sep = ";", extra = "drop") %>% 
#  separate(ToolsTechHaveWorkedWith, into = c("Tool1", "Tool2"), sep = ";", extra = "drop") %>% 
#  replace_na(replace = list(EdLevel = "none", Lang1 = "none", Lang2 = "none", Lang3 = "none", 
#                            Tool1 = "none", Tool2 = "none")) %>% 
#  mutate(Tool1 = gsub("[[:space:]]", "", Tool1),
#         Tool2 = str_replace(Tool2, " ", ""),
#         Lang1 = str_replace(Lang1, '/', "") ) 
# d6 %>% select(Lang1) %>% table()
# d6 %>% select(Tool2) %>% table() 

###############################################
### TIDYMODELLING
## test-train setup
dat_split <- initial_split(d6, prop = 0.7)
dat_split
# extract training and testing sets
d_train <- training(dat_split)
d_test <- testing(dat_split)
d_cv <- vfold_cv(d_train, v = 5)   ## create CV object from training data

# modelling setup
d_recipe <- recipe(ConvertedCompYearly  ~ . ,  data = dat_split) %>% ## this is the model equation
  step_normalize(all_numeric()) %>%
  step_impute_knn(EdLevel) ## don't have any NAs in this dataset, but could easily knn-impute them here

d_recipe <- recipe(ConvertedCompYearly  ~ . ,  data = d6)

## set up Random Forest
rf_model <- rand_forest() %>%
  set_args(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") 
## GBM ??
#gbm_model <- logistic_reg() %>%
#  set_engine("gbm") %>%
#  set_mode("classification") 

# set the workflow
rf_workflow <- workflow() %>%
  add_recipe(d_recipe) %>%
  add_model(rf_model)

# specify mtry values
# rf_grid <- expand.grid(mtry = c(2, 3, 4, 5), trees = c(250, 500))
rf_grid <- expand.grid(mtry = c(5, 7, 9, 11, 13))
rf_grid
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = d_cv,
            grid = rf_grid, # grid of values to try
            metrics = metric_set(rmse, rsq) # metrics we care about
  )

# print results
rf_tune_results %>% collect_metrics()

rf_fit <- rf_workflow %>%
  last_fit(dat_split)

#######################################
## have to do it differently...
# t-t-split
splitter <- sample(1:nrow(d6), size = 0.7*nrow(d6))
trainer <- d6[splitter,]
tester <- d6[-splitter,]

## Random Forest
easy_model <- ranger(ConvertedCompYearly  ~ ., data = trainer, mtry = 7, num.trees = 2000, verbose = T)
easy_model
easy_model2 <- ranger(ConvertedCompYearly  ~ Currency + YearsCode + YearsCodePro + Age, data = trainer, mtry = 2, num.trees = 500)
easy_model2
predicter <- predict(easy_model, tester)
predicter <- predict(easy_model2, tester)

cor(predicter$predictions, tester$ConvertedCompYearly)^2 ## Rsq

ts.plot(tester$ConvertedCompYearly[1:100])
lines(predicter$predictions[1:100], col="Red")

easy_model2 <- ranger(ConvertedCompYearly ~ Currency + YearsCode + YearsCodePro + Age + OrgSize, 
                      data = trainer, mtry = 2, num.trees = 500)
easy_model2

## GBM model
trainer2 <- data.frame(trainer, stringsAsFactors = TRUE)
str(trainer2)

trainer %>% map_if(is.factor, as.character)



easy_model_gbm <- gbm::gbm(ConvertedCompYearly ~ ., 
                           data = trainer2, distribution = "gaussian", n.trees = 65000, verbose = T, 
                           interaction.depth = 6, shrinkage = 0.00015)

trainer %>% map(., ~ifelse(is.character(.)==T, as.factor(.), .))

gbm.perf(easy_model_gbm, oobag.curve = T)
bla <- predict(easy_model_gbm, tester)

cor(bla, tester$ConvertedCompYearly)^2 ## Rsq
