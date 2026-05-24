library(haven)
library(nrba)
library(survey)
library(jtools)
library(dplyr)
library(readxl)
library(patchwork)
library(reshape2)

steps <- read_dta(".............\\steps_2020_7.11.2021_01_p3_final_4.1.2.dta")


age_intervals <- c(18,25,35,45,55,65,70,Inf)
category_labels <- c("18-24", "25-34", "35-44", "45-54","55-64","65-70",'>= 70')
steps$age_category <- cut(steps$age, age_intervals, labels = seq_along(category_labels) - 1, right = FALSE)

age_intervals2 <- c(18,25,40,60,Inf)
category_labels2 <- c("18-24", "25-39", "40-59", '>= 60')
steps$age_category2 <- cut(steps$age, age_intervals2, labels = seq_along(category_labels2) - 1, right = FALSE)

steps <- steps %>% mutate(age_category3 = case_when(
  age_category2 == 0 ~ NA,
  age_category2 == 1 ~ 0,
  age_category2 == 2 ~ 1,
  age_category2 == 3 ~ 2,
  TRUE ~ NA_real_
))

steps$sex <- steps$c1
steps$area <- steps$area

steps$education <- steps$i20 
education_labels <- c("0", "1-6", "7-11", ">=12") 

steps$marriagestatus <- ifelse(steps$marriagestatus %in% c(2, 3), 2, steps$marriagestatus)
marriagestatus_labels <- c("Single", "Married", "Divorced/widow") 

steps$insurance <- ifelse(steps$x3d1 == 0 & steps$x4 == 0 ,0,
                          ifelse((steps$x3d1 == 1 & steps$x4 == 0) | (steps$x3d1 == 0 & steps$x4 == 1),1,2))
insurance_labels <- c("No insurance", "Basic", "Basic+Complementary") 

steps$job <- steps$i21
steps <- steps %>%
  mutate(job = case_when(
    job == 1 ~ 1,
    job == 2 ~ 1,
    job == 3 ~ 1,
    job == 4 ~ 1,
    job == 5 ~ 1,
    job == 6 ~ 2,
    job == 10 ~ 3,
    job == 11 ~ 0,
    job == 12 ~ 0,
    job == 13 ~ 0,
    TRUE ~ NA_real_
  ))

steps$WI_National <- steps$WI_National - 1 

steps$salt_shaker_lastmeal <- steps$d13

steps$salt_addition_eating <- steps$dsf
steps <- steps %>%
  mutate(salt_addition_eating = case_when(
    salt_addition_eating == 5 ~ 0,
    salt_addition_eating == 4 ~ 1,
    salt_addition_eating == 3 ~ 1,
    salt_addition_eating == 2 ~ 2,
    salt_addition_eating == 1 ~ 2,
    TRUE ~ NA_real_
  ))

steps$salt_addition_cooking <- steps$d12a
steps <- steps %>%
  mutate(salt_addition_cooking = case_when(
    salt_addition_cooking == 5 ~ 0,
    salt_addition_cooking == 4 ~ 1,
    salt_addition_cooking == 3 ~ 1,
    salt_addition_cooking == 2 ~ 2,
    salt_addition_cooking == 1 ~ 2,
    TRUE ~ NA_real_
  ))


steps$salty_food <- steps$d15a
steps <- steps %>%
  mutate(salty_food = case_when(
    salty_food == 5 ~ 0,
    salty_food == 4 ~ 1,
    salty_food == 3 ~ 1,
    salty_food == 2 ~ 2,
    salty_food == 1 ~ 2,
    TRUE ~ NA_real_
  ))

steps$salt_amount_self <- steps$d14a
steps <- steps %>%
  mutate(salt_amount_self = case_when(
    salt_amount_self == 5 ~ 0,
    salt_amount_self == 4 ~ 0,
    salt_amount_self == 3 ~ 1,
    salt_amount_self == 2 ~ 2,
    salt_amount_self == 1 ~ 2,
    TRUE ~ NA_real_
  ))

steps$salt_health <- steps$d17

steps$salt_reduce_importance <- steps$d18
steps <- steps %>%
  mutate(salt_reduce_importance = case_when(
    salt_reduce_importance == 3 ~ 0,
    salt_reduce_importance == 2 ~ 1,
    salt_reduce_importance == 1 ~ 2,
    TRUE ~ NA_real_
  ))

steps$salt_recom <- steps$h20b

steps$fruit_app <- steps$d2_2serving
steps$veg_app <- steps$d4_3serving
steps$fast_food <- ifelse(steps$d8n == 1,0,1)
steps$low_activity <- steps$low_activity

steps[(steps$s1b %in% c(-555)),]$s1b <- 0
steps$smoking_status <- ifelse(is.na(steps$s1a) == TRUE,NA,
                               ifelse(steps$s1a == 0 ,0,
                                      ifelse(steps$s1a == 1 & steps$s1b == 0,1,2)))
steps$smoking_current <- steps$s1b

#HTN cascade of care 1400
steps$taking_htn_drug <- steps$h3c 
steps[(steps$taking_htn_drug %in% c(-555)),]$taking_htn_drug <- 0
steps$MeanSys_ag <- steps$MeanSys
steps$MeanDias_ag <- steps$MeanDias
steps$HTN <- ifelse((is.na(steps$MeanSys_ag) | is.na(steps$MeanDias_ag)),
                    NA,
                    ifelse(steps$MeanSys_ag >= 140 | steps$MeanDias_ag >= 90 | steps$taking_htn_drug == 1,1,0))

steps$ever_told_HTN <- steps$h2e 
steps[(steps$ever_told_HTN %in% c(-555)),]$ever_told_HTN <- 0
steps$HTN_awareness <- ifelse((is.na(steps$HTN) | steps$HTN == 0),
                              NA,
                              ifelse(steps$ever_told_HTN == 1,1,0))

steps$HTN_treatment <- ifelse((is.na(steps$HTN_awareness) | steps$HTN_awareness == 0),
                              NA,
                              ifelse(steps$taking_htn_drug == 1,1,0))

steps$HTN_control_14090 <- ifelse((is.na(steps$HTN_treatment) | steps$HTN_treatment == 0),
                                  NA,
                                  ifelse(steps$MeanSys < 140 & steps$MeanDias < 90,1,0))

steps$HTN_control_12080 <- ifelse((is.na(steps$HTN_treatment) | steps$HTN_treatment == 0),
                                  NA,
                                  ifelse(steps$MeanSys < 120 & steps$MeanDias < 80,1,0))

steps$HTN_cascade <- ifelse(is.na(steps$HTN), NA,
                            ifelse(steps$HTN == 0,0,
                                   ifelse(steps$HTN_awareness == 0,1,
                                          ifelse(steps$HTN_treatment == 0,2,
                                                 ifelse(steps$HTN_control_14090 == 0,3,4)))))

steps$HTN_cascade2 <- ifelse(is.na(steps$HTN), NA,
                             ifelse(steps$HTN == 0,0,
                                    ifelse(steps$HTN_awareness == 0,1,
                                           ifelse(steps$HTN_treatment == 0,1,
                                                  ifelse(steps$HTN_control_14090 == 0,3,4)))))

steps <- steps %>%
  mutate(bmi_cat_ag = case_when(
    bmi_cat == 1 ~ 1,
    bmi_cat == 2 ~ 0,
    bmi_cat == 3 ~ 2,
    bmi_cat == 4 ~ 3,
    TRUE ~ NA_real_
  ))

steps$diabetes <- steps$diabetes_FBS

steps$chl_control <- ifelse(steps$CH02l < 200,0,
                            ifelse(steps$CH02l >= 200 & steps$CH02l < 240,1,2))

steps$salt_high <- ifelse(steps$salt_24 <5, 0,1)
#steps$salt_high_median <- ifelse(steps$salt_24 <9.607217, 0,1)

categorical_variables_to_check <- c('age_category', 'age_category2','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                                    'WI_National','bmi_cat_ag', 'salt_shaker_lastmeal', 'salt_addition_cooking',
                                    'salt_addition_eating', 'salty_food', 'salt_amount_self','salt_health',
                                    'salt_reduce_importance', 'salt_recom','fruit_app', 'veg_app', 'fast_food', 'low_activity',
                                    'smoking_status', 'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 
                                    'HTN_control_14090', 'diabetes', 'chl_control', 'salt_high', 'HTN_cascade')

steps$age_category <- factor(steps$age_category)
steps$age_category2 <- factor(steps$age_category2)
steps$age_category3 <- factor(steps$age_category3)
steps$sex <- factor(steps$sex)
steps$area <- factor(steps$area)
steps$job <- factor(steps$job)
steps$insurance <- factor(steps$insurance)
#steps$basic <- factor(steps$basic)
#steps$comp <- factor(steps$comp)
steps$education <- factor(steps$education)
steps$marriagestatus <- factor(steps$marriagestatus)
steps$WI_National <- factor(steps$WI_National)
steps$bmi_cat_ag <- factor(steps$bmi_cat_ag) 
steps$salt_shaker_lastmeal <- factor(steps$salt_shaker_lastmeal)
steps$salt_addition_eating <- factor(steps$salt_addition_eating)
steps$salt_addition_cooking <- factor(steps$salt_addition_cooking)
steps$salty_food <- factor(steps$salty_food)
steps$salt_amount_self <- factor(steps$salt_amount_self)
steps$salt_health <- factor(steps$salt_health)
steps$salt_reduce_importance <- factor(steps$salt_reduce_importance)
steps$salt_recom <- factor(steps$salt_recom)
steps$fruit_app <- factor(steps$fruit_app)
steps$veg_app <- factor(steps$veg_app)
steps$fast_food <- factor(steps$fast_food)
steps$low_activity <- factor(steps$low_activity)
steps$smoking_current <- factor(steps$smoking_current)
steps$HTN <- factor(steps$HTN)
steps$HTN_awareness <- factor(steps$HTN_awareness)
steps$HTN_treatment <- factor(steps$HTN_treatment)
steps$HTN_control_14090 <- factor(steps$HTN_control_14090)
steps$HTN_control_12080 <- factor(steps$HTN_control_12080)
steps$diabetes <- factor(steps$diabetes)
steps$chl_control <- factor(steps$chl_control)
steps$salt_high <- factor(steps$salt_high)
steps$HTN_cascade <- factor(steps$HTN_cascade)
steps$HTN_cascade2 <- factor(steps$HTN_cascade2)

salt_attitude_df <- subset(steps, is.na(steps$salt_shaker_lastmeal) == FALSE)
salt_intake_df <- subset(steps, is.na(steps$salt_high) == FALSE)


for (var in categorical_variables_to_check) {
  if (is.factor(salt_attitude_df)) {
    if (any(table(salt_attitude_df[[var]]) == 0)) {
      salt_attitude_df[[var]] <- droplevels(salt_attitude_df[[var]]) 
    }
  }
}

for (var in categorical_variables_to_check) {
  if (is.factor(salt_intake_df)) {
    if (any(table(salt_intake_df[[var]]) == 0)) {
      salt_intake_df[[var]] <- droplevels(salt_intake_df[[var]]) 
    }
  }
}

label_mappings <- list(
  age_category = c("18-24", "25-34", "35-44", "45-54","55-64","65-70",'>= 70'),
  age_category2 =c("18-24", "25-39", "40-59", '>= 60'),
  age_category3 =c("25-39", "40-59", '>= 60'),
  sex = c( 'Female','Male'),
  area = c("Rural", "Urban"), 
  marriagestatus = c("Single", "Married", "Divorced/widow"),
  education = c("0","1-6", "7-11", ">12"),
  WI_National = c("1 (poorest)", "2", "3", "4", "5 (wealthiest)"),
  job = c('Unemployed', 'Employed', 'Unpaid work', 'Retired'),
  insurance = c("No insurance", "Basic", "Basic+Complementary"),
  basic = c('No', 'Yes'),
  comp =  c('No', 'Yes'),
  bmi_cat_ag = c('normal', 'underweigth', 'overweigth', 'obesity'),
  salt_shaker_lastmeal = c('No', 'Yes'),
  salt_addition_eating = c('Never', 'Rarely/Sometimes', 'Often/Always'),
  salt_addition_cooking = c('Never', 'Rarely/Sometimes', 'Often/Always'),
  salty_food = c('Never', 'Rarely/Sometimes', 'Often/Always'),
  salt_amount_self = c('Very little/Little','Average', 'Much/Very much'),
  salt_health = c('No', 'Yes'),
  salt_reduce_importance = c('Not important', 'Slightly important', 'Very important'),
  salt_recom = c('No', 'Yes'),
  fruit_app = c('No', 'Yes'),
  veg_app = c('No', 'Yes'),
  fast_food = c('No', 'Yes'),
  low_activity = c('No', 'Yes'),
  smoking_current = c('Never','Current'),
  HTN = c('No', 'Yes'),
  HTN_awareness = c('No', 'Yes'),
  HTN_treatment = c('No', 'Yes'),
  HTN_control_14090 = c('No', 'Yes'),
  HTN_control_12080 = c('No', 'Yes'),
  HTN_cascade = c('No', 'unaware', 'aware_untreat', 'treated_uncontrol', 'treated_control'),
  HTN_cascade2 = c('No', 'unaware', 'aware_untreat', 'treated_uncontrol', 'treated_control'),
  diabetes = c('No', 'Yes'),
  chl_control = c('Desirable', 'Borderline high', 'High'),
  salt_high = c('No', 'Yes')
)


label_df <- do.call(rbind, lapply(names(label_mappings), function(var) {
  data.frame(
    Variable = var,
    Category = label_mappings[[var]],
    Sublevel = seq_along(label_mappings[[var]]) - 1
  )
}))


############designs
salt_intake_design <-
  svydesign(
    weights = salt_intake_df$W_Laboratory,
    data = salt_intake_df,
    id = salt_intake_df$familymemberid
  )

salt_attitude_design_q <-
  svydesign(
    weights = salt_attitude_df$W_Questionnaire,
    data = salt_attitude_df,
    id = salt_attitude_df$familymemberid
  )

salt_attitude_design_a <-
  svydesign(
    weights = salt_attitude_df$W_Anthropometry,
    data = salt_attitude_df,
    id = salt_attitude_df$familymemberid
  )

salt_attitude_design_l <-
  svydesign(
    weights = salt_attitude_df$W_Laboratory,
    data = salt_attitude_df,
    id = salt_attitude_df$familymemberid
  )


##########
variable_list_table1 <- c('sex', 'age_category','age_category2','age_category3','area', 'education', 'marriagestatus', 'job', 'WI_National','insurance')

baseline_df_salt_attitude <- baseline_table_final_process_withoutCI(salt_attitude_df, i, salt_attitude_design_q, 2, variable_list_table1, 'questionnaire')
baseline_df_salt_intake <- baseline_table_final_process_withoutCI(salt_intake_df, i, salt_intake_design, 2, variable_list_table1, 'lab')

write.csv(baseline_df_salt_attitude, "....\\AG_result\\Baseline_table_q.csv")
write.csv(baseline_df_salt_intake, "....\\AG_result\\Baseline_table_l.csv")

descriptive_variables <- c( 'age_category2','age_category3','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                            'WI_National','bmi_cat_ag', 'salt_shaker_lastmeal', 'salt_addition_eating',
                            'salt_addition_cooking', 'salty_food', 'salt_amount_self','salt_health',
                            'salt_reduce_importance', 'fruit_app', 'veg_app', 'fast_food', 'low_activity',
                            'smoking_current', 'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 
                            'HTN_control_14090', 'diabetes', 'chl_control', 'HTN_cascade')

high_salt_descriptive_table <- process_descriptive_table(label_df, descriptive_variables, 'salt_high', salt_intake_df, 2, salt_intake_design)
mean_salt_descriptive_table <-process_descriptive_table_mean(label_df, descriptive_variables, 'salt_24', salt_intake_df, 2, salt_intake_design)
write.csv(high_salt_descriptive_table, "....\\AG_result\\high_salt_descriptive_table.csv")
write.csv(mean_salt_descriptive_table, "....\\AG_result\\mean_salt_descriptive_table.csv")

#
descriptive_variables_salt_q <- c( 'age_category2','age_category3','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                                   'WI_National', 'fruit_app', 'veg_app', 'fast_food', 'low_activity',
                                   'smoking_current')
descriptive_variables_salt_a <- c('bmi_cat_ag', 'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 
                                  'HTN_control_14090', 'HTN_cascade')
descriptive_variables_salt_l <- c('diabetes', 'chl_control')

#
salt_shaker_lastmeal_descriptive_table_q <- process_descriptive_table(label_df, descriptive_variables_salt_q, 'salt_shaker_lastmeal', salt_attitude_df, 2, salt_attitude_design_q)
salt_shaker_lastmeal_descriptive_table_a <- process_descriptive_table(label_df, descriptive_variables_salt_a, 'salt_shaker_lastmeal', salt_attitude_df, 2, salt_attitude_design_a)
salt_shaker_lastmeal_descriptive_table_l <- process_descriptive_table(label_df, descriptive_variables_salt_l, 'salt_shaker_lastmeal', salt_attitude_df, 2, salt_attitude_design_l)
salt_shaker_lastmeal_df <- rbind(salt_shaker_lastmeal_descriptive_table_q, salt_shaker_lastmeal_descriptive_table_a, salt_shaker_lastmeal_descriptive_table_l)
write.csv(salt_shaker_lastmeal_df, "....\\AG_result\\salt_shaker_lastmeal_df.csv")

ci_result <- svyciprop(~factor(salt_shaker_lastmeal) == 1, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salt_addition_eating_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_addition_eating', salt_attitude_df, 2, salt_attitude_design_q)
salt_addition_eating_descriptive_table_a <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_a, 'salt_addition_eating', salt_attitude_df, 2, salt_attitude_design_a)
salt_addition_eating_descriptive_table_l <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_l, 'salt_addition_eating', salt_attitude_df, 2, salt_attitude_design_l)
salt_addition_eating_df <- rbind(salt_addition_eating_descriptive_table_q, salt_addition_eating_descriptive_table_a, salt_addition_eating_descriptive_table_l)
write.csv(salt_addition_eating_df, "....\\AG_result\\salt_addition_eating_df.csv")

ci_result <- svyciprop(~factor(salt_addition_eating) == 2, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salt_addition_cooking_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_addition_cooking', salt_attitude_df, 2, salt_attitude_design_q)
salt_addition_cooking_descriptive_table_a <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_a, 'salt_addition_cooking', salt_attitude_df, 2, salt_attitude_design_a)
salt_addition_cooking_descriptive_table_l <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_l, 'salt_addition_cooking', salt_attitude_df, 2, salt_attitude_design_l)
salt_addition_cooking_df <- rbind(salt_addition_cooking_descriptive_table_q, salt_addition_cooking_descriptive_table_a, salt_addition_cooking_descriptive_table_l)

write.csv(salt_addition_cooking_df, "....\\AG_result\\salt_addition_cooking_df.csv")

round(svychisq(~salt_addition_cooking+chl_control,  design = salt_attitude_design_l, statistic = "adjWald")$p.value, 4)
ci_result <- svyciprop(~factor(salt_addition_cooking) == 2, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salty_food_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salty_food', salt_attitude_df, 2, salt_attitude_design_q)
salty_food_descriptive_table_a <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_a, 'salty_food', salt_attitude_df, 2, salt_attitude_design_a)
salty_food_descriptive_table_l <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_l, 'salty_food', salt_attitude_df, 2, salt_attitude_design_l)
salty_food_df <- rbind(salty_food_descriptive_table_q, salty_food_descriptive_table_a, salty_food_descriptive_table_l)
write.csv(salty_food_df, "....\\AG_result\\salty_food_df.csv")

ci_result <- svyciprop(~factor(salty_food) == 2, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salt_amount_self_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_amount_self', salt_attitude_df, 2, salt_attitude_design_q)
salt_amount_self_descriptive_table_a <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_a, 'salt_amount_self', salt_attitude_df, 2, salt_attitude_design_a)
salt_amount_self_descriptive_table_l <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_l, 'salt_amount_self', salt_attitude_df, 2, salt_attitude_design_l)
salt_amount_self_df <- rbind(salt_amount_self_descriptive_table_q, salt_amount_self_descriptive_table_a, salt_amount_self_descriptive_table_l)
write.csv(salt_amount_self_df, "....\\AG_result\\salt_amount_self_df.csv")

ci_result <- svyciprop(~factor(salt_amount_self) == 2, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salt_health_descriptive_table_q <- process_descriptive_table(label_df, descriptive_variables_salt_q, 'salt_health', salt_attitude_df, 2, salt_attitude_design_q)
salt_health_descriptive_table_a <- process_descriptive_table(label_df, descriptive_variables_salt_a, 'salt_health', salt_attitude_df, 2, salt_attitude_design_a)
salt_health_descriptive_table_l <- process_descriptive_table(label_df, descriptive_variables_salt_l, 'salt_health', salt_attitude_df, 2, salt_attitude_design_l)
salt_health_df <- rbind(salt_health_descriptive_table_q, salt_health_descriptive_table_a, salt_health_descriptive_table_l)
write.csv(salt_health_df, "....\\AG_result\\salt_health_df.csv")

ci_result <- svyciprop(~factor(salt_health) == 1, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)

#
salt_reduce_importance_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_reduce_importance', salt_attitude_df, 2, salt_attitude_design_q)
salt_reduce_importance_descriptive_table_a <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_a, 'salt_reduce_importance', salt_attitude_df, 2, salt_attitude_design_a)
salt_reduce_importance_descriptive_table_l <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_l, 'salt_reduce_importance', salt_attitude_df, 2, salt_attitude_design_l)
salt_reduce_importance_df <- rbind(salt_reduce_importance_descriptive_table_q, salt_reduce_importance_descriptive_table_a, salt_reduce_importance_descriptive_table_l)
write.csv(salt_reduce_importance_df, "....\\AG_result\\salt_reduce_importance_df.csv")

write.csv(cbind(salt_shaker_lastmeal_df,salt_addition_eating_df,salt_addition_cooking_df, salty_food_df,
                salt_amount_self_df, salt_health_df, salt_reduce_importance_df), "....\\AG_result\\salt_behavior_df.csv")

ci_result <- svyciprop(~factor(salt_reduce_importance) == 2, salt_attitude_design_q,  method = "logit", na.rm = TRUE)
round(confint(ci_result) * 100 ,2)
round(ci_result[[1]] * 100,2)


#
Crude_model_high_salt <- crudeLR_table_maker(descriptive_variables, 'salt_high','salt_high', salt_intake_design, crude_LR, label_df)
#Crude_model_salt_24 <- crudeLM_table_maker(descriptive_variables, 'salt_24','salt_24', salt_intake_design, crude_lm, label_df) 
write.csv(Crude_model_high_salt, "....\\AG_result\\Crude_model_high_salt.csv")

age_sex_adj_base_formula <- "salt_high ~ factor(age_category3) + factor(sex)"
age_sex_adj_variables <- c("sex",'age_category3')
age_sex_variables <- c( 'age_category3','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                        'WI_National', 'fruit_app', 'veg_app', 'low_activity',
                        'smoking_current', 'bmi_cat_ag', 'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 
                        'HTN_control_14090', 'diabetes', 'chl_control', 'salt_shaker_lastmeal', 'salt_addition_eating',
                        'salt_addition_cooking', 'salty_food', 'salt_amount_self','salt_health',
                        'salt_reduce_importance')
adj_model1 <- adjLR_table_maker(age_sex_adj_base_formula, age_sex_adj_variables, age_sex_variables, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 
write.csv(adj_model1, "....\\AG_result\\age_sex_adj_model_high_salt.csv")


age_sex_adj_base_formula2 <- "salt_high ~ age + factor(sex)"
age_sex_adj_variables2 <- c("sex",'age')
age_sex_variables2 <- c( 'age','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                         'WI_National', 'fruit_app', 'veg_app', 'low_activity',
                         'smoking_current', 'bmi_cat_ag', 'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 
                         'HTN_control_14090', 'diabetes', 'chl_control', 'salt_shaker_lastmeal', 'salt_addition_eating',
                         'salt_addition_cooking', 'salty_food', 'salt_amount_self','salt_health',
                         'salt_reduce_importance')
adj_model2 <- adjLR_table_maker(age_sex_adj_base_formula2, age_sex_adj_variables2, age_sex_variables2, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 

base_formula <- "salt_high ~ factor(sex) + factor(bmi_cat_ag) + factor(veg_app) + factor(low_activity) + factor(HTN)"
adj_variables <- c("sex", "bmi_cat_ag", "veg_app", "low_activity", "HTN")
list_variables <- c( 'age_category3','sex', 'area', 'job', 'insurance', 'education', 'marriagestatus',
                     'WI_National', 'fruit_app', 'veg_app', 'low_activity',
                     'smoking_current', 'bmi_cat_ag', 'HTN', 'diabetes', 'chl_control', 'salt_shaker_lastmeal', 'salt_addition_eating',
                     'salt_addition_cooking', 'salty_food', 'salt_amount_self','salt_health',
                     'salt_reduce_importance')

adj_model <- adjLR_table_maker(base_formula, adj_variables, list_variables, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 
write.csv(adj_model, "....\\AG_result\\adj_model_high_salt.csv")


base_formula_HTN <- "salt_high ~ factor(sex) + factor(bmi_cat_ag) + factor(veg_app) + factor(low_activity)"
adj_variables_HTN <- c("sex", "bmi_cat_ag", "veg_app", "low_activity")
list_variables_HTN <- c(  'HTN', 'HTN_awareness', 'HTN_treatment', 'HTN_control_12080', 'HTN_control_14090')

adj_model_HTN <- adjLR_table_maker(base_formula_HTN, adj_variables_HTN, list_variables_HTN, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 
write.csv(adj_model_HTN, "....\\AG_result\\adj_model_HTNvariables_high_salt.csv")




# I added salt recom later:
descriptive_variables <- c('salt_recom')

high_salt_descriptive_table <- process_descriptive_table(label_df, descriptive_variables, 'salt_high', salt_intake_df, 2, salt_intake_design)
mean_salt_descriptive_table <-process_descriptive_table_mean(label_df, descriptive_variables, 'salt_24', salt_intake_df, 2, salt_intake_design)
write.csv(high_salt_descriptive_table, "....\\AG_result\\salt_recom_high_salt_descriptive_table.csv")
write.csv(mean_salt_descriptive_table, "....\\AG_result\\salt_recom_mean_salt_descriptive_table.csv")

#
descriptive_variables_salt_q <- c( 'salt_recom')

#
salt_shaker_lastmeal_descriptive_table_q <- process_descriptive_table(label_df, descriptive_variables_salt_q, 'salt_shaker_lastmeal', salt_attitude_df, 2, salt_attitude_design_q)
salt_addition_eating_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_addition_eating', salt_attitude_df, 2, salt_attitude_design_q)
salt_addition_cooking_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_addition_cooking', salt_attitude_df, 2, salt_attitude_design_q)
salty_food_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salty_food', salt_attitude_df, 2, salt_attitude_design_q)
salt_amount_self_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_amount_self', salt_attitude_df, 2, salt_attitude_design_q)
salt_health_descriptive_table_q <- process_descriptive_table(label_df, descriptive_variables_salt_q, 'salt_health', salt_attitude_df, 2, salt_attitude_design_q)
salt_reduce_importance_descriptive_table_q <- process_descriptive_table_plus2outcome(label_df, descriptive_variables_salt_q, 'salt_reduce_importance', salt_attitude_df, 2, salt_attitude_design_q)

write.csv(cbind(salt_shaker_lastmeal_descriptive_table_q,salt_addition_eating_descriptive_table_q,salt_addition_cooking_descriptive_table_q,
                salty_food_descriptive_table_q,
                salt_amount_self_descriptive_table_q, salt_health_descriptive_table_q, salt_reduce_importance_descriptive_table_q), "....\\AG_result\\salt_recom_salt_behavior_df.csv")

round(svychisq(~salty_food + salt_recom,  design = salt_attitude_design_q, statistic = "adjWald")$p.value, 4)

#
Crude_model_high_salt <- crudeLR_table_maker(descriptive_variables, 'salt_high','salt_high', salt_intake_design, crude_LR, label_df)

age_sex_adj_base_formula <- "salt_high ~ factor(age_category3) + factor(sex)"
age_sex_adj_variables <- c("sex",'age_category3')
adj_model1 <- adjLR_table_maker(age_sex_adj_base_formula, age_sex_adj_variables, descriptive_variables, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 

base_formula <- "salt_high ~ factor(sex) + factor(bmi_cat_ag) + factor(veg_app) + factor(low_activity) + factor(HTN)"
adj_variables <- c("sex", "bmi_cat_ag", "veg_app", "low_activity", "HTN")
adj_model <- adjLR_table_maker(base_formula, adj_variables, descriptive_variables, salt_intake_design,'salt_high', 'salt_high', adj_LR_t, label_df) 


write.csv(adj_model, "....\\AG_result\\adj_model_high_salt.csv")


