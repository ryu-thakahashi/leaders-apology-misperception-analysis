rm(list=ls())
Sys.setenv(LANG="en")

pacman::p_load(
  dplyr,
  ggplot2,
  tidyr,
  psych
)
sessionInfo()

df = read.csv("data/raw/study_2.csv")
glimpse(df)

prestige_df = 
  df |> 
  mutate(
    prestige1 = ifelse(perspective == "leader", sup_ques_1_1, sub_ques_1_1),
    prestige2 = ifelse(perspective == "leader", sup_ques_1_2, sub_ques_1_2),
    prestige3 = ifelse(perspective == "leader", sup_ques_1_3, sub_ques_1_3),
    prestige4 = ifelse(perspective == "leader", sup_ques_1_4, sub_ques_1_4),
    prestige5 = ifelse(perspective == "leader", sup_ques_1_5, sub_ques_1_5),
    prestige6 = ifelse(perspective == "leader", sup_ques_1_6, sub_ques_1_6),
    prestige7 = ifelse(perspective == "leader", sup_ques_1_7, sub_ques_1_7),
    prestige8 = ifelse(perspective == "leader", sup_ques_1_8, sub_ques_1_8),
    prestige9 = ifelse(perspective == "leader", sup_ques_1_9, sub_ques_1_9),
  ) |> 
  mutate(
    prestige2 = 8 - prestige2,
    prestige4 = 8 - prestige4,
    prestige9 = 8 - prestige9
  ) |> 
  dplyr::select(starts_with("prestige"))
glimpse(prestige_df)
cormat = corr.test(prestige_df)
cormat
psych::alpha(prestige_df)
df$prestige = rowMeans(prestige_df)

# TRIM
raw_trim_df = 
  df |> 
  mutate(
    trim1 = ifelse(perspective == "leader", sup_ques_2_1, sub_ques_2_1),
    trim2 = ifelse(perspective == "leader", sup_ques_2_2, sub_ques_2_2),
    trim3 = ifelse(perspective == "leader", sup_ques_2_3, sub_ques_2_3),
    trim4 = ifelse(perspective == "leader", sup_ques_2_4, sub_ques_2_4),
    trim5 = ifelse(perspective == "leader", sup_ques_2_5, sub_ques_2_5),
    trim6 = ifelse(perspective == "leader", sup_ques_2_6, sub_ques_2_6),
    trim7 = ifelse(perspective == "leader", sup_ques_2_7, sub_ques_2_7),
    trim8 = ifelse(perspective == "leader", sup_ques_2_8, sub_ques_2_8),
    trim9 = ifelse(perspective == "leader", sup_ques_2_9, sub_ques_2_9),
    trim10 = ifelse(perspective == "leader", sup_ques_2_10, sub_ques_2_10),
    trim11 = ifelse(perspective == "leader", sup_ques_2_11, sub_ques_2_11),
    trim12 = ifelse(perspective == "leader", sup_ques_2_12, sub_ques_2_12),
    trim13 = ifelse(perspective == "leader", sup_ques_2_13, sub_ques_2_13),
    trim14 = ifelse(perspective == "leader", sup_ques_2_14, sub_ques_2_14),
    trim15 = ifelse(perspective == "leader", sup_ques_2_15, sub_ques_2_15),
    trim16 = ifelse(perspective == "leader", sup_ques_2_16, sub_ques_2_16),
    trim17 = ifelse(perspective == "leader", sup_ques_2_17, sub_ques_2_17),
    trim18 = ifelse(perspective == "leader", sup_ques_2_18, sub_ques_2_18)
  ) |> 
  dplyr::select(starts_with("trim"))
glimpse(raw_trim_df)

re_trim_id = c(1, 2, 4, 5, 7, 9, 10, 11, 13, 15, 17, 18)
trim_id = c(3, 6, 8, 12, 14, 16)
re_trim_cols = paste0("trim", re_trim_id)
trim_cols = paste0("trim", trim_id)
re_trim_df = 
  raw_trim_df |> 
  dplyr::select(all_of(re_trim_cols))
re_trim_df = 8 - re_trim_df
glimpse(re_trim_df)
trim_df = raw_trim_df |> 
  dplyr::select(all_of(trim_cols)) |> 
  cbind(re_trim_df)
glimpse(trim_df)
psych::alpha(trim_df)
df$trim = rowMeans(trim_df)

glimpse(df)

# ANOVA
tapply(df$prestige, list(df$apology, df$sex), mean)
tapply(df$prestige, list(df$apology, df$sex), sd)
aov_res_1 = 
  aov(
    prestige~factor(perspective)*factor(apology)*factor(scneario)*factor(sex),
    data=df)
summary(aov_res_1)

tapply(df$trim, list(df$apology, df$sex), mean)
tapply(df$trim, list(df$apology, df$sex), sd)
aov_res_2 = 
  aov(
    trim~factor(perspective)*factor(apology)*factor(scneario)*factor(sex),
    data=df)
summary(aov_res_2)
