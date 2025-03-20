pacman::p_load(
  dplyr,
  ggplot2,
  tidyr,
  psych
)
sessionInfo()

df = read.csv("data/raw/study_1a.csv")
glimpse(df)

df$recallcheck = (df$recallcheck-2) * (-1)
table(df$apology, df$recallcheck)

dplyr::mutate(df, accurate=(recallcheck==apology))$accurate

# prestige
prestige_df = 
  df |> 
  dplyr::select(starts_with("prestige")) |> 
  mutate(
    prestige2 = 8 - prestige2,
    prestige4 = 8 - prestige4,
    prestige9 = 8 - prestige9
  )
glimpse(prestige_df)
cormat = corr.test(prestige_df)
cormat
psych::alpha(prestige_df)
df$prestige = rowMeans(prestige_df)

# TRIM
re_trim_id = c(1, 2, 4, 5, 7, 9, 10, 11, 13, 15, 17, 18)
trim_id = c(3, 6, 8, 12, 14, 16)
re_trim_cols = paste0("trim", re_trim_id)
trim_cols = paste0("trim", trim_id)
re_trim_df = 
  df |> 
  dplyr::select(all_of(re_trim_cols))
re_trim_df = 8 - re_trim_df
glimpse(re_trim_df)
trim_df = df |> 
  dplyr::select(all_of(trim_cols)) |> 
  cbind(re_trim_df)
glimpse(trim_df)
psych::alpha(trim_df)
df$trim = rowMeans(trim_df)

# ANOVA
tapply(df$prestige, list(df$apology, df$gender), mean)
tapply(df$prestige, list(df$apology, df$gender), sd)
aov_res_1 = 
  aov(
    prestige~factor(scenario)*factor(apology)*gender,
    data=df)
summary(aov_res_1)

tapply(df$trim, list(df$apology, df$gender), mean)
tapply(df$trim, list(df$apology, df$gender), sd)
aov_res_2 = 
  aov(
    trim~factor(scenario)*factor(apology)*gender,
    data=df)
summary(aov_res_2)
