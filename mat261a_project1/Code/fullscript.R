library(dplyr)

dementia = read.csv("dementia.csv")

df_onset <- dementia %>%
  filter(dementia_new == 1) %>%      # only the row where dementia starts
  group_by(id) %>%
  filter(date == min(date)) %>%      # keep first onset
  ungroup()


lm_fit = lm(time_dementia ~ age, data=df_onset)

hist(dementia$age, xlab="Age of Participants at Baseline before Dementia (years)", main="Histogram of Dementia Participant Baseline Age")
hist(dementia$time_dementia, xlab="Numbers of Days Until Dementia Onset", main="Historgram of Time Until Dementia Onset in Days")

plot(df_onset$age, df_onset$time_dementia, col="blue", main="Biological Age vs. Time to Dementia Onset", ylab="Time To Dementia in Days", xlab="Biological Age at Baseline")

plot(lm_fit$fitted.values, lm_fit$residuals,
     xlab="Fitted values", ylab="Residuals",
     main="Residuals vs. Fitted")
abline(h=0, col="red")

qqnorm(lm_fit$residuals)
qqline(lm_fit$residuals, col="red")