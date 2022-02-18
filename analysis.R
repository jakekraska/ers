### load libraries ###

library(haven)
library(tidyverse)
library(lme4)

### load data ###

data <- read_sav("data.sav")
names(data) <- tolower(names(data))
data <- select(data, userid, gender, age, country, paste(c(outer(c("o","c","e","a","n"),1:20,paste0))), o, c, e, a, n, pdi, idv, mas, unc, lto, ind, erstot)
data <- na.omit(data)
data <- unique(data)
data <- data[data$country != "", ]

data$userid <- as.factor(data$userid)
data$gender <- factor(data$gender, levels=c(0,1), labels=c("Male","Female"))
data$country <- as.factor(data$country)

demographics <- select(data, userid, gender, age, country)
ipip <- select(data, userid, paste(c(outer(c("o","c","e","a","n"),1:20,paste0))), o, c, e, a, n)
ers <- select(data, userid, ers_tot1, ers_tot2, erstot)
culture <- select(data, userid, country, pdi, idv, mas, unc, lto, ind)

ope <- select(data, userid, paste0("o",1:20))
con <- select(data, userid, paste0("c",1:20))
ext <- select(data, userid, paste0("e",1:20))
agr <- select(data, userid, paste0("a",1:20))
neu <- select(data, userid, paste0("n",1:20))

### demographic descriptive statistics ###

sum(table(demographics$userid) > 1)
sum(table(demographics$userid) > 2)
median(table(demographics$userid))
mean(table(demographics$userid))

unique(demographics) %>%
  summarise("n" = n(),
            "Age (Mean)" = mean(age),
            "Age (SD)" = sd(age),
            "Gender (% female)" = sum(gender == "Female")/n,
            "Gender (% male)" = sum(gender == "Male")/n)

length(unique(demographics$country))

ggplot(data, aes(x = gender, y = erstot)) +
  geom_boxplot()

ggplot(data, aes(x = age, y = erstot)) +
  geom_point() + 
  geom_smooth(method = "lm")

unique(demographics) %>%
  group_by(country) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "Gender (% female)" = round(sum(gender == "Female")/n, digits = 2),
            "Gender (% male)" = round(sum(gender == "Male")/n, digits = 2)) %>%
  write.table(file = "countries.txt", sep = ",", quote = FALSE, row.names = FALSE)
  

### alpha ###

psych::alpha(ope[2:21])
psych::alpha(con[2:21])
psych::alpha(ext[2:21])
psych::alpha(agr[2:21])
psych::alpha(neu[2:21])

### correlations and descriptives ###

cor(select(data, gender, age, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, erstot)) %>%
  round(digits = 2) %>%
  write.table(file = "correlations.txt", sep = ",", quote = FALSE, row.names = TRUE)

psych::describe(select(data, gender, age, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, erstot)) %>%
  select(-vars,-n) %>%
  round(digits = 2) %>%
  write.table(file = "descriptives.txt", sep = ",", quote = FALSE, row.names = TRUE)

### models ####

model1 <- lmer(erstot ~ 1 + (1 | userid) + (1 | country), data = data, REML = FALSE)
summary(model1)

model2 <- lmer(erstot ~ age + gender + (1 | userid) + (1 | country), data = data, REML = FALSE)
summary(model2)

model3 <- lmer(erstot ~ age + gender + pdi + idv + mas + unc + lto + ind + (1 | userid) + (1 | country), data = data, REML = FALSE)
summary(model3)

model4 <- lmer(erstot ~ age + gender + o + c + e + a + n + (1 | userid) + (1 | country), data = data, REML = FALSE)
summary(model4)

model5 <- lmer(erstot ~ age + gender + pdi + idv + mas + unc + lto + ind + o + c + e + a + n + (1 | userid) + (1 | country), data = data, REML = FALSE)
summary(model5)

models <- list("model1" = model1, "model2" = model2, "model3" = model3, "model4" = model4, "model5" = model5)

### residuals ###

lapply(names(models), function(x) {
  plot(models[[x]], resid(.) ~ fitted(.), main = x)
})

### normality of residuals ###

lapply(names(models), function(x) {
  hist(resid(models[[x]]), main = x)
})

lapply(names(models), function(x) {
  qqnorm(resid(models[[x]]), main = x)
})

### model comparison ###

anova(model1,model2) # compare intercept only model with age and gender model
anova(model2,model3) # compare age and gender model with cultural model
anova(model2,model4) # compare age and gender with personality model
anova(model3,model5) # compare cultural model with personality and cultural model
anova(model4,model5) # compare personality model with personality and cultural model

