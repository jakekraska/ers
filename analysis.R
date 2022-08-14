#### Load Libraries ####

library(haven)
library(tidyverse)
library(lme4)
library(psych)
library(corrr)

#### Load Data #### Cleaned data available on github 

if (file.exists("data_cleaned.csv")) {
  data <- read_csv("data_cleaned.csv")
} else {
  data <- read_sav("data.sav")
  names(data) <- tolower(names(data))
  data <- select(data, userid, gender, age, country, paste(c(outer(c("o","c","e","a","n"),1:20,paste0))), o, c, e, a, n, pdi, idv, mas, unc, lto, ind, erstot)
  data <- na.omit(data)
  data <- unique(data)
  data <- data[data$country != "", ]
  
  write.csv(data, "data_cleaned.csv", row.names = FALSE)
  
  # will need to rename some countries manually to be consistent between csv
  # Hong Kong, Iran, Russian, Northern Ireland, Scotland, South Korea,
  # Taiwain, Trinidad and Tobago, United Arab Emirates
  # Venezuela, Wales
  
  # Note that OCEAN have been keyed, summed, and divided by 20
  
}

#### Add Continents ####

continents <- read_csv(file = "continents.csv")
data <- left_join(x = data, y = continents, by = "country")

#### Add Beugelsdijk & Welzel Scores ####

bw.scores <- read_csv(file = "beugelsdijk_welzel_cultural_dimensions.csv")
data <- left_join(x = data, y = bw.scores, by = "country")

#### Add GDP Data ####

gdppc <- read_csv(file = "gdppc2020.csv")
data <- left_join(x = data, y = gdppc, by = "country")

#### Remove Rows With Missing Data ####

data <- na.omit(data)

#### Remove Duplicate Attempts ####

data <- data %>% group_by(userid) %>% slice(1) %>% ungroup()

#### Revert OCEAN Scale ####

data$o <- data$o * 20
data$c <- data$c * 20
data$e <- data$e * 20
data$a <- data$a * 20
data$n <- data$n * 20

#### Create Factors ####

data$userid <- as.factor(data$userid)
data$gender <- factor(data$gender, levels=c(0,1), labels=c("Male","Female"))
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

#### Demographics ####

data %>%
  summarise("n" = n(),
            "Age (Mean)" = mean(age),
            "Age (SD)" = sd(age),
            "Gender (% female)" = sum(gender == "Female")/n*100,
            "Gender (% male)" = sum(gender == "Male")/n*100)

length(unique(data$country))

ggplot(data, aes(x = gender, y = erstot)) +
  geom_boxplot()

ggplot(data, aes(x = age, y = erstot)) +
  geom_point() + 
  geom_smooth(method = "lm")

data %>%
  group_by(country) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "Gender (% female)" = round(sum(gender == "Female")/n*100, digits = 2),
            "Gender (% male)" = round(sum(gender == "Male")/n*100, digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "countries.txt", sep = ",", quote = FALSE, row.names = FALSE)
  
data %>%
  group_by(continent) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "Gender (% female)" = round(sum(gender == "Female")/n*100, digits = 2),
            "Gender (% male)" = round(sum(gender == "Male")/n*100, digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "continents.txt", sep = ",", quote = FALSE, row.names = FALSE)

data %>%
  group_by(gender) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "genders.txt", sep = ",", quote = FALSE, row.names = FALSE)

#### GDP and ERS ####

ggplot(data, aes(x = gdppc, y = erstot)) +
  geom_point() + 
  geom_smooth(method = "lm")

#### GDP and FFM ####

for (i in c("o","c","e","a","n")) {
  g <- ggplot(data, aes_string(x = "gdppc", y = i)) +
    geom_point() + 
    geom_smooth(method = "lm")
  print(g)
}

#### GDP and Cultural Values ####

for (i in c("pdi","idv","mas","unc","lto","ind")) {
  g <- ggplot(data, aes_string(x = "gdppc", y = i)) +
    geom_point() + 
    geom_smooth(method = "lm")
  print(g)
}

#### FFM and Cultural Values ####

for (x in c("o","c","e","a","n")) {
  for (y in c("pdi","idv","mas","unc","lto","ind")) {
    g <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point() + 
      geom_smooth(method = "lm")
    print(g)
  }
}

#### Alpha ####

psych::alpha(select(data, paste0("o",1:20)))
psych::alpha(select(data, paste0("c",1:20)))
psych::alpha(select(data, paste0("e",1:20)))
psych::alpha(select(data, paste0("a",1:20)))
psych::alpha(select(data, paste0("n",1:20)))

#### Descriptives ####

data %>%
  mutate(gender = as.integer(gender)) %>%
  select(age, gender, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, col.ind, dut.joy, dis.tru, erstot) %>%
  psych::describe() %>%
  select(-vars,-n) %>%
  round(digits = 2) %>%
  write.table(file = "descriptives.txt", sep = ",", quote = FALSE, row.names = TRUE)

#### Correlations ####

data %>%
  mutate(gender = as.integer(gender)) %>%
  select(gdppc_thous, age, gender, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, col.ind, dut.joy, dis.tru, erstot) %>%
  correlate() %>%
  shave() %>%
  mutate_if(is.numeric, round, 2) %>%
  write.table(file = "correlations.txt", sep = ",", quote = FALSE, row.names = TRUE)

#### effect sizes  function ####

# https://rdrr.io/cran/lme4/man/fixef.html
# https://stackoverflow.com/questions/8526681/extract-random-effect-variances-from-lme4-mer-model-object
# https://stats.stackexchange.com/questions/257985/how-can-i-derive-effect-sizes-in-lme4-and-describe-the-magnitude-of-fixed-effect

effect.size <- function(model) {
  
  # random effects
  sum.of.ran.eff.var <- sum(as.data.frame(VarCorr(model))$vcov)
  denominator <- sqrt(sum.of.ran.eff.var)
  
  # fixed effects
  n.fixed.effect <- length(fixef(model))
  
  # effect size
  print("@@@@@@@@@@@@@@@@@@@@@@@@@@@")
  for (i in 1:n.fixed.effect) {
    print("#################################")
    print("Effect size for:")
    print(fixef(model)[i]/denominator)
  }
}

#### models ##### 

# Given unique labels, the following lines are the same
# (1 | continent) + (1 | country)
# (1 | continent/country)

model1 <- lmer(erstot ~ gdppc_thous + (1 | continent/country), data = data, REML = FALSE)
summary(model1)
effect.size(model1)

model2 <- lmer(erstot ~ gdppc_thous + age + gender + (1 | continent/country), data = data, REML = FALSE)
summary(model2)
effect.size(model2)

model3 <- lmer(erstot ~ gdppc_thous + pdi + idv + mas + unc + lto + ind + (1 | continent/country), data = data, REML = FALSE)
summary(model3)
effect.size(model3)

model4 <- lmer(erstot ~ gdppc_thous + age + gender + pdi + idv + mas + unc + lto + ind + (1 | continent/country), data = data, REML = FALSE)
summary(model4)
effect.size(model4)

model5 <- lmer(erstot ~ gdppc_thous + o + c + e + a + n + (1 | continent/country), data = data, REML = FALSE)
summary(model5)
effect.size(model5)

model6 <- lmer(erstot ~ gdppc_thous + age + gender + o + c + e + a + n + (1 | continent/country), data = data, REML = FALSE)
summary(model6)
effect.size(model6)

model7 <- lmer(erstot ~ gdppc_thous + col.ind + dut.joy + dis.tru + (1 | continent/country), data = data, REML = FALSE)
summary(model7)
effect.size(model7)

model8 <- lmer(erstot ~ gdppc_thous + age + gender + col.ind + dut.joy + dis.tru + (1 | continent/country), data = data, REML = FALSE)
summary(model8)
effect.size(model8)

model9 <- lmer(erstot ~ gdppc_thous + age + gender + pdi + idv + mas + unc + lto + ind + o + c + e + a + n + (1 | continent/country), data = data, REML = FALSE)
summary(model9)
effect.size(model9)

model10 <- lmer(erstot ~ gdppc_thous+ age + gender + col.ind + dut.joy + dis.tru + o + c + e + a + n + (1 | continent/country), data = data, REML = FALSE)
summary(model10)
effect.size(model10)

models <- list("model1" = model1, "model2" = model2, "model3" = model3, "model4" = model4, "model5" = model5, "model6" = model6, "model7" = model7, "model8" = model8, "model9" = model9, "model10" = model10)

#### residuals ####

lapply(names(models), function(x) {
  plot(models[[x]], resid(.) ~ fitted(.), main = x)
})

#### normality of residuals ####

lapply(names(models), function(x) {
  hist(resid(models[[x]]), main = x)
})

lapply(names(models), function(x) {
  qqnorm(resid(models[[x]]), main = x)
})

#### model comparison ####

# H1: Age and gender will be associated with increased ERS.

anova(model1,model2) # compare intercept only model with age and gender model

# H2: Cultural dimensions will improve model fit significantly more than  gender and age.

anova(model2,model3) # compare age and gender model with hofstede model
anova(model2,model4) # compare age and gender model with combined age gender and hofstede model

# H3: The models including the Beugelsdijk and Welzel (2018) cultural dimensions will provide a better fit than the models including Hofstede (2010) cultural dimensions

anova(model3,model7) # Hofstede versus BW 
anova(model4,model8) # Hofstede versus BW with age
anova(model9,model10) # Hofstede versus BW with age, gender and ffm

# H4: Openness, agreeableness and extraversion will be positively associated with ERS.

# Explained by fixed effects

# H5: The FFM personality traits will improve model fit for ERS prediction greater than gender and age.

anova(model2,model5) # compare age and gender with personality model
anova(model2,model6) # compare age and gender to personality with age and gender

# hypothesis 6

anova(model4,model9) # compare hofstede with hofstede and ffm
anova(model8,model10) # compare bw with bw and ffm

