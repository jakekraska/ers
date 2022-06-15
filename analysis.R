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
  
  # will need to rename some countries to be consistent between csv
  # Hong Kong, Iran, Russian, Northern Ireland, Scotland, South Korea,
  # Taiwain, Trinidad and Tobago, United Arab Emirates
  # Venezuela, Wales
  
}

#### Add Continents ####

continents <- read_csv(file = "continents.csv")
data <- left_join(x = data, y = continents, by = "country")

#### Add Beugelsdijk & Welzel Scores ####

bw.scores <- read_csv(file = "beugelsdijk_welzel_cultural_dimensions.csv")
data <- left_join(x = data, y = bw.scores, by = "country")

#### Add GDP Data ####

#gdppc <- read_csv(file = "gdppc2020.csv", col_names = c("country","gdppc","gdppc_thous"))
#data <- left_join(x = data, y = gdppc, by = "country")

#### Remove Rows With Missing Data ####

data <- na.omit(data)

#### Create Factors ####

data$userid <- as.factor(data$userid)
data$gender <- factor(data$gender, levels=c(0,1), labels=c("Male","Female"))
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

#### Select Data ####

demographics <- select(data, userid, gender, age, country, continent, erstot)
ipip <- select(data, userid, paste(c(outer(c("o","c","e","a","n"),1:20,paste0))), o, c, e, a, n, erstot)
ers <- select(data, userid, erstot)
h.culture <- select(data, userid, country, pdi, idv, mas, unc, lto, ind, erstot)
bw.culture <- select(data, userid, country, col.ind, dut.joy, dis.tru, erstot)

ope <- select(data, userid, paste0("o",1:20))
con <- select(data, userid, paste0("c",1:20))
ext <- select(data, userid, paste0("e",1:20))
agr <- select(data, userid, paste0("a",1:20))
neu <- select(data, userid, paste0("n",1:20))

#### Demographics ####

sum(table(demographics$userid) > 1)
sum(table(demographics$userid) > 2)
median(table(demographics$userid))
mean(table(demographics$userid))

unique(demographics) %>%
  distinct(userid, .keep_all = TRUE) %>%
  summarise("n" = n(),
            "Age (Mean)" = mean(age),
            "Age (SD)" = sd(age),
            "Gender (% female)" = sum(gender == "Female")/n*100,
            "Gender (% male)" = sum(gender == "Male")/n*100)

length(unique(demographics$country))

ggplot(data, aes(x = gender, y = erstot)) +
  geom_boxplot()

ggplot(data, aes(x = age, y = erstot)) +
  geom_point() + 
  geom_smooth(method = "lm")

unique(demographics) %>%
  distinct(userid, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "Gender (% female)" = round(sum(gender == "Female")/n*100, digits = 2),
            "Gender (% male)" = round(sum(gender == "Male")/n*100, digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "countries.txt", sep = ",", quote = FALSE, row.names = FALSE)
  
unique(demographics) %>%
  distinct(userid, .keep_all = TRUE) %>%
  group_by(continent) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "Gender (% female)" = round(sum(gender == "Female")/n*100, digits = 2),
            "Gender (% male)" = round(sum(gender == "Male")/n*100, digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "continents.txt", sep = ",", quote = FALSE, row.names = FALSE)

unique(demographics) %>%
  distinct(userid, .keep_all = TRUE) %>%
  group_by(gender) %>%
  summarise("n" = n(),
            "Age (Mean)" = round(mean(age), digits = 2),
            "Age (SD)" = round(sd(age), digits = 2),
            "ERS (Mean)" = round(mean(erstot), digits = 2),
            "ERS (SD)" = round(sd(erstot), digits = 2)) %>%
  write.table(file = "genders.txt", sep = ",", quote = FALSE, row.names = FALSE)

#### Alpha ####

psych::alpha(ope[2:21])
psych::alpha(con[2:21])
psych::alpha(ext[2:21])
psych::alpha(agr[2:21])
psych::alpha(neu[2:21])

#### Descriptives ####

psych::describe(select(data, gender, age, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, col.ind, dut.joy, dis.tru, erstot)) %>%
  select(-vars,-n) %>%
  round(digits = 2) %>%
  write.table(file = "descriptives.txt", sep = ",", quote = FALSE, row.names = TRUE)

#### Correlations ####

correlate(select(data, age, o, c, e, a, n, pdi, idv, mas, unc, lto, ind, col.ind, dut.joy, dis.tru, erstot)) %>%
  shave() %>%
  mutate_if(is.numeric, round, 2) %>%
  write.table(file = "correlations.txt", sep = ",", quote = FALSE, row.names = TRUE)

#### models ##### 

# Given unique labels, the following lines are the same
# (1 | continent) + (1 | country) + (1 | userid)
# (1 | continent/country/userid)

model1 <- lmer(erstot ~ 1 + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model1)

model2 <- lmer(erstot ~ age + gender + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model2)

model3 <- lmer(erstot ~ pdi + idv + mas + unc + lto + ind + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model3)

model4 <- lmer(erstot ~ age + gender + pdi + idv + mas + unc + lto + ind + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model4)

model5 <- lmer(erstot ~ o + c + e + a + n + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model5)

model6 <- lmer(erstot ~ age + gender + o + c + e + a + n + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model6)

model7 <- lmer(erstot ~ col.ind + dut.joy + dis.tru + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model7)

model8 <- lmer(erstot ~ age + gender + col.ind + dut.joy + dis.tru + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model8)

model9 <- lmer(erstot ~ age + gender + pdi + idv + mas + unc + lto + ind + o + c + e + a + n + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model9)

model10 <- lmer(erstot ~ age + gender + col.ind + dut.joy + dis.tru + o + c + e + a + n + (1 | continent/country/userid), data = data, REML = FALSE)
summary(model10)

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

