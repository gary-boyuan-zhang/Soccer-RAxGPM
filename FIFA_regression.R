

# load packages -----------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

eng2021 <- read_csv("data/eng2021_boxscores.csv")
eng2122 <- read_csv("data/eng2122_boxscores.csv")



# clean data --------------------------------------------------------------

eng2021_cleaned <- eng2021 %>%
  select(-("Start%": "MinPerMatch"))

# position grouping ----------------------------------------------------------
'''
GK - Goalkeepers

DF - Defenders
FB - Fullbacks
LB - Left Backs
RB - Right Backs
CB - Center Backs

DM - Defensive Midfielders
CM - Central Midfielders
MF - Midfielders

LM - Left Midfielders
RM - Right Midfielders
WM - Wide Midfielders
LW - Left Wingers
RW - Right Wingers
AM - Attacking Midfielders

FW - Forwards

table(eng2021$Pos)
"GK": 23
"DF", "DF,MF", "DF,FW" : 112 + 11 + 2 = 125
"MF,DF", "MF", "MF,FW": 9 + 72 + 22 = 103
"FW,DF", "FW", "FW,MF": 3 + 48 + 26 = 77
'''

DF <- eng2021_cleaned %>%
  filter(Pos %in% c("DF", "DF,MF", "DF,FW"))

MF <- eng2021_cleaned %>%
  filter(Pos %in% c("MF", "MF,DF", "MF,FW"))

FW <- eng2021_cleaned %>%
  filter(Pos %in% c("FW", "FW,MF", "FW,DF"))


# train model -------------------------------------------------------------

lrDF <- lm(overall ~ ., data = select(DF, ("overall" : "AttPenTch90")))
summary(lrDF)

lrMF <- lm(overall ~ ., data = select(MF, ("overall" : "AttPenTch90")))
summary(lrMF)

lrFW <- lm(overall ~ ., data = select(FW, ("overall" : "AttPenTch90")))
summary(lrFW)
#lrFW$coefficients

# ridge -------------------------------------------------------------------

library(glmnet)

lambdas <- 10^seq(2, -5, by = -1)
DF_cv_ridge <- cv.glmnet(as.matrix(select(DF, ("Min" : "AttPenTch90"))),
                         DF$overall, alpha = 0, lambda = lambdas)
DF_ridge <- glmnet(as.matrix(select(DF, ("Min" : "AttPenTch90"))),
                  DF$overall, alpha = 0, lambda = DF_cv_ridge$lambda.min)
DF_ridge$beta
DF_ridge$a0
DF_ridge$dev.ratio

MF_cv_ridge <- cv.glmnet(as.matrix(select(MF, ("Min" : "AttPenTch90"))),
                         MF$overall, alpha = 0, lambda = lambdas)
MF_ridge <- glmnet(as.matrix(select(MF, ("Min" : "AttPenTch90"))),
                   MF$overall, alpha = 0, lambda = MF_cv_ridge$lambda.min)
MF_ridge$beta
MF_ridge$a0
MF_ridge$dev.ratio

FW_cv_ridge <- cv.glmnet(as.matrix(select(FW, ("Min" : "AttPenTch90"))),
                         FW$overall, alpha = 0, lambda = lambdas)
FW_ridge <- glmnet(as.matrix(select(FW, ("Min" : "AttPenTch90"))),
                   FW$overall, alpha = 0, lambda = FW_cv_ridge$lambda.min)
FW_ridge$beta
FW_ridge$a0
FW_ridge$dev.ratio

# lasso -------------------------------------------------------------------

DF_cv_lasso <- cv.glmnet(as.matrix(select(DF, ("Min" : "AttPenTch90"))),
                         DF$overall, alpha = 1, lambda = lambdas)
DF_lasso <- glmnet(as.matrix(select(DF, ("Min" : "AttPenTch90"))),
                   DF$overall, alpha = 1, lambda = DF_cv_lasso$lambda.min)
DF_lasso$beta
DF_lasso$a0
DF_lasso$dev.ratio

MF_cv_lasso <- cv.glmnet(as.matrix(select(MF, ("Min" : "AttPenTch90"))),
                         MF$overall, alpha = 1, lambda = lambdas)
MF_lasso <- glmnet(as.matrix(select(MF, ("Min" : "AttPenTch90"))),
                   MF$overall, alpha = 1, lambda = MF_cv_lasso$lambda.min)
MF_lasso$beta
MF_lasso$a0
MF_lasso$dev.ratio

FW_cv_lasso <- cv.glmnet(as.matrix(select(FW, ("Min" : "AttPenTch90"))),
                         FW$overall, alpha = 1, lambda = lambdas)
FW_lasso <- glmnet(as.matrix(select(FW, ("Min" : "AttPenTch90"))),
                   FW$overall, alpha = 1, lambda = FW_cv_lasso$lambda.min)
FW_lasso$beta
FW_lasso$a0
FW_lasso$dev.ratio


# vip ---------------------------------------------------------------------

library(broom)

tidy(lrDF) %>%
  filter(term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip() +
  theme_bw()

tidy(DF_cv_ridge$glmnet.fit) %>%
  filter(lambda == DF_cv_ridge$lambda.1se & term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip() +
  theme_bw()

tidy(DF_cv_lasso$glmnet.fit) %>%
  filter(lambda == DF_cv_lasso$lambda.1se & term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  filter(abs(estimate) > 0.5) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = "none") +
  labs(title = "Feature Importance for Defender",
       x = "feature", 
       caption = "Training R^2: 62.9%") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 10))

# MF

tidy(MF_cv_lasso$glmnet.fit) %>%
  filter(lambda == MF_cv_lasso$lambda.1se & term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  filter(abs(estimate) > 0.5) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = "none") +
  labs(title = "Feature Importance for Midfielder",
       x = "feature", 
       caption = "Training R^2: 72.6%") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 10))

# FW

tidy(FW_cv_lasso$glmnet.fit) %>%
  filter(lambda == FW_cv_lasso$lambda.1se & term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  filter(abs(estimate) > 0.5) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = "none") +
  labs(title = "Feature Importance for Forward",
       x = "feature", 
       caption = "Training R^2: 76.5%") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
          axis.title = element_text(size = 10))

