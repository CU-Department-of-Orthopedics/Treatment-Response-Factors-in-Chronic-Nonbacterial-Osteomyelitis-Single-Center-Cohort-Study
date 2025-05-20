## Analysis 

source("Data Clean.R")

library(kableExtra)
## Regression (Total Monotherapy Days)

# Update: Remove 2nd Line TRT Pts 

dat_n2 <- subset(dat, subset = dat$group_nsaid_2line != "Second Line")

dat_n2 <- dat_n2[, c(2:10)]

lin_mod1 <- lm(log(total_monotherapy_days)  ~ . - extraosseous_inv, data = dat_n2)
lin_mod1_sum <- summary(lin_mod1)$coefficients

lin_mod1_sum <- round(as.data.frame(lin_mod1_sum), 3)

names(lin_mod1_sum) <- c("Coefficient", "Std. Error", "t-value", "p-value")

rownames(lin_mod1_sum) <- c(
  "(Intercept)",
  "Sex (Female = 1)",
  "Age at Onset",
  "Onset to Treatment Interval",
  "Presence of Family History",
  "Unifocal Disease Suspected",
  "Regions Affected",
  "Symmetric Bone Lesion"
)

lin_mod1_sum <- lin_mod1_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

library(MASS)

# Stepwise regression model
step.model <- stepAIC(lin_mod1, direction = "both", 
                      trace = FALSE)
summary(step.model)

step.model_sum <- summary(step.model)$coefficients

step.model_sum <- round(as.data.frame(step.model_sum), 4)

step.model_ci <- round(confint(step.model), digits = 5)
step.model_ci <- paste0("[", step.model_ci[, 1], ", ", step.model_ci[,2], "]")
names(step.model_sum) <- c("Coefficient", "Std. Error", "t-value", "p-value")

step.model_sum <- step.model_sum %>%
  mutate(`95% CI` = step.model_ci) %>% 
  arrange(`p-value`) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`),
    )
  )


rownames(step.model_sum) <- c(
  "(Intercept)",
  "Unifocal Disease at Diagnosis",
  "Number of Regions Affected",
  "Onset to Treatment Interval"
)

step.model_sum <- step.model_sum %>% kable() %>% kable_classic(html_font = "cambria", full_width = F)

## Logistic Regression

table(dat$Line2)

logis_mod1 <- glm(Line2 ~ . - pid - extraosseous_inv - group_nsaid_2line - nsaids_trialed - total_monotherapy_days, data = dat, family = binomial(link = "logit"))
summary(logis_mod1)

logis_mod1_sum <- summary(logis_mod1)$coefficients

logis_mod1_sum <- round(as.data.frame(logis_mod1_sum), 3)

names(logis_mod1_sum) <- c("Estimate", "Std. Error", "t-value", "p-value")

rownames(logis_mod1_sum) <- c(
  "(Intercept)",
  "Sex (Female = 1)",
  "Age at Onset",
  "Onset to Treatment Interval",
  "Presence of Family History",
  "Unifocal Disease Suspected",
  "Regions Affected",
  "Symmetric Bone Lesion"
)

logis_mod1_sum <- logis_mod1_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )


step.log.mod <- stepAIC(logis_mod1, direction = "both", 
                         trace = FALSE)

cbind(summary(step.log.mod)$coefficients, confint(step.log.mod))

step.log.mod_sum <- summary(step.log.mod)$coefficients
step.log.mod_sum <- round(as.data.frame(step.log.mod_sum), 4)

step.log.model_ci <- round(confint(step.log.mod), digits = 5)
step.log.model_ci <- paste0("[", step.log.model_ci[, 1], ", ", step.log.model_ci[,2], "]")
names(step.log.mod_sum) <- c("Coefficient", "Std. Error", "t-value", "p-value")

rownames(step.log.mod_sum) <- c(
  "(Intercept)",
  "Onset to Treatment Interval",
  "Positive Family History",
  "Number of Regions Affected",
  "Symmetric Bone Lesions"
)

step.log.mod_sum <- step.log.mod_sum %>% 
  mutate(`95% CI` = step.log.model_ci) %>% 
  arrange(`p-value`) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`),
    )
  )


step.log.mod_sum <- step.log.mod_sum %>% kable() %>% kable_classic(html_font = "cambria", full_width = F)



## Multinomial Logistic Regression 

library(VGAM)

sum.o = function(fit){round(summary(fit)@coef3, digits=5)}

mult_mod1 <- vglm(
  formula = group_nsaid_2line ~ .  - pid - extraosseous_inv - total_monotherapy_days - nsaids_trialed - Line2, 
  family = multinomial(refLevel = 1),
  data = dat
  )

mult_mod1_sum <- round(as.data.frame(sum.o(mult_mod1)), 3)

names(mult_mod1_sum) <- c("Estimate", "Std. Error", "z-value", "p-value")

rownames(mult_mod1_sum) <- c(
  "(Intercept): MT1 vs MT2",
  "(Intercept): MT1 vs 2nd Line",
  "Sex (Female = 1): MT1 vs MT2",
  "Sex (Female = 1): MT1 vs 2nd Line",
  "Age of Onset: MT1 vs MT2",
  "Age of Onset: MT1 vs 2nd Line",
  "Onset to Treatment Interval: MT1 vs MT2",
  "Onset to Treatment Interval: MT1 vs 2nd Line",
  "Presence of Family History: MT1 vs MT2",
  "Presence of Family History: MT1 vs 2nd Line",
  "Unifocal Disease Suspected: MT1 vs MT2",
  "Unifocal Disease Suspected: MT1 vs 2nd Line",
  "Regions Affected: MT1 vs MT2",
  "Regions Affected: MT1 vs 2nd Line",
  "Symmetric Bone Lesion: MT1 vs MT2",
  "Symmetric Bone Lesion: MT1 vs 2nd Line"
)

mult_mod1_sum <- mult_mod1_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

mult_mod2 <- step4vglm(
  object = mult_mod1,
  direction = "both",
  k = 2,
  trace = 0
  )

mult_mod2_sum <- round(as.data.frame(sum.o(mult_mod2)), 3)

names(mult_mod2_sum) <- c("Estimate", "Std. Error", "z-value", "p-value")

rownames(mult_mod2_sum) <- c(
  "(Intercept): MT1 vs MT2",
  "(Intercept): MT1 vs 2nd Line",
  "Onset to Treatment Interval: MT1 vs MT2",
  "Onset to Treatment Interval: MT1 vs 2nd Line",
  "Presence of Family History: MT1 vs MT2",
  "Presence of Family History: MT1 vs 2nd Line",
  "Unifocal Disease Suspected: MT1 vs MT2",
  "Unifocal Disease Suspected: MT1 vs 2nd Line",
  "Regions Affected: MT1 vs MT2",
  "Regions Affected: MT1 vs 2nd Line",
  "Symmetric Bone Lesion: MT1 vs MT2",
  "Symmetric Bone Lesion: MT1 vs 2nd Line"
)

mult_mod2_sum <- mult_mod2_sum %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  )

anova(mult_mod1, mult_mod2, type = "I")

# 
# ## Summary Tables 
# 
# library(table1)
# 
# render.cont <- function(x) {
#   with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
#        c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
# }
# 
# render.cat <- function(x) {
#   c("", 
#     sapply(stats.default(x), 
#            function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
# }
# 
# 
# pvalue <- function(x, ...) {
#   y <- unlist(x)
#   g <- factor(rep(1:length(x), times = sapply(x, length)))
#   if (is.numeric(y)) {
#     p <- t.test(y ~ g)$p.value
#   } else {
#     p <- chisq.test(table(y, g))$p.value
#   }
#   c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
# }
# 
# pvalueANOVA <- function(x, ...) {
#   # Construct vectors of data y, and groups (strata) g
#   y <- unlist(x)
#   g <- factor(rep(1:length(x), times=sapply(x, length)))
#   
#   if (is.numeric(y)) {
#     # For numeric variables, perform a standard 2-sample t-test
#     ano <- aov(y ~ g)
#     p <- summary(ano)[[1]][[5]][1]
#     
#   } else {
#     # For categorical variables, perform a chi-squared test of independence
#     p <- chisq.test(table(y, g))$p.value
#   }
#   # Format the p-value, using an HTML entity for the less-than sign.
#   # The initial empty string places the output on the line below the variable label.
#   c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
# }
# 
# dat_sum <- dat
# 
# label(dat_sum$age_at_onset_of_sx) <- "Age at Onset"
# label(dat_sum$onset_treat_interval) <- "Onset to Treatment Interval (days)"
# 
# dat_sum$fam_hist_pres <- factor(dat_sum$fam_hist_pres, labels = c("No", "Yes")) 
# label(dat_sum$fam_hist_pres) <- "Family History?"
# 
# dat_sum$extraosseous_inv <- factor(dat_sum$extraosseous_inv, labels = c("No", "Yes")) 
# label(dat_sum$extraosseous_inv) <- "Extraosseuos Involvement?"
# 
# label(dat_sum$esr_at_pres) <- "ESR at Presentation"
# label(dat_sum$crp_at_pres) <- "CRP at Presentation"
# 
# dat_sum$unifocal_disease_suspected <- factor(dat_sum$unifocal_disease_suspected, labels = c("No", "Yes")) 
# label(dat_sum$unifocal_disease_suspected) <- "Unifocal Disease Suspected?"
# 
# label(dat_sum$regions_affected) <- "No. Regions Affected"
# 
# dat_sum$symmetry_bone <- factor(dat_sum$symmetry_bone, labels = c("No", "Yes")) 
# label(dat_sum$symmetry_bone) <- "Symmetric Bone Lesion?"
# 
# label(dat_sum$total_monotherapy_days) <- "Total Monotherapy Days"
# 
# dat_sum$nsaids_trialed <- as.factor(dat_sum$nsaids_trialed) 
# label(dat_sum$nsaids_trialed) <- "No. NSAIDs Trialed"
# 
# tab1 <- table1(
#   ~ . - pid - sex - Line2 | group_nsaid_2line ,
#   data = dat_sum,
#   overall = F,
#   extra.col=list(`P-value`= pvalueANOVA),
#   render.continuous = render.cont,
#   render.categorical = render.cat,
#   render.missing = NULL,
#   rowlabelhead = "Treatment Group"
# )
# 
# 
# 
