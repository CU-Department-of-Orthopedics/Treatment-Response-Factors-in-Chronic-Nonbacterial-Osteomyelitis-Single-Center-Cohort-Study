## Summary Stats 

source("Data Clean.R")

dat2 <- ...

dat2 <- dat2[,colSums(is.na(dat2))<nrow(dat2)]

exclude_list <- c(37, 45, 83, 145, 12, 44, 53, 72, 128, 167,5,8,12,13,18,19,29,36,37,39,40,42,43,44,45,50,53,57,60,62,63,66,70,72,73,83,84,94,95,96,98,100,112,113,114,115,116,118,128,130,133,136,145,157,158,159,160,167,169,170,172,175,180,184,186,198,201,202,216,218,224,227,234,241,242,243,246,247)
include_list <- dat$pid

dat2 <- dat2 %>% 
  filter(
    !`Patient #` %in% exclude_list
  ) %>% 
  filter(
    `Patient #` %in% include_list
  )

names(dat2)

dat_full <- merge(x = dat, y = dat2, by.x = "pid", by.y = "Patient #")

dat_full <- dat_full %>% 
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1, NA)), factor)

dat_key <- dat_full %>% 
  dplyr::select(
    pid, group_nsaid_2line
  )


## Summary Tables 

## T-test and Chi-square Table Fn 

see_x <- dat_full %>% dplyr::select(pid, remission, group_nsaid_2line) %>% 
  filter(group_nsaid_2line == "MT1")

dat_sum_all <- dat_full %>% 
  dplyr::select(!pid)

dat_sum_2lin <- dat_sum_all %>% 
  filter(
    group_nsaid_2line == "Second Line"
  )




dat_sum_all$nsaids_trialed <- as.factor(dat_sum_all$nsaids_trialed)

library(table1)

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}

pvalueT <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g), simulate = T)$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

pvalueANOVA <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    ano <- aov(y ~ g)
    p <- summary(ano)[[1]][[5]][1]
    
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g), simulate = T)$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

## Tables 

# Table 1
names(dat_sum_all)

label(dat_sum_all$age_at_onset_of_sx) <- "Age at Onset of Symptoms"
label(dat_sum_all$`symptom onset to date of diagnosis`) <- "Symptom Onset to Date of Diagnosis (days)"
label(dat_sum_all$`interval from symptom onset to treatment`) <- "Interval from Symptom Onset to Treatment (days)"
label(dat_sum_all$nsaids_trialed) <- "# NSAIDs Trialed"
dat_sum_all$regions_affected <- as.factor(dat_sum_all$regions_affected)
label(dat_sum_all$regions_affected) <- "Regions Affected"

t1_o <- table1(
  ~ age_at_onset_of_sx + `Age at diagnosis` + 
    `symptom onset to date of diagnosis` + 
    `Duration of follow-up` + 
    `ESR at presentation` + 
    `CRP at presentation (mg/dL)` + 
    `WBC at presentation` + 
    regions_affected + 
    `Total Number of whole body MRI` + 
    `interval from symptom onset to treatment` + 
    `Total # days on NSAID monotherapy` + 
    nsaids_trialed + 
    `Head & face` +                                   
    `Upper torso (clavicle, scapula, sternum, rib)` +
    `upper extremity`  +                           
    `lower extremity` +                            
    `Neck and back (spine, not sacrum)` +          
    `Lower torso (sacrum, pelvis)` +                
    `symmetry same bone` +                           
    `symmetry same region` | group_nsaid_2line,
  data = dat_sum_all,
  # overall = F,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)

table1(
  ~ `Total # days on NSAID monotherapy` | group_nsaid_2line,
  data = dat_sum_all,
  # overall = F,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)

ggplot(
  data = subset(dat_sum_all, subset = group_nsaid_2line == "MT2"),
  aes(
    x = `Total # days on NSAID monotherapy`
    )
) +
  geom_histogram(
    position = "identity",
    color = "black",
    fill = "lightblue",
    bins = 13
    ) + 
  theme_bw(
    
  ) +
  facet_wrap(
    ~ group_nsaid_2line
  )


t1_p <- table1(
  ~ age_at_onset_of_sx + `Age at diagnosis` + 
    `symptom onset to date of diagnosis` + 
    `Duration of follow-up` + 
    `ESR at presentation` + 
    `CRP at presentation (mg/dL)` + 
    `WBC at presentation` + 
    regions_affected + 
    `Total Number of whole body MRI` + 
    `interval from symptom onset to treatment` + 
    `Total # days on NSAID monotherapy` + 
    nsaids_trialed + 
    `Head & face` +                                   
    `Upper torso (clavicle, scapula, sternum, rib)` +
    `upper extremity`  +                           
    `lower extremity` +                            
    `Neck and back (spine, not sacrum)` +          
    `Lower torso (sacrum, pelvis)` +                
    `symmetry same bone` +                           
    `symmetry same region` | group_nsaid_2line,
  data = dat_sum_all,
  overall = F,
  extra.col=list(`p-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

# Table 2 

# •	How many patients in each group (total, 1, 2, 3)

label(dat_sum_all$sex) <- "Sex"
label(dat_sum_all$`inflammatory arthritis`) <- "Inflammatory Arthritis"
label(dat_sum_all$`inflammatory bowel disease`) <- "Inflammatory Bowel Disease"
label(dat_sum_all$psoriasis) <- "Psoriasis"
label(dat_sum_all$extraosseous_inv) <- "Had Extraosseous Involvement"
label(dat_sum_all$`acne, severe`) <- "Acne (Severe)"
label(dat_sum_all$`arthritis (distant to the site of osteitis)`) <- "Arthritis (distant to the site of osteitis)"
label(dat_sum_all$`apthous ulceration`) <- "Apthous Ulceration"
label(dat_sum_all$`Bx proven (1 or 0)`) <- "Biopsy Proven"
label(dat_sum_all$`Biopsy results (N, A, AC, C, CF)`) <- "Biopsy Results"
label(dat_sum_all$`Lesions evident on plain films (1 or 0)`) <- "Lesions Evident of Plain Films"
label(dat_sum_all$unifocal_disease_suspected) <- "Unifocal Disease Suspected"

label(dat_sum_all$`active disease`) <- "Active Disease"
label(dat_sum_all$`inactive disease on tx`) <- "Inactive Disease on Tx"
label(dat_sum_all$remission) <- "Remission"

t2_o <- table1(
  ~ sex + Race + Ethnicity + 
    unifocal_disease_suspected + 
    `inflammatory arthritis` + `inflammatory bowel disease` + psoriasis + 
    extraosseous_inv + `acne, severe` + `arthritis (distant to the site of osteitis)` + 
    `apthous ulceration`  + `Bx proven (1 or 0)` + 
    `Biopsy results (N, A, AC, C, CF)` + `Lesions evident on plain films (1 or 0)` + 
    `Total # of distinct lesions` | group_nsaid_2line,
  data = dat_sum_all,
  # overall = F,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)


t2_p <- table1(
  ~ sex + Race + Ethnicity + 
    unifocal_disease_suspected + 
    `inflammatory arthritis` + `inflammatory bowel disease` + psoriasis + 
    extraosseous_inv + `acne, severe` + `arthritis (distant to the site of osteitis)` + 
    `apthous ulceration`  + `Bx proven (1 or 0)` + 
    `Biopsy results (N, A, AC, C, CF)` + `Lesions evident on plain films (1 or 0)` + 
    `Total # of distinct lesions` | group_nsaid_2line,
  data = dat_sum_all,
  overall = F,
  extra.col=list(`p-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

# Table 3

# o	2nd line agent group
# o	status at the end of the study 
# 	active disease (row 267)
# 	inactive disease on tx (row 268)
# 	remission (row 269)
# o	# patients with each bone involved (rows 232 to 256

names(dat_sum_2lin)

table(dat_sum_2lin$remission, useNA = "ifany")

t3_o <- table1(
  ~ MTX + `TNF inihib` + 
  `MTX at same time as TNF`  + `spine only (C, T, L)` + 
    `active disease` + `inactive disease on tx` + remission | group_nsaid_2line,
  data = dat_sum_2lin,
  overall = F,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)



## Locations 

paste(colnames(dat_sum_all[, 53:77]), sep = ", ")

t4_o <- table1(
  ~ `Upper torso (clavicle, scapula, sternum, rib)`  + `upper extremity`  +
    `lower extremity` + `Neck and back (spine, not sacrum)` +
    `Lower torso (sacrum, pelvis)`  + mandible +
    clavicle + Sternum + scapula + ribs + `proximal humerus` +
    `distal humerus` + `proximal ulna`  + `distal ulna` + 
    `proximal radius` + `distal radius` + 
    `hand bones (carpal, metacarpal, phalanges)` +
    `proximal femur` + `distal femur` +
    `proximal tibia` + patella + `distal tibia` + 
    `proximal fibula` + `distal fibula` + 
    `foot (calcaneus, tarsal, metatarsal, phalanges)` + 
    `cervical spine` + 
    `thoracic spine` + `lumbar spine` + sacrum  + pelvis
    | group_nsaid_2line,
  data = dat_sum_all,
  # extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)


t4_p <- table1(
  ~ `Upper torso (clavicle, scapula, sternum, rib)`  + `upper extremity`  +
    `lower extremity` + `Neck and back (spine, not sacrum)` +
    `Lower torso (sacrum, pelvis)`  + mandible +
    clavicle + Sternum + scapula + ribs + `proximal humerus` +
    `distal humerus` + `proximal ulna`  + `distal ulna` + 
    `proximal radius` + `distal radius` + 
    `hand bones (carpal, metacarpal, phalanges)` +
    `proximal femur` + `distal femur` +
    `proximal tibia` + patella + `distal tibia` + 
    `proximal fibula` + `distal fibula` + 
    `foot (calcaneus, tarsal, metatarsal, phalanges)` + 
    `cervical spine` + 
    `thoracic spine` + `lumbar spine` + sacrum  + pelvis
  | group_nsaid_2line,
  data = dat_sum_all,
  overall = F,
  extra.col=list(`p-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

# Extra 
dat_remis <- dat_sum_all %>% 
  select(
    `active disease`, `inactive disease on tx`, remission, group_nsaid_2line
  )

## Updated 4/17 from KD email 
dat_remis[24, 3] <- 1
dat_remis[66, 3] <- 1

t5_o <- table1(
  ~ `active disease` + `inactive disease on tx` + remission | group_nsaid_2line,
  data = dat_remis,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)


t5_p <- table1(
  ~ `active disease` + `inactive disease on tx` + remission | group_nsaid_2line,
  data = dat_remis,
  overall = F,
  extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

dat_pam <- dat_sum_2lin %>% 
  filter(
    dat_sum_2lin$pamidronate == 1
  )

t6_o <- table1(
  ~ `Total # of pamidronate infusions` ,
  data = dat_pam,
  # overall = F,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

dat_pam %>% 
  select(`Total # of pamidronate infusions`)


## CPR 

dat_crp <- ...

dat_crp <- merge(dat_crp, dat, by = 'pid')

dat_crp <- dat_crp %>% 
  mutate(
    Detect = case_when(
      `CRP at presentation (mg/dL)` == "Undetectable" ~ 0,
      is.na(`CRP at presentation (mg/dL)`) ~ NA, 
      TRUE ~ 1
    ),
    CRP = case_when(
      `CRP at presentation (mg/dL)` == "Undetectable" ~ "0",
      TRUE ~ `CRP at presentation (mg/dL)`
    )
  )

dat_crp$CRP <- as.numeric(dat_crp$CRP)

label(dat_crp$CRP) <- "CRP at presentation (mg/dL)"
dat_crp$Detect <- factor(dat_crp$Detect, labels = c("No", "Yes"))

label(dat_crp$Detect) <- "CPR Detectable?"


table1(
  ~ CRP + Detect | group_nsaid_2line,
  data = dat_crp,
  overall = F,
  extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

table1(
  ~ CRP + Detect | group_nsaid_2line,
  data = dat_crp,
  # overall = T,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)


## More data 

dat_mri <- ...

dat_mri <- merge(dat_mri, dat_key, by = "pid")

dat_mri <- dat_mri %>% 
  mutate(
    `Total Number of whole body MRI` = case_when(
      `Total Number of whole body MRI` >= 3 ~ "3+",
      TRUE ~ as.character(`Total Number of whole body MRI`)
    )
  )

dat_mri$`Total Number of whole body MRI` <- as.factor(dat_mri$`Total Number of whole body MRI`)


  
table1(
  ~ `Total Number of whole body MRI` | group_nsaid_2line,
  data = dat_mri,
  overall = F,
  extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

table1(
  ~ `Total Number of whole body MRI` | group_nsaid_2line,
  data = dat_mri,
  # overall = T,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)


dat_reg <- dat_sum_all %>% 
  select(
    regions_affected, group_nsaid_2line
  )

dat_reg$regions_affected <- as.numeric(dat_reg$regions_affected)

label(dat_reg$regions_affected) <- "Regions Affected"

table1(
  ~ regions_affected | group_nsaid_2line,
  data = dat_reg,
  # overall = T,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

## Pain Amplication 

dat_pain_amp <- ...

dat_pain_amp <- merge(dat_key, dat_pain_amp, by.x = "pid", by.y = "PID")

dat_pain_amp$`pain amplification` <- factor(dat_pain_amp$`pain amplification`, labels = c("No", "Yes"))
label(dat_pain_amp$`pain amplification`) <- "Pain Amplification"

dat_pain_amp$`vertebral complications` <- factor(dat_pain_amp$`vertebral complications`, labels = c("No", "Yes"))
label(dat_pain_amp$`vertebral complications`) <- "Vertebral Complications"

table1(
  ~ `pain amplification` + `vertebral complications` | group_nsaid_2line,
  data = dat_pain_amp,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)

table1(
  ~ `pain amplification` + `vertebral complications` | group_nsaid_2line,
  data = dat_pain_amp,
  overall = F,
  extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)


dat_nsaid_only <- dat_n2

table1(
~ total_monotherapy_days, 
  data = dat_n2,
  # overall = F,
  # extra.col=list(`P-value`= pvalueANOVA),
  render.continuous = render.cont,
  render.categorical = render.cat
)
