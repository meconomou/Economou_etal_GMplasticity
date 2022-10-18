
# Load data ---------------------------------------------------------------

source(here::here("code","01_load_data.R"))

aseg_cs <- fread(here::here("data","tidy","aseg_cs.csv"), header=T, dec=".", sep=",", na="NA") %>%
  filter(!id %in% "sub-081")

dat_rois <- dat_rois %>%
  filter(!id %in% "sub-081") %>%
  mutate(intervention = factor(intervention, levels=c("ActiveControl","CONTROL","GG_NE")))

thickness <- dat_rois %>%
  filter(measure %in% "thickness") %>%
  select(id, time, risk, intervention, incl_final,
         ageM, region, value, sex) %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.)) %>%
  mutate(intervention = factor(intervention, levels=c("ActiveControl","CONTROL","GG_NE")))


area <- dat_rois %>%
  filter(measure %in% "area") %>%
  select(id, time, risk, intervention, incl_final,
         ageM, region, value, sex) %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.)) %>%
  mutate(intervention = factor(intervention, levels=c("ActiveControl","CONTROL","GG_NE")))

thickness_wide <- thickness %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

area_wide <- area %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

# Prepare for analysis controlling total area

area_total <- fread(here::here("data","tidy","all_data.csv"), header=T, dec=".", sep=",", na="NA") %>%
  filter(!id %in% "sub-081") %>%
  filter(measure %in% "area") %>%
  filter(region %in% c("lh_WhiteSurfArea","rh_WhiteSurfArea")) %>%
  pivot_wider(names_from=region, values_from=value) %>%
  rename(lh_totalArea = 4) %>%
  rename(rh_totalArea = 5) %>%
  mutate(across(c(lh_totalArea, rh_totalArea), ~ as.numeric(scale(.)))) %>%
  mutate_if(is.character, ~as.factor(.)) %>%
  select(id, time, lh_totalArea, rh_totalArea)

areaTotal_wide <- merge(area_wide, area_total, by=c("id","time"), all.x=T)

roilist_lh <- c("lh_fusiform", "lh_inferiortemporal", "lh_middletemporal", "lh_parsopercularis",
                "lh_parstriangularis", "lh_superiortemporal","lh_supramarginal")

roilist_rh <- c("rh_fusiform", "rh_inferiortemporal", "rh_middletemporal", "rh_parsopercularis",
                "rh_parstriangularis", "rh_superiortemporal","rh_supramarginal")

link_lh<-as.data.frame(roilist_lh)
link_lh$model<- 1:7
link_lh$model<-as.factor(link_lh$model)

link_rh<-as.data.frame(roilist_rh)
link_rh$model<- 1:7
link_rh$model<-as.factor(link_rh$model)

roilist <- c("lh_fusiform", "lh_inferiortemporal", "lh_middletemporal", "lh_parsopercularis",
             "lh_parstriangularis", "lh_superiortemporal","lh_supramarginal","rh_fusiform",
             "rh_inferiortemporal", "rh_middletemporal","rh_parsopercularis", "rh_parstriangularis",
             "rh_superiortemporal", "rh_supramarginal")

link<-as.data.frame(roilist)
link$model<- 1:14
link$model<-as.factor(link$model)


# Baseline CT -------------------------------------------------------------

thickness_pre <- thickness %>% filter(time %in% "ses-01") %>% mutate_at("risk", ~as.factor(.))

thickness_pre_wide <- thickness_pre %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

CT_models_pre_sex <- lapply(roilist, function(x) {
  lm(substitute(i ~ risk + sex, list(i=as.name(x))), data=thickness_pre_wide)
})
CT_pre_resid <- do.call(rbind, lapply(CT_models_pre_sex, performance::check_normality)) %>% as.data.frame()

# Trimming based on residuals + refitting

CT_models_pre_sex_11_dat <- data.frame(thickness_pre_wide$id, rstandard(CT_models_pre_sex[[11]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2) %>%
  filter(!resid > 2)

CT_models_pre_sex_11_dat <- merge(CT_models_pre_sex_11_dat, thickness_pre_wide, by="id", all.x=T)
CT_models_pre_sex_11_trim <- lm(rh_parsopercularis ~ risk + sex, data=CT_models_pre_sex_11_dat)
# CT_models_pre_sex_11_qqplot <- plot(check_normality(CT_models_pre_sex_11_trim), type="qq")

CT_models_pre_sex_14_dat <- data.frame(thickness_pre_wide$id, rstandard(CT_models_pre_sex[[14]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2) %>%
  filter(!resid > 2)

CT_models_pre_sex_14_dat <- merge(CT_models_pre_sex_14_dat, thickness_pre_wide, by="id", all.x=T)
CT_models_pre_sex_14_trim <- lm(rh_supramarginal ~ risk + sex, data=CT_models_pre_sex_14_dat)
# CT_models_pre_sex_14_qqplot <- plot(check_normality(CT_models_pre_sex_14_trim), type="qq")


# Baseline SA -------------------------------------------------------------

area_pre <- area %>% filter(time %in% "ses-01") %>% mutate_at("risk", ~as.factor(.))

area_pre_wide <- areaTotal_wide %>%
  filter(time %in% "ses-01") %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.))

SA_models_pre_sex_lh <- lapply(roilist_lh, function(x) {
  lm(substitute(i ~ risk + sex + lh_totalArea, list(i=as.name(x))), data=area_pre_wide)
})

SA_models_pre_sex_rh <- lapply(roilist_rh, function(x) {
  lm(substitute(i ~ risk + sex + rh_totalArea, list(i=as.name(x))), data=area_pre_wide)
})

SA_pre_resid_lh <- do.call(rbind, lapply(SA_models_pre_sex_lh, performance::check_normality)) %>% as.data.frame()
SA_pre_resid_rh <- do.call(rbind, lapply(SA_models_pre_sex_rh, performance::check_normality)) %>% as.data.frame()

# Trimming based on residuals + refitting

SA_models_pre_sex_lh_7_dat <- data.frame(area_pre_wide$id, rstandard(SA_models_pre_sex_lh[[7]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2.2) %>%
  filter(!resid > 2.2)

SA_models_pre_sex_lh_7_dat <- merge(SA_models_pre_sex_lh_7_dat, area_pre_wide, by="id", all.x=T)
SA_models_pre_sex_lh_7_trim <- lm(lh_supramarginal ~ risk + sex + lh_totalArea, data=SA_models_pre_sex_lh_7_dat)
# SA_models_pre_sex_lh_7_qqplot <- plot(check_normality(SA_models_pre_sex_lh_7_trim), type="qq")

SA_models_pre_sex_rh_1_dat <- data.frame(area_pre_wide$id, rstandard(SA_models_pre_sex_rh[[1]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2) %>%
  filter(!resid > 2)

SA_models_pre_sex_rh_1_dat <- merge(SA_models_pre_sex_rh_1_dat, area_pre_wide, by="id", all.x=T)
SA_models_pre_sex_rh_1_trim <- lm(rh_fusiform ~ risk + sex +  rh_totalArea, data=SA_models_pre_sex_rh_1_dat)
# SA_models_pre_sex_rh_1_qqplot <- plot(check_normality(SA_models_pre_sex_rh_1_trim), type="qq")

SA_models_pre_sex_rh_7_dat <- data.frame(area_pre_wide$id, rstandard(SA_models_pre_sex_rh[[7]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2) %>%
  filter(!resid > 2)

SA_models_pre_sex_rh_7_dat <- merge(SA_models_pre_sex_rh_7_dat, area_pre_wide, by="id", all.x=T) %>%
  mutate(rh_supramarginal_z = scale(rh_supramarginal, scale=T, center=T))

SA_models_pre_sex_rh_7_trim <- lm(rh_supramarginal ~ risk + sex +  rh_totalArea, data=SA_models_pre_sex_rh_7_dat)
SA_models_pre_sex_rh_7_trim_z <- lm(rh_supramarginal_z ~ risk + sex +  rh_totalArea, data=SA_models_pre_sex_rh_7_dat)

# Intervention CT ---------------------------------------------------------

CT_models_sex_robust <- lapply(roilist, function(x) {
  robustlmm::rlmer(substitute(i ~ time*intervention + sex + (1|id), list(i=as.name(x))), data=thickness_wide)
})

# Intervention SA ---------------------------------------------------------

SA_models_sex_robust_lh <- lapply(roilist_lh, function(x) {
  robustlmm::rlmer(substitute(i ~ time*intervention + sex + lh_totalArea + (1|id),
                              list(i=as.name(x))), data=areaTotal_wide)
})

SA_models_sex_robust_rh <- lapply(roilist_rh, function(x) {
  robustlmm::rlmer(substitute(i ~ time*intervention + sex + rh_totalArea + (1|id),
                              list(i=as.name(x))), data=areaTotal_wide)
})


# Write output ------------------------------------------------------------

save(CT_models_pre_sex, CT_models_pre_sex_11_trim, CT_models_pre_sex_14_trim,
     SA_models_pre_sex_lh, SA_models_pre_sex_rh,
     SA_models_pre_sex_lh_7_trim, SA_models_pre_sex_rh_1_trim, SA_models_pre_sex_rh_7_trim,
     SA_models_pre_sex_rh_7_trim_z, file = here::here("data","models","baseline_models.Rdata"))

save(CT_models_sex_robust, SA_models_sex_robust_lh, SA_models_sex_robust_rh,
     file = here::here("data","models","mixed_models.Rdata"))

