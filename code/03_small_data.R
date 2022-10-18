# Load data ---------------------------------------------------------------

source(here::here("code","01_load_data.R"))

thickness <- dat_rois %>%
  filter(measure %in% "thickness") %>%
  select(id, time, risk, intervention, incl_final,
         ageM, region, value, sex) %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.))

thickness_wide <- thickness %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

area <- dat_rois %>%
  filter(measure %in% "area") %>%
  select(id, time, risk, intervention, incl_final,
         ageM, region, value, sex) %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.))

area_wide <- area %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

thickness_pre <- thickness %>% filter(time %in% "ses-01") %>% mutate_at("risk", ~as.factor(.))
area_pre <- area %>% filter(time %in% "ses-01") %>% mutate_at("risk", ~as.factor(.))

thickness_pre_wide <- thickness_pre %>%
  arrange(region) %>%
  pivot_wider(names_from=region, values_from=c(value)) %>%
  mutate_if(is.character, ~as.factor(.))

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

area_pre_wide <- areaTotal_wide %>%
  filter(time %in% "ses-01") %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_if(is.character, ~as.factor(.))


# Load models -------------------------------------------------------------

load(here::here("data","models","baseline_models.Rdata"))
load(here::here("data","models","mixed_models.Rdata"))

roilist <- c("lh_fusiform", "lh_inferiortemporal", "lh_middletemporal", "lh_parsopercularis",
             "lh_parstriangularis", "lh_superiortemporal","lh_supramarginal","rh_fusiform",
             "rh_inferiortemporal", "rh_middletemporal","rh_parsopercularis", "rh_parstriangularis",
             "rh_superiortemporal", "rh_supramarginal")

link<-as.data.frame(roilist)
link$model<- 1:14
link$model<-as.factor(link$model)

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

# Results - baseline models -----------------------------------------------

CT_models_pre_sex_trim <- list(CT_models_pre_sex[[1]],CT_models_pre_sex[[2]],CT_models_pre_sex[[3]],CT_models_pre_sex[[4]],
                               CT_models_pre_sex[[5]],CT_models_pre_sex[[6]],CT_models_pre_sex[[7]],CT_models_pre_sex[[8]],
                               CT_models_pre_sex[[9]],CT_models_pre_sex[[10]],CT_models_pre_sex_11_trim,CT_models_pre_sex[[12]],
                               CT_models_pre_sex[[13]],CT_models_pre_sex_14_trim)
CT_pre_sex_results <- lm2df_sex(CT_models_pre_sex_trim)

SA_models_pre_sex_trim <- list(SA_models_pre_sex_lh[[1]], SA_models_pre_sex_lh[[2]], SA_models_pre_sex_lh[[3]], SA_models_pre_sex_lh[[4]],
                               SA_models_pre_sex_lh[[5]], SA_models_pre_sex_lh[[6]], SA_models_pre_sex_lh_7_trim, SA_models_pre_sex_rh_1_trim,
                               SA_models_pre_sex_rh[[2]],SA_models_pre_sex_rh[[3]],SA_models_pre_sex_rh[[4]],
                               SA_models_pre_sex_rh[[5]], SA_models_pre_sex_rh[[6]], SA_models_pre_sex_rh_7_trim)
SA_pre_sex_results <- lm2df_gmv(SA_models_pre_sex_trim)

set.seed(123)
SA_R_SMG_risk_results <- parameters::model_parameters(SA_models_pre_sex_rh_7_trim_z, bootstrap=TRUE, ci_method="bcai")


# Results - mixed models --------------------------------------------------

# example command to extract model parameters using the bootstrap method
# below the results for the regions reported in the manuscript
set.seed(123)
CT_L_SMG_results <- parameters::model_parameters(CT_models_sex_robust[[7]], effects="all", ci_method="boot", standardize="basic")
CT_L_STG_results <- parameters::model_parameters(CT_models_sex_robust[[6]], effects="all", ci_method="boot", standardize="basic")
CT_R_ITG_results <- parameters::model_parameters(CT_models_sex_robust[[9]], effects="all", ci_method="boot", standardize="basic")
SA_L_MTG_results <- parameters::model_parameters(SA_models_sex_robust_lh[[3]], effects="all", ci_method="boot", standardize="basic")
SA_L_STG_results <- parameters::model_parameters(SA_models_sex_robust_lh[[6]], effects="all", ci_method="boot", standardize="basic")

# Estimated Marginal Means
CT_L_STG_emms <- emmeans(CT_models_sex_robust[[6]], specs = trt.vs.ctrl ~ time|intervention)$contrasts %>%
  summary(infer=T) %>%
  as.data.frame() %>%
  mutate(p_fdr = p.adjust(p.value, method="fdr"))
CT_L_SMG_emms <- emmeans(CT_models_sex_robust[[7]], specs = trt.vs.ctrl ~ time|intervention)$contrasts %>%
  summary(infer=T) %>%
  as.data.frame() %>%
  mutate(p_fdr = p.adjust(p.value, method="fdr"))
CT_R_ITG_emms <- emmeans(CT_models_sex_robust[[9]], specs = trt.vs.ctrl ~ time|intervention)$contrasts %>%
  summary(infer=T) %>%
  as.data.frame() %>%
  mutate(p_fdr = p.adjust(p.value, method="fdr"))

SA_L_MTG_emms <-emmeans(SA_models_sex_robust_lh[[3]], specs = trt.vs.ctrl ~ time|intervention)$contrasts %>%
  summary(infer=T) %>%
  as.data.frame() %>%
  mutate(p_fdr = p.adjust(p.value, method="fdr"))

SA_L_STG_emms <-emmeans(SA_models_sex_robust_lh[[6]], specs = trt.vs.ctrl ~ time|intervention)$contrasts %>%
  summary(infer=T) %>%
  as.data.frame() %>%
  mutate(p_fdr = p.adjust(p.value, method="fdr"))


