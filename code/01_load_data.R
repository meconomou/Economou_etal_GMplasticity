# Load packages -----------------------------------------------------------

# Assuming these are locally installed

library(tidyverse)
library(here)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(lme4)
library(emmeans)
library(effects)
library(lmerTest)
library(gtsummary)
library(lubridate)
library(performance)
library(papaja)
library(png)
library(ggtext)
library(ggeffects)
library(parameters)
library(markdown)
library(flextable)
library(cowplot)
library(ggpubr)

# Helper functions --------------------------------------------------------

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

mergeAll_id       <- function(x, y){
  df            <- merge(x, y, by="id", all.x=T)
  return(df)
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

regexp_digit <- "[[:digit:]]+"

lm2df_sex <- function(s){
  dat<-do.call(rbind, lapply(s, joint_tests))
  dat <- dat %>% as.data.frame() %>%
    rename(variable=`model term`) %>%
    mutate(model = rep(c(1:14), each=2))
  dat <- merge(link, dat, by.y="model", all=T) %>%
    rename(roi=roilist) %>%
    rename(fstat=F.ratio) %>%
    rename(pval=p.value)
}

lm2df_gmv <- function(s){
  dat<-do.call(rbind, lapply(s, joint_tests))
  dat <- dat %>% as.data.frame() %>%
    rename(variable=`model term`) %>%
    mutate(model = rep(c(1:14), each=3))
  dat <- merge(link, dat, by.y="model", all=T) %>%
    rename(roi=roilist) %>%
    rename(fstat=F.ratio) %>%
    rename(pval=p.value)
}

# lm2df_lh_tsa <- function(s){
#   dat<-do.call(rbind, lapply(s, joint_tests))
#   dat <- dat %>% as.data.frame() %>%
#     rename(variable=`model term`) %>%
#     mutate(model = rep(c(1:7), each=3))
#   dat <- merge(link_lh, dat, by.y="model", all=T) %>%
#     rename(roi=roilist_lh) %>%
#     rename(fstat=F.ratio) %>%
#     rename(pval=p.value)
# }
#
# lm2df_rh_tsa <- function(s){
#   dat<-do.call(rbind, lapply(s, joint_tests))
#   dat <- dat %>% as.data.frame() %>%
#     rename(variable=`model term`) %>%
#     mutate(model = rep(c(1:7), each=3))
#   dat <- merge(link_rh, dat, by.y="model", all=T) %>%
#     rename(roi=roilist_rh) %>%
#     rename(fstat=F.ratio) %>%
#     rename(pval=p.value)
# }

# Load tidy data ----------------------------------------------------------

subject_data <- fread(here::here("data","tidy","subject_data.csv"), header=T, dec=".", sep=",", na="NA")
subject_data <- subject_data %>%
  mutate_if(is.character, ~factor(.)) %>%
  mutate_at("M_SES", factor) %>%
  mutate_at("FR", factor)

dat_rois <- fread(here("data","tidy","dat_rois.csv"), header=T, dec=".", sep=",", na="NA")
sublist_final <- fread(here("data","tidy","sublist_final.txt"), header=T, dec=".", sep=",", na="NA")
sublist_final_noNA <- fread(here("data","tidy","sublist_final_noNA.txt"), header=T, dec=".", sep=",", na="NA")

