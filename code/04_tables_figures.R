
# Load data ---------------------------------------------------------------

source(here::here("code","03_small_data.R"))

# Formatting options ------------------------------------------------------

myColorsGroups<- brewer.pal(3,"Dark2")
myShapesGroups<- c(15,17,19)
names(myColorsGroups)<-c("GG_NE","ActiveControl","CONTROL")
names(myShapesGroups)<-c("GG_NE","ActiveControl","CONTROL")
groupcolor<-scale_colour_manual(name="Group",
                                values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                labels=c("GG-FL","Active control","Typical control"))
groupfill<-scale_fill_manual(name="Group",
                             values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                             labels=c("GG-FL","Active control","Typical control"))
groupshape <- scale_shape_manual(name="Group",
                                 values=myShapesGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                 labels=c("GG-FL","Active control","Typical control"))

greyGroups <- grey.colors(3, start = 0.2, end = 0.7)
names(greyGroups) <- c("GG_NE","ActiveControl","CONTROL")

myColorsRisk <- c("#1F78B4", "#33A02C")
names(myColorsRisk)<-c("0","1")
riskcolor<-scale_colour_manual(name="Group",
                               values=myColorsRisk, limits=c("0", "1"),
                               labels=c("Typical control","At-risk"))
riskfill<-scale_fill_manual(name="Group",
                            values=myColorsRisk, limits=c("0", "1"),
                            labels=c("Typical control","At-risk"))

# Axes
time.labs <- c("pre-test", "post-test")
names(time.labs) <- c("ses-01", "ses-02")
group.labs <- c("GG-FL", "Active control", "Typical control")
names(group.labs) <- c("GG_NE", "ActiveControl", "CONTROL")
risk.labs <- c("Typical control", "At-risk")
names(risk.labs) <- c("0", "1")
roi.labs <- c("L Fusiform", "L Inferior Temporal", "L Middle Temporal","L Superior Temporal",
              "L Supramarginal", "L pars Op", "L pars Tri", "R Fusiform","R Inferior Temporal",
              "R Middle Temporal", "R Superior Temporal", "R Supramarginal", "R pars Op", "R pars Tri")

names(roi.labs) <- c("lh_fusiform", "lh_inferiortemporal", "lh_middletemporal", "lh_superiortemporal",
                     "lh_supramarginal", "lh_parsopercularis", "lh_parstriangularis",
                     "rh_fusiform", "rh_inferiortemporal", "rh_middletemporal", "rh_superiortemporal",
                     "rh_supramarginal","rh_parsopercularis","rh_parstriangularis")

mytheme <- theme_bw() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

jitter <- position_jitter(width=0.05, seed=123)
dodge <- position_dodge(width = 0.4)


# Load ROI annot images ---------------------------------------------------

lh_smg_img <- tiff::readTIFF(here("figs", "annot", "lh_smg_annot_cut.tiff"))
lh_mtg_img <- tiff::readTIFF(here("figs", "annot", "lh_mtg_annot_cut.tiff"))
lh_stg_img <- tiff::readTIFF(here("figs", "annot", "lh_stg_annot_cut.tiff"))
rh_smg_img <- tiff::readTIFF(here("figs", "annot", "rh_smg_annot_cut.tiff"))
rh_itg_img <- tiff::readTIFF(here("figs", "annot", "rh_itg_annot_cut.tiff"))

# Table 1 -----------------------------------------------------------------

analyzed <- sublist_final %>%
  filter(!is.na(incl_final)) %>%
  filter(!incl_final %in% "no") %>%
  unite("idtime", c("id","time"), sep="_") %>%
  filter(!idtime %in% c("sub-074_ses-02")) %>%
  separate("idtime", into=c("id","time"), sep="_", extra="drop") %>%
  filter(!id %in% "sub-081")

analyzed_ids <- analyzed %>%
  group_by(id) %>%
  tally() %>%
  select(id)

table_dat <- merge(analyzed_ids, subject_data, by="id", all.x=T)

table1_gtsum <- table_dat %>%
  mutate(expectedHours=(15*6*12)/60) %>%
  mutate(proportionPlayed=(HoursPlayed/expectedHours)*100) %>%
  mutate(gamePeriod_weeks=gamePeriod_days/7) %>%
  select(intervention, sex, FR, iq, LQ, pre_ageM, post_ageM, M_SES, HoursPlayed, gamePeriod_weeks, proportionPlayed) %>%
  mutate(sex=recode_factor(sex, "Jongen"="M","Meisje"="F")) %>%
  mutate(LQ=recode_factor(LQ, "LH"="Left-handed","RH"="Right-handed","AD"="Ambidextrous")) %>%
  mutate(M_SES=recode_factor(M_SES, "1"="Low","2"="Middle","3"="High")) %>%
  rename(group=intervention) %>%
  mutate(group=ifelse(group=="GG_NE","GG-FL",
                      ifelse(group=="ActiveControl","Active Control","Typical Control"))) %>%
  mutate_if(is.character, ~factor(.)) %>%
  mutate(group=forcats::fct_relevel(group, "GG-FL", "Active Control")) %>%
  tbl_summary(by=group,
              type = list(c(FR, sex) ~ "dichotomous"),
              value = list(sex ~ "F"),
              statistic = list(all_continuous() ~ c("{median} ({min} - {max})"),
                               all_categorical() ~ "{n}"),
              digits = list(c(pre_ageM, post_ageM) ~ 0, c(iq, HoursPlayed, gamePeriod_weeks, proportionPlayed) ~ 0),
              label=list(pre_ageM ~ "Age at pre-test MRI (months)", post_ageM ~ "Age at post-test MRI (months)",
                         iq ~ "Non-verbal intelligence", FR ~ "Familial risk",
                         LQ ~ "Handedness", sex ~ "Sex (female/male)", M_SES ~ "Socioeconomic status",
                         HoursPlayed ~ "Training exposure (hours)",
                         proportionPlayed ~ "Proportion of completed intervention (%)",
                         gamePeriod_weeks ~ "Training period (weeks)")) %>%
  add_p(pvalue_fun = ~papaja::printp(.x))

table1_tbl <- table1_gtsum %>%
  gtsummary::as_tibble() %>%
  rename(Variable=1) %>% rename(GG=2) %>% rename(AC=3) %>%
  rename(TC=4) %>% rename(`p-value`=5) %>%
  mutate(GG = ifelse(Variable == "Sex (female/male)", paste0(GG,"/",29-as.numeric(GG)),GG)) %>%
  mutate(AC = ifelse(Variable == "Sex (female/male)", paste0(AC,"/",25-as.numeric(AC)),AC)) %>%
  mutate(TC = ifelse(Variable == "Sex (female/male)", paste0(TC,"/",24-as.numeric(TC)),TC)) %>%
  add_row(Variable = "Left/right/ambidextrous", GG = "2/26/1", AC = "2/21/2", TC = "2/20/2") %>%
  add_row(Variable = "Low/middle/high/unknown", GG = "7/11/11/0", AC = "8/12/4/1", TC = "3/8/13/0") %>%
  filter(!Variable %in% c("Left-handed","Right-handed","Ambidextrous",
                          "Low","Middle","High","Unknown")) %>%
  mutate(TC = ifelse(Variable == "Training exposure (hours)",NA,TC)) %>%
  mutate(TC = ifelse(Variable == "Training period (weeks)",NA,TC)) %>%
  mutate(TC = ifelse(Variable == "Proportion of completed intervention (%)",NA,TC)) %>%
  mutate(Variable = factor(Variable, levels=c("Sex (female/male)","Familial risk",
                                              "Non-verbal intelligence","Handedness",
                                              "Left/right/ambidextrous","Age at pre-test MRI (months)",
                                              "Age at post-test MRI (months)", "Socioeconomic status",
                                              "Low/middle/high/unknown","Training exposure (hours)",
                                              "Training period (weeks)","Proportion of completed intervention (%)"))) %>%
  arrange(Variable) %>%
  rename(`GG-Flemish (n = 29)`=2) %>%
  rename(`Active control (n = 25)`=3) %>%
  rename(`Typical control (n = 24)` = 4)

table1_flex <- flextable::flextable(table1_tbl) %>%
  # line_spacing(space=0.2, part="all") %>%
  # flextable::autofit(add_w = 0, add_h = 0) %>%
  # flextable::autofit() %>%
  font(fontname="Times New Roman", part="all") %>%
  set_table_properties(layout = "autofit") %>%
  flextable::theme_booktabs(bold_header = T) %>%
  flextable::footnote(., part="header",
                      i=1,j=2:4,
                      value=as_paragraph("Counts (n) or Median (range)"),
                      ref_symbols = "a") %>%
  flextable::footnote(., part="header",inline=T,
                      i=1,j=5,
                      value=as_paragraph("Group differences were assessed using a Kruskal-Wallis rank sum test for age, non-verbal intelligence and training characteristics, a Pearson's Chi-squared test for sex, handedness and socioeconomic status, and a Fisher's exact test for familial risk"),
                      ref_symbols = "b") %>%
  flextable::footnote(., part="body",inline=T,
                      i=4,j=1,
                      value=as_paragraph("Based on a subset of questions adapted from the Edinburgh Handedness Inventory (Oldfield, 1971)"),
                      ref_symbols = "c") %>%
  flextable::footnote(., part="body",inline=T,
                      i=8,j=1,
                      value=as_paragraph("Based on maternal educational level (low = no extra degree after secondary school; middle = professional bachelor/academic bachelor; high = Master or PhD)."),
                      ref_symbols = "d") %>%
  flextable::footnote(., part="body",inline=T,
                      i=12,j=1,
                      value=as_paragraph("The proportion of completed intervention is calculated as the number of hours spent on-task divided by the total amount of hours children were instructed to play during 12 weeks (estimated at 18 h)."),
                      ref_symbols = "e") %>%
  flextable::add_header_row(top=T, values=c("","Group",""), colwidths=c(1,3,1))


# Figure 1 ----------------------------------------------------------------

# CONSORT flowchart not created in R

# Figure 2 ----------------------------------------------------------------

SA_models_pre_sex_rh_7_dat <- data.frame(area_pre_wide$id, rstandard(SA_models_pre_sex_rh[[7]])) %>%
  rename(id=1) %>%
  rename(resid=2) %>%
  filter(!resid < -2) %>%
  filter(!resid > 2)

SA_models_pre_sex_rh_7_dat <- merge(SA_models_pre_sex_rh_7_dat, area_pre_wide, by="id", all.x=T) %>%
  mutate(rh_supramarginal_z = scale(rh_supramarginal, scale=T, center=T))
SA_R_SMG_pre_risk_emms_4plot <- emmeans(SA_models_pre_sex_rh_7_trim, specs=pairwise~risk)$emmeans %>% summary(infer=T)

rSMG_risk_plot <- SA_models_pre_sex_rh_7_dat %>%
  mutate(risk=factor(risk, levels=c("0","1"))) %>%
  # filter(region %in% c("rh_supramarginal")) %>%
  filter(time %in% "ses-01") %>%
  ggplot(aes(x=risk, y=rh_supramarginal, colour=risk)) +
  geom_boxplot(width=.2, outlier.shape=NA, alpha=.5) +
  geom_point(alpha=.2, position=position_jitter(width=.2, seed=123)) +
  # geom_point(inherit.aes=F, data=SA_R_SMG_pre_risk_emms_4plot, aes(x=risk, y=emmean, colour=risk),size=2) +
  geom_pointrange(inherit.aes=F, data=SA_R_SMG_pre_risk_emms_4plot, aes(x=risk, y=emmean, ymin=lower.CL, ymax= upper.CL, colour=risk),
                  position=position_nudge(-0.22), size=1, shape=18) +
  # facet_wrap(~region, nrow=2, ncol=7, labeller=labeller(region=roi.labs)) +
  riskcolor +
  riskfill +
  labs(x="Group", title="R Supramarginal", y=bquote("Surface area"~(mm^2))) +
  scale_x_discrete(labels=c("Typical control","At-risk")) +
  # coord_flip() +
  mytheme +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5, vjust=2))

rSMG_risk_plot_img <- ggdraw() +
  draw_plot(rSMG_risk_plot) +
  draw_image(rh_smg_img, scale=0.25, x=-0.2, y=0.362)

ggsave(filename=here("figs","Figure2_annot.png"), plot=rSMG_risk_plot_img, height=5, width=4, units=c("in"), dpi=300)
tiff(here("figs","Figure2_annot.tiff"), units="in", width=4, height=5, res=300)
rSMG_risk_plot_img
dev.off()

# Figure 3 ----------------------------------------------------------------

CT_L_SMG_emms_4plot <- emmeans(CT_models_sex_robust[[7]], specs = trt.vs.ctrl ~ time|intervention)$emmeans %>% as.data.frame()
CT_L_SMG_emms_4lines <- as.data.frame(CT_L_SMG_emms_4plot %>% filter(time %in% "ses-01") %>% filter(intervention %in% "GG_NE") %>% select(emmean))$emmean

CT_lSMGinteraction_plot <- thickness %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  filter(region %in% c("lh_supramarginal")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(width=.2, outlier.shape=NA, aes(group=time, colour=intervention)) +
  geom_point(aes(group=id, colour=intervention), alpha=.15) +
  geom_line(aes(group=id, colour=intervention), alpha=.15) +
  geom_pointrange(inherit.aes=F, data=CT_L_SMG_emms_4plot[CT_L_SMG_emms_4plot$time=="ses-01",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(0.23)) +
  geom_pointrange(inherit.aes=F, data=CT_L_SMG_emms_4plot[CT_L_SMG_emms_4plot$time=="ses-02",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(-0.23)) +
  facet_wrap(~intervention, labeller=labeller(intervention=group.labs)) +
  scale_y_continuous(breaks=c(2.6, 2.8, 3.0, 3.2)) +
  labs(x="Session", y="Cortical thickness (mm)", title="L Supramarginal") +
  scale_x_discrete(labels=c("pre","post")) +
  groupcolor +
  groupshape +
  mytheme +
  theme(legend.position="bottom",
        axis.text.x=element_text(size=11),
        strip.text.x=element_text(size=12),
        axis.title.x = element_text(vjust=-0.5),
        plot.title=element_text(hjust=0.5, vjust=5),
        plot.margin=unit(c(1.5,0.1,0,0.05),"cm"))

CT_lSMGinteraction_plot_img <- ggdraw() +
  draw_plot(CT_lSMGinteraction_plot) +
  draw_image(lh_smg_img, scale=0.245, x=-0.376, y=0.42)
# draw_line(x=c(0.21, 0.28), y=c(0.51, 0.56), size=1, colour="#1B9E77")

ggsave(filename=here("figs","Figure3_annot.png"), plot=CT_lSMGinteraction_plot_img, height=5, width=4.5, units=c("in"), dpi=300)
tiff(here("figs","Figure3_annot.tiff"), units="in", width=4.5, height=5, res=300)
CT_lSMGinteraction_plot_img
dev.off()

# Figure 4 ----------------------------------------------------------------

CT_L_STG_emms_4plot <- emmeans(CT_models_sex_robust[[6]], specs = trt.vs.ctrl ~ time|intervention)$emmeans %>% as.data.frame()
CT_R_ITG_emms_4plot <- emmeans(CT_models_sex_robust[[9]], specs = trt.vs.ctrl ~ time|intervention)$emmeans %>% as.data.frame()

CT_lSTGinteraction_plot <- thickness %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  filter(region %in% c("lh_superiortemporal")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(width=.3, outlier.shape=NA, aes(group=time, colour=intervention), position=position_dodge(width=.4)) +
  geom_point(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  geom_pointrange(inherit.aes=F, data=CT_L_STG_emms_4plot[CT_L_STG_emms_4plot$time=="ses-01",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(0.25)) +
  geom_pointrange(inherit.aes=F, data=CT_L_STG_emms_4plot[CT_L_STG_emms_4plot$time=="ses-02",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(-0.25)) +
  facet_wrap(~intervention, labeller=labeller(intervention=group.labs)) +
  scale_y_continuous(limits=c(2.6, 3.6), breaks=c(2.6, 2.8, 3.0, 3.2, 3.4, 3.6)) +
  labs(x="Session", y="Cortical thickness (mm)", title="L Superior Temporal") +
  scale_x_discrete(labels=c("pre","post")) +
  groupcolor +
  groupshape +
  mytheme +
  theme(legend.position="none",
        axis.text.x=element_text(size=11),
        strip.text.x=element_text(size=12),
        axis.title.x = element_text(vjust=-0.5),
        plot.title=element_text(hjust=0.5, vjust=5),
        plot.margin=unit(c(1.6,0,1,0),"cm"))

CT_rITGinteraction_plot <- thickness %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  filter(region %in% c("rh_inferiortemporal")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(width=.3, outlier.shape=NA, aes(group=time, colour=intervention), position=position_dodge(width=.4)) +
  geom_point(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  geom_pointrange(inherit.aes=F, data=CT_R_ITG_emms_4plot[CT_R_ITG_emms_4plot$time=="ses-01",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(0.25)) +
  geom_pointrange(inherit.aes=F, data=CT_R_ITG_emms_4plot[CT_R_ITG_emms_4plot$time=="ses-02",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(-0.25)) +
  facet_wrap(~intervention, labeller=labeller(intervention=group.labs)) +
  scale_y_continuous(limits=c(2.6, 3.6), breaks=c(2.6, 2.8, 3.0, 3.2, 3.4, 3.6)) +
  scale_x_discrete(labels=c("pre","post")) +
  labs(x="Session", y="", title="R Inferior Temporal") +
  groupcolor +
  groupshape +
  mytheme +
  theme(legend.position="none",
        axis.text.x=element_text(size=11),
        strip.text.x=element_text(size=12),
        axis.title.x = element_text(vjust=-0.5),
        plot.title=element_text(hjust=0.5, vjust=5),
        plot.margin=unit(c(1.6,0.1,1,0.15),"cm"))

# ggarrange(CT_L_SMG_plot, CT_L_STG_plot, CT_R_ITG_plot, nrow=3, align="hv", labels="AUTO", common.legend=T)
CT_ACinteractions_plot <- ggarrange(CT_lSTGinteraction_plot, CT_rITGinteraction_plot, align="hv", labels="AUTO", common.legend=T, legend="bottom")
CT_ACinteractions_plot_img <- ggdraw() +
  draw_plot(CT_ACinteractions_plot) +
  draw_image(lh_stg_img, scale=0.11, x=-0.39, y=0.425) +
  draw_image(rh_itg_img, scale=0.11, x=0.108, y=0.425)

ggsave(filename=here("figs","Figure4_annot.png"),
       plot=CT_ACinteractions_plot_img,
       height=7, width=9, units=c("in"), dpi=300)

tiff(here("figs","Figure4_annot.tiff"), units="in", width=9, height=7, res=300)
CT_ACinteractions_plot_img
dev.off()

# Figure 5 ----------------------------------------------------------------

areaTotal_wide <- areaTotal_wide %>%
  mutate(lh_middletemporal_z = scale(lh_middletemporal, scale=T, center=T)) %>%
  mutate(lh_superiortemporal_z = scale(lh_superiortemporal, scale=T, center=T))

SA_L_MTG_emms_4plot <- emmeans(SA_models_sex_robust_lh[[3]], specs = trt.vs.ctrl ~ time|intervention)$emmeans %>% as.data.frame()
SA_L_STG_emms_4plot <- emmeans(SA_models_sex_robust_lh[[6]], specs = trt.vs.ctrl ~ time|intervention)$emmeans %>% as.data.frame()

SA_L_MTG_plot <- area %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  filter(region %in% c("lh_middletemporal")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(width=.3, outlier.shape=NA, aes(group=time, colour=intervention), position=position_dodge(width=.4)) +
  geom_point(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  # geom_pointrange(inherit.aes=F, data=SA_L_MTG_emms_4plot,
  #                 aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
  #                 shape=18, size=1, position=position_dodge(0.4)) +
  # geom_line(inherit.aes=F, data=SA_L_MTG_emms_4plot,
  #           aes(x=time, y=emmean, colour=intervention, group=intervention), position=position_dodge(0.4)) +
  geom_pointrange(inherit.aes=F, data=SA_L_MTG_emms_4plot[SA_L_MTG_emms_4plot$time=="ses-01",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(0.25)) +
  geom_pointrange(inherit.aes=F, data=SA_L_MTG_emms_4plot[SA_L_MTG_emms_4plot$time=="ses-02",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(-0.25)) +
  facet_wrap(~intervention, labeller=labeller(intervention=group.labs)) +
  labs(x="Session", y=bquote("Surface area"~(mm^2)), title="L Middle Temporal") +
  # scale_y_continuous(limits=c(2300, 5000), breaks=c(2500, 3000, 3500, 4000, 4500, 5000)) +
  scale_y_continuous(limits=c(2000, 4500), breaks=c(2000, 2500, 3000, 3500, 4000, 4500)) +
  scale_x_discrete(labels=c("pre","post")) +
  groupcolor +
  groupshape +
  mytheme +
  theme(legend.position="none",
        axis.text.x=element_text(size=11),
        strip.text.x=element_text(size=12),
        axis.title.x = element_text(vjust=-0.5),
        plot.title=element_text(hjust=0.5, vjust=5),
        plot.margin=unit(c(1.6,0,1,0),"cm"))

SA_L_STG_plot <- area %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  filter(region %in% c("lh_superiortemporal")) %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(width=.3, outlier.shape=NA, aes(group=time, colour=intervention), position=position_dodge(width=.4)) +
  geom_point(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  # geom_pointrange(inherit.aes=F, data=SA_L_STG_emms_4plot,
  #                 aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
  #                 shape=18, size=1, position=position_dodge(0.4)) +
  # geom_line(inherit.aes=F, data=SA_L_STG_emms_4plot,
  #           aes(x=time, y=emmean, colour=intervention, group=intervention), position=position_dodge(0.4)) +
  geom_pointrange(inherit.aes=F, data=SA_L_STG_emms_4plot[SA_L_STG_emms_4plot$time=="ses-01",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(0.25)) +
  geom_pointrange(inherit.aes=F, data=SA_L_STG_emms_4plot[SA_L_STG_emms_4plot$time=="ses-02",],
                  aes(x=time, y=emmean, ymin=asymp.LCL, ymax= asymp.UCL, colour=intervention),
                  shape=18, size=1, position=position_nudge(-0.25)) +
  facet_wrap(~intervention, labeller=labeller(intervention=group.labs)) +
  labs(x="Session", y="", title="L Superior Temporal") +
  # scale_y_continuous(limits=c(2300, 5000), breaks=c(2500, 3000, 3500, 4000, 4500, 5000)) +
  scale_y_continuous(limits=c(2500, 5000), breaks=c(2500, 3000, 3500, 4000, 4500, 5000)) +
  scale_x_discrete(labels=c("pre","post")) +
  groupcolor +
  groupshape +
  mytheme +
  theme(legend.position="bottom",
        axis.text.x=element_text(size=11),
        strip.text.x=element_text(size=12),
        axis.title.x = element_text(vjust=-0.5),
        plot.title=element_text(hjust=0.6, vjust=5),
        plot.margin=unit(c(1.6,0.6,1,0),"cm"))

SA_interactions_plot <- ggarrange(SA_L_MTG_plot, SA_L_STG_plot, align="hv", labels="AUTO", common.legend=T, legend="bottom")
SA_interactions_plot_img <- ggdraw() +
  draw_plot(SA_interactions_plot) +
  draw_image(lh_mtg_img, scale=0.11, x=-0.378, y=0.44) +
  draw_image(lh_stg_img, scale=0.11, x=0.122, y=0.44)

ggsave(filename=here("figs","Figure5_annot.png"),
       plot=SA_interactions_plot_img,
       height=7, width=9, units=c("in"), dpi=300)

tiff(here("figs","Figure5_annot.tiff"), units="in", width=9, height=7, res=300)
SA_interactions_plot_img
dev.off()
