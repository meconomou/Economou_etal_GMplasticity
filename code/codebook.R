# Codebook ----------------------------------------------------------------

# code to create codebook.csv (already generated in this repository)

require(codebook)
require(labelled)
require(data.table)
require(here)
require(tidyverse)

codebook_data <- fread(here::here("data","tidy","subject_data.csv"), header=T, dec=".", sep=",", na="NA")

var_label(codebook_data$id) <- "Unique subject identifier"
var_label(codebook_data$intervention) <- "Intervention group assignment"
val_labels(codebook_data$intervention) <- c("GG-Flemish" = "GG_NE",
                                            "Active control" = "ActiveControl",
                                            "Typical control" = "CONTROL")

var_label(codebook_data$sex) <- "sex"
val_labels(codebook_data$sex) <- c("female" = "F", "male" = "M")

var_label(codebook_data$risk) <- "Cognitive risk for dyslexia (identified by early large-scale screening)"
val_labels(codebook_data$risk) <- c("Has a risk" = 1, "Does not have a risk" = 0)

var_label(codebook_data$FR) <- "Familial risk for dyslexia (at least one first-degree relative with a diagnosis)"
val_labels(codebook_data$FR) <- c("Has a risk" = 1, "Does not have a risk" = 0)

var_label(codebook_data$M_SES) <- "Socio-economic status (maternal education level)"
val_labels(codebook_data$M_SES) <- c("No extra degree after secondary school" = 1,
                                     "Professional/academic bachelor" = 2,
                                     "Master or PhD" = 3)

var_label(codebook_data$LQ) <- "Laterality quotient (based on a subset of questions from the Edinburgh Handedness Inventory)"
val_labels(codebook_data$LQ) <- c("Left handed" = "LH",
                                  "Ambidextrous" = "AD",
                                  "Right handed" = "RH")

var_label(codebook_data$iq) <- "Non-verbal intelligence (assessed using Raven's colored progressive matrices)"
var_label(codebook_data$HoursPlayed) <- "Total amount spent playing the assigned intervention game (hours)"
var_label(codebook_data$gamePeriod_days) <- "Duration of the total intervention playing phase (days)"


codebook_mri <- fread(here("data","tidy","dat_rois.csv"), header=T, dec=".", sep=",", na="NA")

var_label(codebook_mri$time) <- "Experimental session"
val_labels(codebook_mri$time) <- c("Pre-test" = "ses-01", "Post-test" = "ses-02")

var_label(codebook_mri$ageM) <- "Age at MRI (months)"

var_label(codebook_mri$region) <- "cortical region from Freesurfer parcellation"
val_labels(codebook_mri$region) <-
  c("Left Fusiform Gyrus" = "lh_fusiform","Right Fusiform Gyrus" = "rh_fusiform",
    "Left Inferior Temporal Gyrus" = "lh_inferiortemporal", "Right Inferior Temporal Gyrus" = "rh_inferiortemporal",
    "Left Middle Temporal Gyrus" = "lh_middletemporal", "Right Middle Temporal Gyrus" = "rh_middletemporal",
    "Left Superior Temporal Gyrus" = "lh_superiortemporal", "Right Superior Temporal Gyrus" = "rh_superiortemporal",
    "Left Supramarginal Gyrus" = "lh_supramarginal", "Right Supramarginal Gyrus" = "rh_supramarginal",
    "Left Inferior Frontal Gyrus (pars opercularis)" = "lh_parsopercularis", "Right Inferior Frontal Gyrus (pars opercularis)" = "rh_parsopercularis",
    "Left Inferior Frontal Gyrus (pars triangularis)" = "lh_parstriangularis", "Right Inferior Frontal Gyrus (pars triangularis)" = "rh_parstriangularis")

var_label(codebook_mri$measure) <- "Cortical measure"
val_labels(codebook_mri$measure) <- c("Cortical Thickness (mm)" = "thickness", "Cortical Surface Area (mm2)" = "area")

var_label(codebook_mri$value) <- "Value of cortical measure"

codebook_table_dem <- codebook_table(codebook_data) %>%
  select(name, label, value_labels) %>%
  filter(!name %in% c("pre_ageM","post_ageM"))

codebook_table_mri <- codebook_table(codebook_mri) %>%
  select(name, label, value_labels) %>%
  filter(name %in% c("time","ageM","region","measure","value"))

codebook <- rbind(codebook_table_dem, codebook_table_mri)

fwrite(codebook, file=here("codebook.csv"),
       col.names=TRUE, row.names=FALSE, dec=".",sep=";", na="NA")
fwrite(codebook, file=here("codebook.txt"),
       col.names=TRUE, row.names=FALSE, dec=".",sep=";", na="NA")
