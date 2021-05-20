##########################################################################
## Data wrangling for Explicit yoked group project                      ##
## -- St. Germain, Williams, Balbaa, Poskus, Leshchyshen, Lohse, Carter ##
##                                                                      ##
## Created by Laura St. Germain and Mike Carter                         ##
##########################################################################


# SCRIPT SETUP ---------------
#
# Required libraries
library(tidyverse)

# Load data files into base objects
motor_data <- readr::read_csv("data/2020-03-30_behaviour-data.csv")
qaire_data <- readr::read_csv("data/2020-03-30_qaire-data.csv")


# INITIAL SETUP FOR MOTOR PERFORMANCE DATA ---------------
#
# Create a workable tibble and add columns for non-resetting
# blocks and trials
motor_tib <- motor_data %>%
    dplyr::mutate(block_id_expt = rep(rep(1:7, each = 5), 150)) %>%
    dplyr::mutate(trial_expt = rep(1:35, 150))

# Re-order columns for readability
motor_tib <- motor_tib %>%
    dplyr::select(1:6, block_id_expt, trial_expt, everything())

# Turn variables into factors
motor_tib <- motor_tib %>%
    dplyr::mutate(
        group_id = forcats::as_factor(group_id),
        phase_id = forcats::as_factor(phase_id),
        block_id = forcats::as_factor(block_id),
        block_id_expt = forcats::as_factor(block_id_expt),
    )
dplyr::glimpse(motor_tib)


# CREATE TIBBLES OF MOTOR DATA FOR STATS AND FIGURES ---------------
#
# Tibble for entire experiment (7 blocks with 5 trials in each
# at the group level)
motor_7blocks_g_tib <- motor_tib %>%
    dplyr::group_by(group_id, phase_id, block_id_expt) %>%
    dplyr::summarize(
        n = n(),
        mean_mt = mean(time, na.rm = TRUE),
        sd_mt = sd(time, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(time)$ymin,
        ci_upp = ggplot2::mean_cl_normal(time)$ymax,
        .groups = "drop"
    )

# Tibble for acquisition phase (5 blocks of 5 trials at
# participant level)
motor_acq_p_tib <- motor_tib %>%
    dplyr::filter(phase_id == 2) %>%
    dplyr::group_by(id, group_id, phase_id, block_id) %>%
    dplyr::summarize(
        n = n(),
        mean_mt = mean(time, na.rm = TRUE),
        sd_mt = sd(time, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(time)$ymin,
        ci_upp = ggplot2::mean_cl_normal(time)$ymax,
        .groups = "drop"
    )

# Tibble for pre-test and retention (1 block of 5 trials each at
# participant level)
motor_pre_ret_p_tib <- motor_tib %>%
    dplyr::filter(phase_id != 2) %>%
    dplyr::group_by(id, group_id, phase_id, block_id) %>%
    dplyr::summarize(
        n = n(),
        mean_mt = mean(time, na.rm = TRUE),
        sd_mt = sd(time, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(time)$ymin,
        ci_upp = ggplot2::mean_cl_normal(time)$ymax,
        .groups = "drop"
    )

# Tibble for entire experiment (trial-by-trial at the group level)
motor_trialxtrial_g_tib <- motor_tib %>%
    dplyr::group_by(group_id, phase_id, trial_expt) %>%
    dplyr::summarize(
        n = n(),
        mean_mt = mean(time, na.rm = TRUE),
        sem = sd(time, na.rm = TRUE)/sqrt(n),
        se_low = mean_mt - sem,
        se_upp = mean_mt + sem,
        .groups = "drop"
    )

# Tibble of model request data (for each acquisition block at the group level)
model_reqs_g_tib <- motor_tib %>%
    dplyr::filter(phase_id == 2) %>%
    dplyr::filter(group_id == 1) %>%
    dplyr::group_by(group_id, phase_id, block_id) %>%
    dplyr::summarize(
        n = n(),
        mean_model = mean(choose_model, na.rm = TRUE),
        .groups = "drop"
    )

# Tibble of speed requests when model was selected (for each acquisition
# block at the group level)
speed_reqs_g_tib <- motor_tib %>%
    dplyr::filter(phase_id == 2) %>%
    dplyr::filter(group_id == 1) %>%
    dplyr::filter(choose_model == 1) %>%
    dplyr::group_by(group_id, phase_id, block_id) %>%
    dplyr::summarize(
        n = n(),
        mean_speed = mean(choose_speed, na.rm = TRUE),
        .groups = "drop"
    )


# INITIAL SETUP FOR QUESTIONNAIRE DATA ---------------
#
# Create a workable tibble and turn variables into factors
qaire_tib <- qaire_data %>%
    dplyr::mutate(
        group_id = forcats::as_factor(group_id),
        scale_id = forcats::as_factor(scale_id),
        time_id = forcats::as_factor(time_id)
    )
glimpse(qaire_tib)


# CREATE TIBBLES OF QAIRE DATA FOR STATS AND FIGURES ---------------
#
# Tibble for each scale at the participant level
qaire_im_p_tib <- qaire_tib %>%
    dplyr::filter(scale_id == "im")

qaire_pc_p_tib <- qaire_tib %>%
    dplyr::filter(scale_id == "pc")

qaire_pa_p_tib <- qaire_tib %>%
    dplyr::filter(scale_id == "pa")

# Tibbles for each scale at the group level
qaire_im_g_tib <- qaire_im_p_tib %>%
    dplyr::group_by(group_id, time_id) %>%
    dplyr::summarize(
        n = n(),
        mean_rating = mean(score, na.rm = TRUE),
        sd_rating = sd(score, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(score)$ymin,
        ci_upp = ggplot2::mean_cl_normal(score)$ymax,
        .groups = "drop"
    )

qaire_pc_g_tib <- qaire_pc_p_tib %>%
    dplyr::group_by(group_id, time_id) %>%
    dplyr::summarize(
        n = n(),
        mean_rating = mean(score, na.rm = TRUE),
        sd_rating = sd(score, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(score)$ymin,
        ci_upp = ggplot2::mean_cl_normal(score)$ymax,
        .groups = "drop"
    )

qaire_pa_g_tib <- qaire_pa_p_tib %>%
    dplyr::group_by(group_id, time_id) %>%
    dplyr::summarize(
        n = n(),
        mean_rating = mean(score, na.rm = TRUE),
        sd_rating = sd(score, na.rm = TRUE),
        ci_low = ggplot2::mean_cl_normal(score)$ymin,
        ci_upp = ggplot2::mean_cl_normal(score)$ymax,
        .groups = "drop"
    )
