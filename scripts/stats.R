##########################################################################
## Statistical analyses for Explicit yoked group project                ##
## -- St. Germain, Williams, Balbaa, Poskus, Leshchyshen, Lohse, Carter ##
##                                                                      ##
## Created by Laura St. Germain and Keith R Lohse                       ##
##########################################################################


# SCRIPT SETUP ---------------
#
# Load everything from other R script file
source("scripts/wrangle.R")

# Required libraries
library(jmv)
library(pwr)
library(TOSTER)

#-------------------------------------------------
# CONVERT TIBBLES TO WIDE FORMAT FOR JMV() LIBRARY
#
motor_acq_p_wide <- motor_acq_p_tib %>%
    dplyr::select(id, group_id, block_id, mean_mt) %>%
    tidyr::pivot_wider(names_from = block_id, values_from = mean_mt) %>%
    dplyr::rename(b1 = 3) %>% # specifying using column number
    dplyr::rename(b2 = 4) %>% # specifying using column number
    dplyr::rename(b3 = 5) %>% # specifying using column number
    dplyr::rename(b4 = 6) %>% # specifying using column number
    dplyr::rename(b5 = 7) # specifying using column number

motor_pre_ret_p_wide <- motor_pre_ret_p_tib %>%
    dplyr::select(id, group_id, phase_id, block_id, mean_mt) %>%
    tidyr::pivot_wider(names_from = phase_id, values_from = mean_mt) %>%
    dplyr::rename(pre = 4) %>%  # specifying using column number
    dplyr::rename(ret = 5) # specifying using column number

qaire_im_p_wide <- qaire_im_p_tib %>%
    tidyr::pivot_wider(names_from = time_id, values_from = score) %>%
    dplyr::rename(t1 = 4) %>% # specifying using column number
    dplyr::rename(t2 = 5) %>% # specifying using column number
    dplyr::rename(t3 = 6) %>% # specifying using column number
    dplyr::rename(t4 = 7) # specifying using column number

qaire_pc_p_wide <- qaire_pc_p_tib %>%
    tidyr::pivot_wider(names_from = time_id, values_from = score) %>%
    dplyr::rename(t1 = 4) %>% # specifying using column number
    dplyr::rename(t2 = 5) %>% # specifying using column number
    dplyr::rename(t3 = 6) %>% # specifying using column number
    dplyr::rename(t4 = 7) # specifying using column number


qaire_pa_p_wide <- qaire_pa_p_tib %>%
    tidyr::pivot_wider(names_from = time_id, values_from = score) %>%
    dplyr::rename(t1 = 4) %>% # specifying using column number
    dplyr::rename(t2 = 5) %>% # specifying using column number
    dplyr::rename(t3 = 6) %>% # specifying using column number
    dplyr::rename(t4 = 7) # specifying using column number


#-------------------------------------------------
# CHECK FOR INFLUENTIAL CASES
#
dat_inf <- motor_pre_ret_p_wide
dat_inf$group.c <- dat_inf$group_id
contrasts(dat_inf$group.c) <- contr.poly(3)
contrasts(dat_inf$group.c)

dat_inf$pre.c <- dat_inf$pre - mean(dat_inf$pre)
mod_inf <- lm(ret ~ pre.c + group.c, data = dat_inf)
# plot(mod_inf) # uncomment to see regression diagnostics
dat_inf$influence <- cooks.distance(mod_inf)
summary(dat_inf$influence)
dat_inf[dat_inf$influence > 1.0,]


#-------------------------------------------------
# STATISTICAL ANALYSES - MOTOR DATA
#
# Analysis on retention data
# Check interaction term for homogeneity of slopes
jmv::ancova(
    formula = ret ~ group_id + pre + group_id:pre,
    data = motor_pre_ret_p_wide
)

# Re-run but exclude interaction term as assumption met
jmv::ancova(
    formula = ret ~ group_id + pre,
    data = motor_pre_ret_p_wide,
    effectSize = "eta",
    emMeans = ~ group_id,
    emmTables = TRUE
)

# Run t-test on retention comparing self-controlled versus yoked
# for test of traditional self-controlled learning advantage
motor_sc_vs_yk_tib <- motor_pre_ret_p_wide %>%
    dplyr::filter(group_id != 3)

jmv::ttestIS(
    formula = ret ~ group_id,
    data = motor_sc_vs_yk_tib,
    students = FALSE,
    welchs = TRUE,
    effectSize = TRUE)


# Analysis on acquisition data
jmv::anovaRM(
    data = motor_acq_p_wide,
    rm = list(
        list(label = "block_id",
             levels = c("b1", "b2", "b3", "b4", "b5"))
        ),
    rmCells = list(
        list(measure = "b1", cell = "b1"),
        list(measure = "b2", cell = "b2"),
        list(measure = "b3", cell = "b3"),
        list(measure = "b4", cell = "b4"),
        list(measure = "b5", cell = "b5")
        ),
    bs = group_id,
    effectSize = "ges",
    rmTerms = ~ block_id,
    bsTerms = ~ group_id,
    spherCorr = "GG",
    postHoc = list("block_id"), # Only significant test
    postHocCorr = "holm"
)


#-------------------------------------------------
# STATISTICAL ANALYSES - QAIRE DATA
#
# Analysis on perceived autonomy
# Check interaction term for homogeneity of slopes
jmv::anovaRM(
    data = qaire_pa_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
    ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
    ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1 + group_id:t1,
    spherCorr = "GG"
)

# Re-run but exclude interaction term as assumption met
jmv::anovaRM(
    data = qaire_pa_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
    ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
    ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1,
    effectSize = "ges",
    spherCorr = "GG",
    postHoc = list("group_id"), # Only significant test
    postHocCorr = "holm",
    emMeans = ~ group_id,
    emmTables = TRUE
)


# Analysis on intrinsic motivation
# Check interaction term for homogeneity of slopes
jmv::anovaRM(
    data = qaire_im_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
        ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
        ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1 + group_id:t1,
    spherCorr = "GG"
)

# Re-run but exclude interaction term as assumption met
jmv::anovaRM(
    data = qaire_im_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
    ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
    ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1,
    effectSize = "ges",
    spherCorr = "GG"
)


# Analysis on perceived competence
# Check interaction term for homogeneity of slopes
jmv::anovaRM(
    data = qaire_pc_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
    ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
    ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1 + group_id:t1,
    spherCorr = "GG"
)

# Re-run but exclude interaction term as assumption met
jmv::anovaRM(
    data = qaire_pc_p_wide,
    rm = list(
        list(label = "time_id",
             levels = c("t2", "t3", "t4"))
    ),
    rmCells = list(
        list(measure = "t2", cell = "t2"),
        list(measure = "t3", cell = "t3"),
        list(measure = "t4", cell = "t4")
    ),
    bs = group_id,
    cov = t1,
    rmTerms = ~ time_id,
    bsTerms = ~ group_id + t1,
    effectSize = "ges",
    spherCorr = "GG",
    postHoc = list("time_id"), # Only significant test
    postHocCorr = "holm",
    emMeans = ~ time_id,
    emmTables = TRUE
)


#-------------------------------------------------
# STATISTICAL ANALYSES - EQUIVALENCE TESTS
#
# Determine effect size we had 33% power to detect using a independent
# samples t-test to be used as equivalence bounds
pwr::pwr.t.test(n = 50,
                sig.level = 0.05,
                power = 0.33,
                type = "two.sample",
                alternative = "two.side")

# Equivalence test for self-controlled vs traditional yoked in retention
TOSTER::TOSTtwo(m1 = 9.99, sd1 = 1.15, n1 = 50,
                m2 = 10.18, sd2 = 1.14, n2 = 50,
                low_eqbound_d = -0.31,
                high_eqbound_d = 0.31,
                alpha = 0.05,
                var.equal = FALSE)

# Equivalence test for traditional yoked vs explicit yoked in retention
TOSTER::TOSTtwo(m1 = 10.18, sd1 = 1.14, n1 = 50,
                m2 = 10.12, sd2 = 1.15, n2 = 50,
                low_eqbound_d = -0.31,
                high_eqbound_d = 0.31,
                alpha = 0.05,
                var.equal = FALSE)
