##########################################################################
## Visualizations for Explicit yoked group project                      ##
## -- St. Germain, Williams, Balbaa, Poskus, Leshchyshen, Lohse, Carter ##
##                                                                      ##
## Created by Mike Carter                                               ##
##########################################################################


# SCRIPT SETUP ---------------
#
# Load everything from other R script file
source("scripts/wrangle.R")

# Required libraries
library(cowplot)

# Color and theme setup
color_theme <- c("#bf616a", "#5e81ac", "#d08770")

theme_set(
    theme_classic() +
        theme(
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12, face = "bold")
        )
)


# FIGURE 1 ---------------
#
# Figure 1a
fig1a <- ggplot2::ggplot(motor_trialxtrial_g_tib,
                         aes(x = trial_expt,
                             y = mean_mt,
                             group = interaction(group_id, phase_id),
                             color = group_id)) +
    geom_ribbon(aes(ymin = se_low,
                    ymax = se_upp,
                    fill = group_id),
                linetype = 0,
                alpha = 0.2) +
    geom_line(aes(linetype = group_id,
                  group = interaction(group_id, phase_id))) +
    scale_x_continuous(name = "Trial",
                       limits = c(1, 35),
                       breaks = seq(1, 35, 3)) +
    scale_y_continuous(name = "Mean stacking time (s)",
                       limits = c(5, 25),
                       breaks = seq(5, 25, 5)) +
    scale_linetype_manual(values = c(1, 3, 5),
                          labels = c("Self-Controlled",
                                     "Traditional Yoked",
                                     "Explicit Yoked")) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_fill_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    theme(
        legend.position = c(0.84, 0.88),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")
    ) +
    annotate(
        geom = "text", x = 3, y = 5, label = "Pre-test", size = 4,
        fontface = "bold"
    ) +
    annotate(
        geom = "text", x = 18, y = 5, label = "Acquisition", size = 4,
        fontface = "bold"
    ) +
    annotate(
        geom = "text", x = 33, y = 5, label = "Retention", size = 4,
        fontface = "bold"
    )
fig1a

# Figure 1b
fig1b <- ggplot2::ggplot(motor_7blocks_g_tib,
                         aes(x = block_id_expt,
                             y = mean_mt,
                             group = interaction(group_id, phase_id),
                             color = group_id,
                             shape = group_id)) +
    geom_line(aes(linetype = group_id,
                  group = interaction(group_id, phase_id)),
              position = position_dodge(0.5)) +
    geom_pointrange(aes(ymin = ci_low,
                        ymax = ci_upp),
                    size = 0.5,
                    position = position_dodge(0.5)) +
    scale_x_discrete(name = "Blocks of 5 trials",
                     labels = c("1" = "Pre",
                                "2" = "Acq 1",
                                "3" = "Acq 2",
                                "4" = "Acq 3",
                                "5" = "Acq 4",
                                "6" = "Acq 5",
                                "7" = "Ret")) +
    scale_y_continuous(name = "Mean stacking time (s)",
                       limits = c(8, 16),
                       breaks = seq(8, 16, 2)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_linetype_manual(values = c(1, 3, 5),
                          labels = c("Self-Controlled",
                                     "Traditional Yoked",
                                     "Explicit Yoked")) +
    theme(
        legend.position = c(0.84, 0.88),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")
    )# +
    #guides(color = guide_legend(nrow = 1))
fig1b

# Figure 1c
# Create a tribble of adjusted retention means from script_stats.R
motor_adj_means_g_tib <- tibble::tribble(
    ~group_id, ~marg_mean, ~ci_low, ~ci_upp, # header row
    #---------------------------------------
    1, 9.994766, 9.67648, 10.31305,
    2, 10.177894, 9.859608, 10.49618,
    3, 10.122785, 9.804499, 10.44107
)

# Make group_id a factor
motor_adj_means_g_tib <- motor_adj_means_g_tib %>%
    dplyr::mutate(
        group_id = forcats::as_factor(group_id)
    )

fig1c <- ggplot2::ggplot(motor_adj_means_g_tib,
                         aes(x = group_id,
                             y = marg_mean,
                             color = group_id,
                             shape = group_id)) +
    geom_errorbar(aes(ymin = ci_low,
                      ymax = ci_upp),
                  width = .1) +
    geom_point(size = 3) +
    scale_x_discrete(name = "Group",
                     labels = c("1" = "Self-Controlled",
                                "2" = "Traditional Yoked",
                                "3" = "Explicit Yoked")) +
    scale_y_continuous(name = "Adjusted mean stacking \ntime (s) in Retention",
                       limits = c(9, 11),
                       breaks = seq(9, 11, 0.5)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    theme(
        legend.position = "none"
    )
fig1c

# Figure 1d
# Select relevant columns for scatterplot of pre-test and retention
scatter_tib <- motor_pre_ret_p_tib %>%
    dplyr::select(id, group_id, phase_id, mean_mt) %>%
    tidyr::pivot_wider(names_from = phase_id, values_from = mean_mt) %>%
    dplyr::rename(pre = 3) %>% # specifying using column number
    dplyr::rename(ret = 4) # specifying using column number

fig1d <- ggplot2::ggplot(scatter_tib,
                         aes(x = pre, y = ret,
                             color = group_id,
                             shape = group_id)) +
    geom_point(size = 2) +
    scale_x_continuous(name = "Pre-test stacking time (s)",
                       limits = c(5, 30),
                       breaks = seq(5, 30, 5)) +
    scale_y_continuous(name = "Retention stacking time (s)",
                       limits = c(5, 30),
                       breaks = seq(5, 30, 5)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    theme(
        legend.position = c(0.84, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted")
fig1d


# FIGURE 1 PANELSET ---------------
#
fig1_panel <- cowplot::ggdraw() +
    draw_plot(fig1a, x = 0, y = 0.5, width = 0.5, height = 0.5) +
    draw_plot(fig1b, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
    draw_plot(fig1c, x = 0, y = 0, width = 0.5, height = 0.5) +
    draw_plot(fig1d, x = 0.5, y = 0, width = 0.5, height = 0.5) +
    draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                    x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5))
fig1_panel


# FIGURE 2 ---------------
#
# Figure 2a
fig2a <- ggplot2::ggplot(qaire_pa_g_tib,
                         aes(x = time_id,
                             y = mean_rating,
                             color = group_id,
                             shape = group_id)) +
    geom_pointrange(aes(ymin = ci_low,
                        ymax = ci_upp),
                    size = 0.75,
                    position = position_dodge(1)) +
    geom_point(data = qaire_pa_p_tib, aes(x = time_id,
                                          y = score,
                                          color = group_id,
                                          shape = group_id),
               alpha = 0.3, position = position_jitterdodge(dodge.width = 1)) +
    scale_x_discrete(name = "",
                     labels = c("1" = "After Pre-test",
                                "2" = "After Trial 5",
                                "3" = "After Trial 25",
                                "4" = "Before Retention")) +
    scale_y_continuous(name = "Perceived autonomy",
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5),
               linetype = "dotted", alpha = 0.8) +
    theme(
        legend.position = "none"
    )
fig2a

# Figure 2b
fig2b <- ggplot2::ggplot(qaire_im_g_tib,
                         aes(x = time_id,
                             y = mean_rating,
                             color = group_id,
                             shape = group_id)) +
    geom_pointrange(aes(ymin = ci_low,
                        ymax = ci_upp),
                    size = 0.75,
                    position = position_dodge(1)) +
    geom_point(data = qaire_im_p_tib, aes(x = time_id,
                                          y = score,
                                          color = group_id,
                                          shape = group_id),
               alpha = 0.3, position = position_jitterdodge(dodge.width = 1)) +
    scale_x_discrete(name = NULL,
                     labels = c("1" = "After Pre-test",
                                "2" = "After Trial 5",
                                "3" = "After Trial 25",
                                "4" = "Before Retention")) +
    scale_y_continuous(name = "Intrinsic motivation",
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5),
               linetype = "dotted", alpha = 0.8) +
    theme(
        legend.position = "none"
    )
fig2b

# Figure 2c
fig2c <- ggplot2::ggplot(qaire_pc_g_tib,
                         aes(x = time_id,
                             y = mean_rating,
                             color = group_id,
                             shape = group_id)) +
    geom_pointrange(aes(ymin = ci_low,
                        ymax = ci_upp),
                    size = 0.75,
                    position = position_dodge(1)) +
    geom_point(data = qaire_pc_p_tib, aes(x = time_id,
                                          y = score,
                                          color = group_id,
                                          shape = group_id),
               alpha = 0.3, position = position_jitterdodge(dodge.width = 1)) +
    scale_x_discrete(name = NULL,
                     labels = c("1" = "After Pre-test",
                                "2" = "After Trial 5",
                                "3" = "After Trial 25",
                                "4" = "Before Retention")) +
    scale_y_continuous(name = "Perceived competence",
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1)) +
    scale_color_manual(values = color_theme,
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    scale_shape_manual(values = c(16, 17, 15),
                       labels = c("Self-Controlled",
                                  "Traditional Yoked",
                                  "Explicit Yoked")) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5),
               linetype = "dotted", alpha = 0.8) +
    theme(
        legend.position = "none"
    )
fig2c




# FIGURE 2 PANELSET ---------------
#
# fig2_panel <- cowplot::plot_grid(
#     fig2a + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
#     fig2b + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
#     fig2c + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
#     align = "vh",
#     labels = c("A", "B", "C"),
#     nrow = 1
# )

fig2_panel <- cowplot::plot_grid(
    fig2a + theme(axis.text.x = element_blank()),
    fig2b + theme(axis.text.x = element_blank()),
    fig2c,
    align = "vh",
    labels = c("A", "B", "C"),
    ncol = 1
)

shared_legend <- cowplot::get_legend(
    fig2a +
        ggplot2::guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.text = element_text(size = 11, face = "bold"))
)

# Add shared legend above panel figure
cowplot::plot_grid(shared_legend, fig2_panel,
                   ncol = 1, rel_heights = c(.1, 1.5))


# SAVE FIGURES AS .TIFF ---------------
#
# Fig 1 panel
cowplot::ggsave2("fig1.tiff",
                 plot = fig1_panel,
                 device = "tiff",
                 path = "figs/",
                 width = 279,
                 height = 216,
                 units = "mm",
                 dpi = 300)

# Fig 2 panel
cowplot::ggsave2("fig2.tiff",
                 plot = fig2_panel,
                 device = "tiff",
                 path = "figs/",
                 width = 216,
                 height = 279,
                 units = "mm",
                 dpi = 300)
