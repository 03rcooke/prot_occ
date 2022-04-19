# title: est_plot_func
# description: function to produce estimation plots 
# see here for general idea https://www.graphpad.com/support/faq/creating-an-estimation-plot-of-the-results-of-an-unpaired-t-test/#:~:text=What%20are%20Estimation%20Plots%3F,P%2Dvalue%20(1).

est_plot_func <- function(grp, est_df, est_diff, vari, lim = NULL, axlab) {
  
  low_lim <- lim[[1]]
  upp_lim <- lim[[2]]
  
  vari_diff <- paste0(vari, "_diff")
  vari_sig <- paste0(vari, "_sig")
  
  df <- est_df %>% 
    dplyr::filter(grp == !!grp)
  
  diff_df <- est_diff[[1]] %>% 
    dplyr::filter(grp == !!grp)
  
  diff_av <- est_diff[[2]] %>% 
    dplyr::filter(grp == !!grp)
  
  numb <- GB_func_spp %>% 
    dplyr::filter(grp == !!grp)
  
  # relative effect size
  eff <- effsize::cohen.d(as.formula(paste0(vari, " ~ prot")), data = df, hedges.correction = TRUE)
  
  eff_text <- paste0(sprintf("%.1f", -eff$estimate), " [", sprintf("%.1f", -eff$conf.int[[2]]), ", ", sprintf("%.1f", -eff$conf.int[[1]]), "]")
  
  meds <- df %>% 
    dplyr::group_by(prot) %>% 
    dplyr::summarise_at(vars(vari), list(meds = ~median)) %>% 
    dplyr::mutate(diff = c(0, diff_av[ , paste0(vari_diff, "_median")][[1]])) %>% 
    dplyr::mutate(colr = c("#EE7733", "#0077BB"))
  
  est_plot <- ggplot(df, aes(x = prot, y = .data[[vari]])) +
    # median lines
    geom_hline(data = meds, aes(yintercept = meds, colour = prot), lty = 2, lwd = 1) +
    # jittered points
    geom_jitter(aes(colour = prot), alpha = 0.4)  +
    # scales
    scale_y_continuous(name = axlab, limits = c(low_lim + meds[1,2][[1]], upp_lim + meds[1,2][[1]])) +
    scale_colour_manual(values = c("#EE7733", "#0077BB")) +
    labs(x = "") +
    theme(legend.position = "none")
  
  est_diff_plot <- ggplot(diff_df, aes(x = "", y = .data[[vari_diff]])) +
    # median lines
    geom_hline(data = meds, aes(yintercept = diff), colour = meds$colr, lty = 2, lwd = 1) +
    # half violin
    gghalves::geom_half_violin(aes(fill = .data[[vari_sig]]), side = "r", colour = NA, scale = "count", alpha = 0.6) +
    # 95% CrI
    geom_linerange(data = diff_av, aes(ymax = .data[[paste0(vari_diff, "_upp_ci")]], ymin = .data[[paste0(vari_diff, "_low_ci")]], x = "", colour = .data[[vari_sig]]), size = 1, inherit.aes = FALSE) +
    # median
    geom_point(data = diff_av, aes(y = .data[[paste0(vari_diff, "_median")]], colour = .data[[vari_sig]]), size = 10, shape = "-") +
    # relative effect size
    annotate("text", x = 1, y = min(diff_df[vari_diff]), label = eff_text) +
    # scales
    scale_y_continuous(name = "Difference", position = "right", limits = c(low_lim, upp_lim)) +
    scale_fill_manual(values = c("#33BBEE", "grey50", "#009988"), drop = FALSE) +
    scale_colour_manual(values = c("#33BBEE", "grey50", "#009988"), drop = FALSE) +
    labs(x = "") +
    theme(legend.position = "none",
          axis.ticks.x = element_blank())
  
  est_prep_plot <- cowplot::plot_grid(est_plot, est_diff_plot, rel_widths = c(1, 0.8))
  
  return(list(est_prep_plot,
              meds,
              diff_av,
              eff,
              est_diff_plot))
  
}