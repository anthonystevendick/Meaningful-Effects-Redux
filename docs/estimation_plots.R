#
# This is a reanalysis of Evanski JM, Zundel CG, Baglot SL, Desai S, Gowatch LC, Ely SL, et al. The First "Hit" to the Endocannabinoid 
# System? Associations Between Prenatal Cannabis Exposure and Frontolimbic White Matter Pathways in Children. Biol Psychiatry Glob Open Sci 2024;4(1):11–18
# It uses the ABCD 4.0 release.
#

#### Load libraries
library(mosaic)
library(ggplot2)
library(WRS2)
library(lm.beta)
library(gamm4)
library(rstatix)
library(jtools)
library(pbkrtest)
library(r2glmm)
library(dabestr)
library(lme4)
library(gghalves)
library(dplyr)
library(plyr)
library(forcats)

# Set working directory
setwd("your_directory")

#Load 4.0 data release
nda4_0<-readRDS("nda4.0.rds")

# Define a function for recoding and combining use flags
recode_prenatal_use <- function(data, var8, var9, newvar) {
  data[[var8]][data[[var8]] %in% c(555, 777, 999)] <- NA
  data[[var9]][data[[var9]] %in% c(555, 777, 999)] <- NA
  
  data[[newvar]] <- ifelse(data[[var8]] == 1 | data[[var9]] == 1, 1, 0)
  data[[newvar]][is.na(data[[var8]]) & is.na(data[[var9]])] <- NA
  
  return(data)
}

# Apply to each substance
nda4_0 <- recode_prenatal_use(nda4_0, "devhx_8_marijuana_p", "devhx_9_marijuana_p", "prenatal_cannabis_yn")
nda4_0 <- recode_prenatal_use(nda4_0, "devhx_8_tobacco_p",    "devhx_9_tobacco_p",    "prenatal_smoke_yn")
nda4_0 <- recode_prenatal_use(nda4_0, "devhx_8_alcohol_p",   "devhx_9_alcohol_p",   "prenatal_alcohol_yn")
nda4_0 <- recode_prenatal_use(nda4_0, "devhx_8_coc_crack_p", "devhx_9_coc_crack_p", "prenatal_cocaine_yn")

# Optional: check the results
summary(nda4_0[, c("prenatal_cannabis_yn", "prenatal_smoke_yn", "prenatal_alcohol_yn", "prenatal_cocaine_yn")])

# Frequency table including NA
table(nda4_0$prenatal_cannabis_yn, useNA = "always")

# Create a new binary race variable: 1 = White, 0 = Non-White
nda4_0$race_binary <- ifelse(nda4_0$race.4level == "White", 1, 0)

# Optional: label it as a factor for modeling or summary tables
nda4_0$race_binary <- factor(nda4_0$race_binary, levels = c(0, 1), labels = c("Non-White", "White"))

# Check the result
table(nda4_0$race_binary, useNA = "always")

# Recode pregnancy planned and premature birth: Set invalid codes to NA
nda4_0$devhx_6_pregnancy_planned_p[nda4_0$devhx_6_pregnancy_planned_p %in% c(555, 777, 999)] <- NA
nda4_0$devhx_12a_born_premature_p[nda4_0$devhx_12a_born_premature_p %in% c(555, 777, 999)] <- NA

# Subset the final analysis variables
analysis_vars <- c(
  "src_subject_id",
  "dmri_dti.fa_fiber.at_fx.rh",  # Right fornix FA
  "dmri_dti.fa_fiber.at_fx.lh",  # Left fornix FA
  "dmri_dti.fa_fiber.at_unc.lh", # Left uncinate FA
  "dmri_dti.fa_fiber.at_cgh.rh", #right parahippocampal cingulum FA
  "prenatal_cannabis_yn",
  "prenatal_smoke_yn",
  "prenatal_alcohol_yn",
  "prenatal_cocaine_yn",
  "age",     
  "female",       
  "race_binary",
  "high.educ",         # derived maternal education
  "household.income",
  "famhx_q6a_depression_p",
  "devhx_6_pregnancy_planned_p",
  "devhx_12a_born_premature_p",
  "site_id_l",
  "rel_family_id",
  "eventname"
)

# Create the subset data frame
analysis_df <- nda4_0[, analysis_vars]

# Filter to baseline rows only
analysis_df <- subset(analysis_df, eventname == "baseline_year_1_arm_1")

#saveRDS(analysis_df, "analysis_df.Rds")

# Load and start here once you do the above once

#analysis_df <- readRDS("analysis_df.Rds")

# Define the codes that should be treated as NA
na_codes <- c(555, 777, 999)

# Loop through each column in analysis_df and replace 555/777/999 with NA
analysis_df[] <- lapply(analysis_df, function(col) {
  if (is.numeric(col) || is.factor(col)) {
    col[col %in% na_codes] <- NA
  }
  return(col)
})


summary_table <- sapply(analysis_df, function(x) {
  c(
    N_total = length(x),
    N_NA = sum(is.na(x)),
    N_777 = sum(x == 777, na.rm = TRUE),
    N_999 = sum(x == 999, na.rm = TRUE),
    N_555 = sum(x == 555, na.rm = TRUE)
  )
})

summary_table <- as.data.frame(t(summary_table))
summary_table

library(lme4)

model_fx_rh <- lmer(
  dmri_dti.fa_fiber.at_fx.rh ~
    prenatal_cannabis_yn +
    prenatal_smoke_yn +
    prenatal_alcohol_yn +
    prenatal_cocaine_yn +
    age +
    female +
    race_binary +
    high.educ +
    household.income +
    famhx_q6a_depression_p +
    devhx_6_pregnancy_planned_p +
    devhx_12a_born_premature_p +
    (1 | site_id_l/rel_family_id),
  data = analysis_df,
  REML = FALSE
)

summary(model_fx_rh)

###
### Below we use a modification of the dabestr() package. I could not get the BCa bootstrap to run
### on the ABCD data (too many data points?) so I used percent bootstrap
### The dabestr() package is described here. Please cite accordingly.
### https://cran.r-project.org/web/packages/dabestr/index.html
### https://github.com/ACCLAB/dabestr
### Need to run the above bit to clear dabestr() defaults so that percent bootstrap can be run

# ─────────────────────────────────────────────────────────────────────────────
# STEP 0) Restart R so that no objects or packages are loaded.

# STEP 1) Load dabestr’s namespace (but do NOT attach it):
loadNamespace("dabestr")
ns <- asNamespace("dabestr")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2) Define helper “_flexible” functions in the global environment:

get_boot_row_flexible <- function(
  ctrl_measurement,
  test_measurement,
  effect_size_func,
  seed,
  reps,
  is_paired,
  control_group,
  test_group,
  ci,
  ci_type = c("perc", "bca")
) {
  control_test_measurement <- list(
    control = ctrl_measurement,
    test    = test_measurement
  )
  
  # Qualify var_w_df() inside dabestr
  ctrl_size <- length(ctrl_measurement)
  ctrl_var  <- dabestr:::var_w_df(ctrl_measurement, ctrl_size)
  
  test_size <- length(test_measurement)
  test_var  <- dabestr:::var_w_df(test_measurement, test_size)
  
  # Qualify calculate_group_variance()
  grp_var <- dabestr:::calculate_group_variance(
    ctrl_var = ctrl_var,
    ctrl_N   = ctrl_size,
    test_var = test_var,
    test_N   = test_size
  )
  weight <- 1 / grp_var

  set.seed(seed)
  # Qualify effsize_boot()
  boots <- dabestr:::effsize_boot(
    data             = control_test_measurement,
    effect_size_func = effect_size_func,
    reps             = reps,
    paired           = is_paired
  )

  bootci <- boot::boot.ci(
    boots,
    conf = ci / 100,
    type = ci_type
  )

  pct_ci_low  <- NA_real_; pct_ci_high <- NA_real_
  bca_ci_low  <- NA_real_; bca_ci_high <- NA_real_

  # look for "perc" instead of "percent":
  if ("perc" %in% ci_type) {
    pct_ci_low  <- bootci$percent[4]
    pct_ci_high <- bootci$percent[5]
  }
  
  # (leave the “bca” check as-is)
  if ("bca" %in% ci_type) {
    bca_ci_low  <- bootci$bca[4]
    bca_ci_high <- bootci$bca[5]
  }

  boot_row <- list(
    control_group = control_group,
    test_group    = test_group,
    bootstraps    = list(as.vector(boots$t)),
    nboots        = length(boots$t),
    bca_ci_low    = bca_ci_low,
    bca_ci_high   = bca_ci_high,
    pct_ci_low    = pct_ci_low,
    pct_ci_high   = pct_ci_high,
    ci            = ci,
    ci_type       = paste(ci_type, collapse = ","),
    difference    = boots$t0,
    weight        = weight
  )
  return(boot_row)
}

bootstrap_flexible <- function(
  dabest_obj,
  effect_size_func,
  seed       = 12345,
  reps       = 5000,
  boot_labs  = NULL,
  ci_type    = c("perc","bca")
) {
  boot_result             <- tibble::tibble()
  baseline_ec_boot_result <- tibble::tibble()
  raw_data                <- dabest_obj$raw_data
  idx                     <- dabest_obj$idx
  resamples               <- dabest_obj$resamples

  if (! (is.list(idx))) idx <- list(idx)

  enquo_x      <- dabest_obj$enquo_x
  enquo_y      <- dabest_obj$enquo_y
  ci           <- dabest_obj$ci
  paired       <- dabest_obj$paired
  is_paired    <- dabest_obj$is_paired
  is_colour    <- dabest_obj$is_colour
  proportional <- dabest_obj$proportional

  quoname_x <- rlang::as_name(enquo_x)
  quoname_y <- rlang::as_name(enquo_y)

  delta_x_labels <- list()
  delta_y_labels <- boot_labs

  minimeta <- dabest_obj$minimeta
  delta2   <- dabest_obj$delta2

  # Qualify check_params()
  dabestr:::check_params(is_paired, boot_labs, proportional, delta2, ci)

  for (group in idx) {
    group_length <- length(group)
    for (i in seq_len(group_length - 1)) {
      if ((! is_paired) || (paired == "baseline")) {
        control_group <- group[1]
      } else {
        control_group <- group[i]
      }
      test_group <- group[i + 1]

      ctrl_tibble  <- raw_data %>%
                      dplyr::filter( !!enquo_x == !!control_group )
      ctrl_measure <- ctrl_tibble[[ quoname_y ]]

      test_tibble  <- raw_data %>%
                      dplyr::filter( !!enquo_x == !!test_group )
      test_measure <- test_tibble[[ quoname_y ]]

      xlabels        <- paste(test_group, control_group, sep = "\nminus\n")
      delta_x_labels <- append(delta_x_labels, xlabels)

      boot_row <- get_boot_row_flexible(
        ctrl_measurement = ctrl_measure,
        test_measurement = test_measure,
        effect_size_func = effect_size_func,
        seed             = seed,
        reps             = reps,
        is_paired        = is_paired,
        control_group    = control_group,
        test_group       = test_group,
        ci               = ci,
        ci_type          = ci_type
      )

      boot_result <- dplyr::bind_rows(boot_result, boot_row)
    }
  }

  if (minimeta) {
    # Qualify boot_weighted_row()
    boot_last_row <- dabestr:::boot_weighted_row(boot_result = boot_result, ci)
    boot_result   <- dplyr::bind_rows(boot_result, boot_last_row)
  }

  if (delta2) {
    # Qualify boot_delta_delta()
    boot_last_row <- dabestr:::boot_delta_delta(boot_result = boot_result, ci)
    boot_result   <- dplyr::bind_rows(boot_result, boot_last_row)
  }

  for (group in idx) {
    control_group  <- group[1]
    test_group     <- control_group

    ctrl_tibble   <- raw_data %>%
                     dplyr::filter( !!enquo_x == !!control_group )
    ctrl_measure  <- ctrl_tibble[[ quoname_y ]]
    test_measure  <- ctrl_measure

    xlabels <- paste(test_group, control_group, sep = "\nminus\n")

    boot_row <- get_boot_row_flexible(
      ctrl_measurement = ctrl_measure,
      test_measurement = test_measure,
      effect_size_func = effect_size_func,
      seed             = seed,
      reps             = reps,
      is_paired        = is_paired,
      control_group    = control_group,
      test_group       = test_group,
      ci               = ci,
      ci_type          = ci_type
    )

    baseline_ec_boot_result <- dplyr::bind_rows(
      baseline_ec_boot_result,
      boot_row
    )
  }

  raw_y_labels <- ifelse(
    proportional,
    "proportion of success",
    "value"
  )

  out <- list(
    raw_data                 = raw_data,
    idx                      = idx,
    delta_x_labels           = delta_x_labels,
    delta_y_labels           = delta_y_labels,
    raw_y_labels             = raw_y_labels,
    is_paired                = is_paired,
    is_colour                = is_colour,
    paired                   = paired,
    resamples                = resamples,
    Ns                       = dabest_obj$Ns,
    control_summary          = dabest_obj$control_summary,
    test_summary             = dabest_obj$test_summary,
    ylim                     = dabest_obj$ylim,
    enquo_x                  = dabest_obj$enquo_x,
    enquo_y                  = dabest_obj$enquo_y,
    enquo_id_col             = dabest_obj$enquo_id_col,
    enquo_colour             = dabest_obj$enquo_colour,
    proportional             = proportional,
    minimeta                 = minimeta,
    delta2                   = dabest_obj$delta2,
    proportional_data        = dabest_obj$proportional_data,
    boot_result              = boot_result,
    baseline_ec_boot_result  = baseline_ec_boot_result
  )

  class(out) <- c("dabest_effectsize")
  return(out)
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3) Redefine mean_diff_flexible(), qualifying each internal helper:

mean_diff_flexible <- function(
  dabest_obj,
  perm_count = 5000,
  ci_type    = c("perc", "bca")
) {
  # Qualify check_dabest_object():
  dabestr:::check_dabest_object(dabest_obj)

  effect_size_type <- "mean_diff"
  is_paired        <- dabest_obj$is_paired
  reps             <- dabest_obj$resamples

  if (is_paired) {
    main_results <- dabestr:::bootstrap(
      dabest_obj,
      dabestr:::effect_size_mean_func,
      boot_labs = "Paired\nmean difference",
      reps      = reps,
      ci_type   = ci_type
    )
  } else {
    main_results <- dabestr:::bootstrap(
      dabest_obj,
      dabestr:::effect_size_mean_func,
      boot_labs = "Mean difference",
      reps      = reps,
      ci_type   = ci_type
    )
  }

  permtest_and_pvalues <- dabestr:::Pvalues_statistics(
    dabest_obj,
    ef_size_fn       = dabestr:::effect_size_mean_func,
    effect_size_type = effect_size_type,
    perm_count       = perm_count
  )

  output <- c(
    main_results,
    list(effect_size_type = effect_size_type),
    permtest_and_pvalues
  )
  class(output) <- c("dabest_effectsize")
  return(output)
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4) Overwrite the original bindings in the dabestr namespace:

unlockBinding("bootstrap", ns)
assign("bootstrap", bootstrap_flexible, envir = ns)
lockBinding("bootstrap", ns)

unlockBinding("mean_diff", ns)
assign("mean_diff", mean_diff_flexible, envir = ns)
lockBinding("mean_diff", ns)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 5) Finally, attach dabestr and verify:

library(dabestr)

args(mean_diff)
###

# 1. Recode the grouping variable as a factor with labels
analysis_df$prenatal_cannabis_yn_factor <- factor(
  analysis_df$prenatal_cannabis_yn,
  levels = c(0, 1),
  labels = c("No Cannabis", "Cannabis")
)

# 2. Subset to complete cases for the outcome and group# (Assuming `analysis_df` is your full data.frame.)

plot_df <- analysis_df %>%
  # 1. Drop any missing in the two key columns:
  filter(
    !is.na(prenatal_cannabis_yn_factor),
    !is.na(dmri_dti.fa_fiber.at_fx.rh)
  ) %>%
  # 2. NOW also drop any FA > 0.70. There appears to be one major outlier. This for plotting only,
  # and should probably have been dealt with in the analysis step.
  filter(dmri_dti.fa_fiber.at_fx.rh <= 0.70) %>%
  # 3. Convert the binary to a factor (so that levels = c("No Cannabis","Cannabis")):
  mutate(
    prenatal_cannabis_yn_factor = factor(
      prenatal_cannabis_yn,
      levels = c(0, 1),
      labels = c("No Cannabis", "Cannabis")
    )
  )

raw_data <- as.data.frame(plot_df)

# 3. Create the dabest object
dabest_obj <- load(
    data      = raw_data,
    x         = prenatal_cannabis_yn_factor,     # ← NO quotes
    y         = dmri_dti.fa_fiber.at_fx.rh,      # ← NO quotes
    idx       = list(c("No Cannabis", "Cannabis")),
    paired    = NULL,
    ci        = 95,
    resamples = 50000
)

# 4. Compute the mean difference with bootstrapping (default = 5000 resamples; but change for more precision!)
#dabest_result <- mean_diff(dabest_obj, perm_count = 50000)
#percent bootstrap used because BCa fails with large sample (known issue on stack exchange)
dabest_result_perc <- mean_diff(
   dabest_obj,
   perm_count = 50000,
   ci_type    = "perc"
 )


###SAVE FOR RELOADING LATER
#saveRDS(dabest_result_perc, file = "dabest_result_perc.rds")

library(dplyr)
library(plyr)

# Set working directory
dabest_result_perc <- readRDS("dabest_result_perc.rds")

source("custom_dabest_plot.R") # this was modified from a number of dabestr() packages for customization

# 1) Recode the “prenatal_cannabis_yn_factor” inside raw_data
dabest_result_perc$raw_data$prenatal_cannabis_yn_factor <-
  fct_recode(
    dabest_result_perc$raw_data$prenatal_cannabis_yn_factor,
    Unexposed = "No Cannabis",
    PCE       = "Cannabis"
  )

# 2) Recode the Ns grouped_df so that its factor also becomes Unexposed / PCE
#    (this ensures the “N = …” tick labels on the raw‐plot get updated)
dabest_result_perc$Ns <-
  dabest_result_perc$Ns %>%
  mutate(
    prenatal_cannabis_yn_factor = fct_recode(
      prenatal_cannabis_yn_factor,
      Unexposed = "No Cannabis",
      PCE       = "Cannabis"
    ),
    # Rebuild the “swarmticklabs” column so it now reads “Unexposed\nN = …” and “PCE\nN = …”
    swarmticklabs = paste0(prenatal_cannabis_yn_factor, "\nN = ", n)
  )

# 3) Recode the bootstrap tables’ control_group/test_group so any internal references get renamed
dabest_result_perc$boot_result <-
  dabest_result_perc$boot_result %>%
  mutate(
    control_group = recode(control_group, "No Cannabis" = "Unexposed", "Cannabis" = "PCE"),
    test_group    = recode(test_group,    "No Cannabis" = "Unexposed", "Cannabis" = "PCE")
  )

dabest_result_perc$baseline_ec_boot_result <-
  dabest_result_perc$baseline_ec_boot_result %>%
  mutate(
    control_group = recode(control_group, "No Cannabis" = "Unexposed", "Cannabis" = "PCE"),
    test_group    = recode(test_group,    "No Cannabis" = "Unexposed", "Cannabis" = "PCE")
  )

dabest_result_perc$permtest_pvals <-
  dabest_result_perc$permtest_pvals %>%
  mutate(
    control_group = recode(control_group, "No Cannabis" = "Unexposed", "Cannabis" = "PCE"),
    test_group    = recode(test_group,    "No Cannabis" = "Unexposed", "Cannabis" = "PCE")
  )

# 4) Overwrite idx so that the raw‐plot knows group 1 is “Unexposed” and group 2 is “PCE”
dabest_result_perc$idx <- list(c("Unexposed", "PCE"))

# 5) Overwrite the delta‐plot’s x‐tick label so it reads exactly “Unexposed – PCE”
dabest_result_perc$delta_x_labels <- list("PCE - Unexposed")

estimation_plot<-custom_dabest_plot(
    dabest_result_perc,
    float_contrast   = TRUE,
    contrast_label   = expression(paste("\u25B2 ","PCE - Unexposed")),
    show_legend      = FALSE,
    custom_palette   = "gryffindor",
    swarm_label      = "Right Fornix FA",
    raw_marker_alpha = 0.3,
    raw_marker_size  = 0.2,
    raw_marker_spread= 0.2,
    tufte_size       = 1,
    es_marker_size   = 2.5,
    es_line_size     = 1.2,
    contrast_x_text  = 16,
    contrast_y_text  = 16,
    swarm_x_text     = 16,
    swarm_y_text     = 16,
    swarm_ylim       = c(0.2, 0.65),
    contrast_ylim    = c(-0.01, 0.015),
    delta_text       = FALSE,
    show_zero_dot    = TRUE,
    contrast_bars    = FALSE,
    swarm_bars       = FALSE
)

ggsave(
    filename = "estimation_plot.png",
    plot     = estimation_plot,
    width    = 10,       # 6 inches wide
    height   = 6,       # 6 inches tall
    dpi      = 600
)

ggsave(
    filename = "estimation_plot.tiff",
    plot     = estimation_plot,
    width    = 10,
    height   = 6,
    dpi      = 600,
    compression = "lzw"  # you can choose another compression if desired
)


## ───────────────────────────────────────────────────────────────────────────────
# 1) Load required packages
# ───────────────────────────────────────────────────────────────────────────────
library(dabestr)   # must have custom_plot_delta() & custom_assign_plot_kwargs() already loaded
library(ggplot2)   # for coord_cartesian(), theme(), ggsave()

# (Assumes all of your custom_* functions—
#  custom_assign_plot_kwargs(), custom_plot_delta(), custom_apply_palette(), etc.—
#  are already defined in your environment.)

# ───────────────────────────────────────────────────────────────────────────────
# 2) Define a single “zoomed” set of plot‐kwargs
# ───────────────────────────────────────────────────────────────────────────────
plot_kwargs_zoom <- list(
  # (a) Label the Δ‐axis exactly as in your estimation plot, with a ▲ symbol:
  contrast_label       = expression(paste("\u25B2 ", "PCE - Unexposed")),

  # (b) Suppress dabestr’s default Δ‐distribution bars (we’ll draw our own):
  contrast_bars        = FALSE,

  # (c) Keep the “zero‐dot” marker on the left (optional):
  show_zero_dot        = TRUE,

  # (d) No mini‐meta / delta2 panels:
  show_mini_meta       = FALSE,
  show_delta2          = FALSE,

  # (e) Force a “gryffindor” palette so the half‐violin + triangle are black/grey:
  custom_palette       = "gryffindor",

  # (f) Tell dabestr not to draw any swarm bars (we only build Δ here):
  swarm_bars           = FALSE,
  params_swarm_bars    = list(color = NULL, alpha = 0.3),
  params_contrast_bars = list(color = NULL, alpha = 0.3),

  # (g) Disable any numeric Δ‐text annotation:
  delta_text           = FALSE,

  # (h) Turn ON delta_dots = TRUE so that a filled ▲ (shape 17) is drawn at the mean:
  delta_dots           = TRUE,
  params_delta_dots    = list(
    pch   = 17,    # filled triangle
    alpha = 0.5,   # transparency
    cex   = 2,     # not used by custom_plot_delta(), but harmless
    size  = 2,     # not used by custom_plot_delta(), but harmless
    side  = "right"
  ),

  # (i) Increase the triangle’s “size” (this overrides the default 0.5):
  es_marker_size      = 4,    # <— make the ▲ bigger (try 2, 3, 4 until satisfied)
  es_line_size        = 1.0,  # <— (optional) thickness of the vertical CI bar

  # (j) Placeholder for delta_text parameters (unused):
  params_delta_text    = list(
    color         = NULL,
    alpha         = 1,
    fontsize      = 10,
    ha            = "center",
    va            = "center",
    rotation      = 0,
    x_location    = "right",
    x_coordinates = NULL,
    y_coordinates = NULL,
    x_adjust      = 0
  )
)
# ───────────────────────────────────────────────────────────────────────────────
# 3) Merge “zoomed” kwargs into dabestr’s defaults
# ───────────────────────────────────────────────────────────────────────────────
full_kwargs_zoom <- custom_assign_plot_kwargs(
  dabest_result_perc,   # your existing dabest_effectsize object
  plot_kwargs_zoom
)

# ───────────────────────────────────────────────────────────────────────────────
# 4) Build the Δ‐plot (un‐zoomed) exactly once
# ───────────────────────────────────────────────────────────────────────────────
delta_list <- custom_plot_delta(
  dabest_effectsize_obj = dabest_result_perc,
  float_contrast        = TRUE,
  plot_kwargs           = full_kwargs_zoom
)
delta_plot <- delta_list$delta_plot

# ───────────────────────────────────────────────────────────────────────────────
# 5) Apply palette first (re‐assign), then add the exact same scale/theme layers
# ───────────────────────────────────────────────────────────────────────────────
# 5a) Reassign with the “gryffindor” palette:
delta_plot <- custom_apply_palette(
  delta_plot,
  full_kwargs_zoom$custom_palette
)

# 5b) Now add the x‐tick (with ▲), move y‐axis to the right, and match font sizes:
delta_plot <- delta_plot +
  scale_x_continuous(
    breaks = 2,
    labels = expression(paste("\u25B2 ", "PCE - Unexposed"))
  ) +
  # Force the y‐axis onto the right side, exactly as in the combined plot
  scale_y_continuous(position = "right") +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 16),
    axis.title.x = element_blank(),        # drop redundant x‐axis title if any
    axis.text.y  = element_text(size = 16),
    axis.title.y = element_text(size = 18)
  )

# ───────────────────────────────────────────────────────────────────────────────
# 6) Zoom the Δ‐axis to ±0.01 via coord_cartesian()
# ───────────────────────────────────────────────────────────────────────────────
delta_plot_zoomed <- delta_plot +
  coord_cartesian(ylim = c(-0.01, +0.005))

# ───────────────────────────────────────────────────────────────────────────────
# 7) Save out both PNG and TIFF at 600 dpi (6×6 inches here, adjust as needed)
# ───────────────────────────────────────────────────────────────────────────────
ggsave(
  filename = "delta_plot_zoomed_pm0.01.png",
  plot     = delta_plot_zoomed,
  width    = 6,
  height   = 6,
  dpi      = 600
)

ggsave(
  filename    = "delta_plot_zoomed_pm0.01.tiff",
  plot        = delta_plot_zoomed,
  width       = 6,
  height      = 6,
  dpi         = 600,
  compression = "lzw"
)


###
###
###
# Loading of the dataset
data(twogroup_data)

# Preparing the data to be plotted
dabest_obj <- load(non_proportional_data,
                   x = Group, y = Measurement,
                   idx = c("Control 1", "Test 1")
)
dabest_obj.mean_diff <- mean_diff(dabest_obj)

# Plotting an estimation plot
toy_plot <- dabest_plot(dabest_obj.mean_diff, TRUE, swarm_bars = FALSE, contrast_bars = FALSE)


ggsave(
  filename = "toy_plot.png",
  plot     = toy_plot,
  width    = 10,
  height   = 6,
  dpi      = 600
)

ggsave(
  filename    = "toy_plot.tiff",
  plot        = toy_plot,
  width       = 10,
  height      = 6,
  dpi         = 600,
  compression = "lzw"
)
