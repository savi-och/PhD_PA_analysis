# =====================================================================
# Plotting score=2 fractions alongside flow rates for both platforms
# =====================================================================
#install.packages("scales")
library(tidyverse)
library(readxl)   # swap for readr::read_csv if your source is a CSV
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(scales)
library(vipor)     # ships with ggbeeswarm; provides offsetX()
library(patchwork)

# ---------------------------------------------------------------------
# ---- 0. OPEN DECISIONS — set these two, everything else follows ----
# ---------------------------------------------------------------------
setwd ("C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA")

DATA_PATH <- "C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA\\Image_scoring_Spreadsheet.xlsx"  # <-- edit
DATA_PATH <- "C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 2_ResultsChapter1_Constriction\\Image_Scoring_Spreadsheet_withAdh.xlsx"  # <-- edit
SHEET     <- 1


PLATFORM_FILTER <- "Adhesion"   #  "PA" OR "Constriction" or "Adhesion" NULL
PLATFORMS_KEEP <- c("Constriction", "PA")   # <---- the two platforms to compare
# set to NULL later for all platforms side by side
MIN_ELIGIBLE_ROI <- 2          # keep slices with > 1 eligible ROI

# ============================================================
# Colocalisation: per-slice score fractions
# ============================================================

# ---- read + rename by position -----------------------------
# READ ME: if columns are added, keep them AFTER the renamed block
# or update the indices.
raw <- read_excel(DATA_PATH, sheet = SHEET)
names(raw)[1:18] <- c("slice_id", "platform","perfusion_mode", "seal_loss", "n_eligible_rois", "n_sections","mean_rois_section",
                      "n_0", "n_1", "n_2", "fr_n_0","fr_n_1","fr_n_2", "flow_rate","flow_rate_seal_loss", "comments","extrusion_barrier", "pressure_flow")

# ---- per-slice table (full, unfiltered = your QC base) -----
per_slice <- raw %>%
  mutate(
    n_eligible = n_0 + n_1 + n_2,     # or use your stored eligible count
    f0 = n_0 / n_eligible,
    f1 = n_1 / n_eligible,
    f2 = n_2 / n_eligible
  )

# sanity: per-slice fractions should sum to 1
 per_slice %>% mutate(chk = f0 + f1 + f2) %>% count(round(chk, 6))
 per_slice %>% mutate(chk1 = n_0 + n_1 + n_2, matches = chk1 == n_eligible_rois)%>%count(matches)
 
 summary_tbl <- per_slice %>%
   filter(n_eligible_rois >= MIN_ELIGIBLE_ROI) %>%
   { if (!is.null(PLATFORM_FILTER)) filter(., platform == PLATFORM_FILTER) else . } %>%
   group_by(platform) %>%
   summarise(
     n_slices    = n(),
     total_elig  = sum(n_eligible_rois),
     mean_f0     = mean(f0),
     mean_f1     = mean(f1),
     mean_f2     = mean(f2),
     median_f2   = median(f2),      # median as the robust companion to mean_f2
     .groups = "drop"
   ) %>%
   #mutate(across(starts_with("mean_"), ~ .x * 100))%>%
   mutate(check = mean_f0 + mean_f1 + mean_f2)   # should be ~1 per row
 
 per_slice %>%
   mutate(kept = n_eligible_rois >= MIN_ELIGIBLE_ROI) %>%
   count(platform, kept)   # slices kept vs dropped, per platform

# ---- plot 1 data: average of per-slice fractions -----------
plot1_data <- per_slice %>%
  filter(n_eligible >= MIN_ELIGIBLE_ROI) %>%               # figure-feeding filter
  { if (!is.null(PLATFORM_FILTER)) filter(., platform == PLATFORM_FILTER) else . } %>%
  group_by(platform) %>%
  summarise(across(c(f0, f1, f2), mean), .groups = "drop") %>%
  pivot_longer(c(f0, f1, f2), names_to = "score", values_to = "mean_fraction") %>%
  mutate(score = factor(score, levels = c("f2", "f1", "f0"),  
                        labels = c("2", "1", "0")))

# check the three segments still sum to ~1 per platform
 plot1_data %>% group_by(platform) %>% summarise(tot = sum(mean_fraction))

# ---- plot 1 ------------------------------------------------
greens <- c("0" = "#A9D18E", "1" = "#4EA72E", "2" = "#2E7D22")  # placeholder, tune to taste

p1 <- ggplot(plot1_data, aes(platform, mean_fraction, fill = score)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = percent(mean_fraction, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 6, colour = "white", fontface = "bold",
  )+
  ## ONLY FOR STATIC EXPLANT CONSTRICTION
  # geom_text(
  #   aes(label = if_else(mean_fraction >= 0.005,
  #                       percent(mean_fraction, accuracy = 1), "")),
  #   position = position_stack(vjust = 0.5),
  #   size = 6, colour = "white", fontface = "bold"
  # )+
  scale_fill_manual(values = greens, name = "Score") +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(expand=expansion(add=0.5))+
  labs(title = paste0("Per-slice averaged colocalisation score fraction"),x = NULL, y = "Mean per-slice fraction") +
  theme_minimal()+
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 14),   # tick labels
    axis.title = element_text(size = 16),    # axis titles
    axis.text.y = element_blank(),
    legend.text  = element_text(size = 11),
    legend.title = element_text(size = 12))

p1

####################################################################
#
# ---- Plot of individual slice fractions alongside flowrates -----
#
####################################################################

FLOOR_FLOW <- 10        # axis minimum, where NQ/zero gets pinned
CEIL_FLOW  <- 1e5       # 100000, clean decade above your 10400 max

# map a flow rate (log scale) into the 0..1 fraction band
to_frac <- function(flow) {
  (log10(flow) - log10(FLOOR_FLOW)) / (log10(CEIL_FLOW) - log10(FLOOR_FLOW))
}
# inverse, for the secondary axis labels
from_frac <- function(y) {
  10 ^ (y * (log10(CEIL_FLOW) - log10(FLOOR_FLOW)) + log10(FLOOR_FLOW))
}


flow_long <- per_slice %>%
  filter(platform == PLATFORM_FILTER, n_eligible_rois >= MIN_ELIGIBLE_ROI) %>%
  mutate(
    flow_main = if_else(flow_rate == 0 | is.na(flow_rate), FLOOR_FLOW, flow_rate),
    is_nq     = flow_rate == 0 | is.na(flow_rate),     # flag for styling
    flow_loss  = if_else(flow_rate_seal_loss == 0 | is.na(flow_rate_seal_loss),
                         NA_real_, flow_rate_seal_loss),   # NA where no seal loss
    y_main = to_frac(flow_main),
    y_loss = to_frac(flow_loss),
    slice_id = fct_reorder(slice_id, f2)
  )


p2 <- ggplot(flow_long, aes(x = reorder(slice_id, f2))) +
  geom_col(aes(y = f2), fill = "#A9D18E", width = 0.6) +
  geom_segment(aes(xend = slice_id, y = y_main, yend = y_loss),
               linewidth = 0.3, colour = "grey70") +
  geom_point(aes(y = y_main, colour = "Sealed"), size = 2.5) +
  geom_point(aes(y = y_loss, colour = "Unsealed"), size = 2.5, na.rm = TRUE) +
  scale_colour_manual(
    name = "Flow rate",
    values = c("Sealed" = "#E76BF3", "Unsealed" = "#56B4E9")
  )+
  ggrepel::geom_text_repel(
    data = ~ filter(.x, is_nq),
    aes(y = y_main, label = "Low\nNQ"),
    size = 2.5, colour = "grey20", fontface = "bold",
    nudge_x = 0.54, direction = "y",
    segment.colour = "grey50", segment.size = 0.3,
    min.segment.length = 0            # force the connector to always draw
  )+
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  geom_text(aes(y = -0.05, label = n_eligible_rois),
            size = 3, colour = "grey30", vjust = 0.5) +
  annotate("text", x = 0.4, y = -0.05, label = "No. of ROIs:",
           hjust = 1, size = 2.8, colour = "grey30", fontface = "bold")+
  scale_y_continuous(
    name = "ROI Fraction score = 2",
    #limits = c(0, 1),
    sec.axis = sec_axis(~ from_frac(.), name = "Flow rate (nL/min)",
                        breaks = c(10, 100, 1000, 10000, 100000),
                        labels = scales::label_log()),
  ) +
  labs(title = paste0("Perfused slice intravascular colocalisation fraction \u2014 ", PLATFORM_FILTER),
      x = "Slice ID (ascending % score = 2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1, size=7),
        axis.ticks.length.x = unit(2, "pt"),
        axis.title.y.left  = element_text(colour = "#2E7D22", size = 13, face = "bold"),
        axis.text.y.left  = element_text(colour = "#2E7D22", size=11),
        axis.title.y.right = element_text(colour = "#E76BF3", size=13, face = "bold"),
        axis.text.y.right  = element_text(colour = "#E76BF3", size=11),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 20))
p2

###### ---- p2.Specific for constriction platform ---- ######


# ---- verify these encodings against your sheet -------------
# pressure_flow: matched case-insensitively on "flow" / "pressure"
# extrusion_barrier: "ADH" = adhered
ADH_LABEL <- "ADH"
FLOOR_FLOW <- 10        # axis minimum, where NQ/zero gets pinned
CEIL_FLOW  <- 1e5       # 100000, clean decade above your 10400 max

# ---- one panel builder -------------------------------------
make_panel <- function(df, panel_title, show_left = FALSE, show_right = FALSE) {
  
  df <- df %>%
    filter(n_eligible_rois >= MIN_ELIGIBLE_ROI,
           mean_rois_section >= 1) %>%
    mutate(
      flow_main = if_else(flow_rate == 0 | is.na(flow_rate), FLOOR_FLOW, flow_rate),
      is_nq     = flow_rate == 0 | is.na(flow_rate),
      flow_loss = if_else(flow_rate_seal_loss == 0 | is.na(flow_rate_seal_loss),
                          NA_real_, flow_rate_seal_loss),
      y_main    = to_frac(flow_main),
      y_loss    = to_frac(flow_loss),
      slice_id  = fct_reorder(as.character(slice_id), f2)   # re-factor within subset
    )
  
  p <- ggplot(df, aes(x = slice_id)) +
    geom_col(aes(y = f2), fill = "#A9D18E", width = 0.6) +
    geom_segment(aes(xend = slice_id, y = y_main, yend = y_loss),
                 linewidth = 0.3, colour = "grey70") +
    geom_point(aes(y = y_main, colour = "Sealed"), size = 2.5) +
    geom_point(aes(y = y_loss, colour = "Perifusion"), size = 2.5, na.rm = TRUE) +
    scale_colour_manual(name = "Flow rate",
                        values = c("Sealed" = "#E76BF3", "Perifusion" = "#56B4E9")) +
    ggrepel::geom_text_repel(
      data = ~ filter(.x, is_nq),
      aes(y = y_main, label = "Low\nNQ"),
      size = 2.5, colour = "grey20", fontface = "bold",
      nudge_x = 0.54, direction = "y",
      segment.colour = "grey50", segment.size = 0.3,
      min.segment.length = 0
    ) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    geom_text(aes(y = -0.05, label = n_eligible_rois),
              size = 3, colour = "grey30", vjust = 0.5) +
    scale_y_continuous(
      name = if (show_left) "ROI Fraction score = 2" else NULL,
      sec.axis = sec_axis(~ from_frac(.),
                          name   = if (show_right) "Flow rate (nL/min)" else NULL,
                          breaks = c(10, 100, 1000, 10000, 100000),
                          labels = scales::label_log())
    ) +
    labs(title = panel_title, x = NULL) +
    theme_minimal() +
    theme(
      plot.title         = element_text(size = 10, face = "bold"),
      axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
      axis.ticks.length.x = unit(2, "pt"),
      axis.title.y.left  = element_text(colour = "#2E7D22", size = 12, face = "bold"),
      axis.text.y.left   = element_text(colour = "#2E7D22", size = 12),
      axis.title.y.right = element_text(colour = "#E76BF3", size = 12, face = "bold"),
      axis.text.y.right  = element_text(colour = "#E76BF3", size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3),
      plot.margin        = margin(t = 10, r = 5, b = 10, l = 5)
    )
  
  # ROI-row header only on the left-most panel
  if (show_left) {
    p <- p + annotate("text", x = 0.4, y = -0.05, label = "No. of ROIs:",
                      hjust = 1, size = 3, colour = "grey30", fontface = "bold")
  }
  # strip inner y-axis text/ticks so only outer axes show numbers
  if (!show_left)  p <- p + theme(axis.text.y.left  = element_blank(),
                                  axis.ticks.y.left  = element_blank())
  if (!show_right) p <- p + theme(axis.text.y.right = element_blank(),
                                  axis.ticks.y.right = element_blank())
  p
}

# ---- three subsets (fixed platform) ------------------------
# encoding: pressure_flow  P = pressure, Q = flow
base <- per_slice %>% filter(platform == PLATFORM_FILTER)

panel_flow    <- base %>% filter(pressure_flow == "Q")
panel_adhered <- base %>% filter(pressure_flow == "P",
                                 extrusion_barrier == ADH_LABEL)
panel_barrier <- base %>% filter(pressure_flow == "P",
                                 !(extrusion_barrier %in% ADH_LABEL))
# ---- build + assemble --------------------------------------
p_flow    <- make_panel(panel_flow,    "Flow-driven",              show_left = TRUE)
p_barrier <- make_panel(panel_barrier, "Pressure-driven")
p_adhered <- make_panel(panel_adhered, "Pressure-driven, \n adhered", show_right = TRUE)

w <- c(nrow(panel_flow), nrow(panel_barrier), nrow(panel_adhered))

divider <- ggplot() +
  annotate("segment", x = 0, xend = 0, y = 0, yend = 1, colour = "grey85", linewidth = 0.4) +
  theme_void()

p_combined <- (p_flow | divider | p_barrier| divider | p_adhered) +
  # plot_layout(widths = w, guides = "collect") +
  plot_layout(widths = c(nrow(panel_flow), 0.05,
                         nrow(panel_barrier), 0.05,
                         nrow(panel_adhered)),
              guides = "collect")+
  plot_annotation(title = paste0("Perfused slice intravascular colocalisation \u2014 ",
                                 PLATFORM_FILTER)) &
  theme(legend.position = "right")

p_combined

###################


###################
# ---- replicate level slice fraction across platforms ----
###################

# USE IF YOU WANT ALL PLATFORMS
# plot3_data <- per_slice %>%
#   filter(n_eligible_rois >= MIN_ELIGIBLE_ROI) %>%   # all platforms, same threshold as p1/p2
#   #filter(platform == PLATFORM_FILTER)   #<----ADD IF YOU WANT PER PLATFORM
#USE IF YOU WANT ONLY A SUBSET
plot3_data <- per_slice %>%
  filter(n_eligible_rois >= MIN_ELIGIBLE_ROI,
         platform %in% PLATFORMS_KEEP) %>%
  mutate(platform = factor(platform, levels = PLATFORMS_KEEP))

# medians computed upstream, one row per platform
plot3_medians <- plot3_data %>%
  group_by(platform) %>%
  summarise(med_f2 = median(f2), .groups = "drop")

p3 <- ggplot(plot3_data, aes(x = platform, y = f2)) +
  ggbeeswarm::geom_quasirandom(aes(size = n_eligible_rois),
                               width = 0.2, alpha = 0.7, colour = "#A9D18E") +
  # median from its own table, not stat_summary
  geom_crossbar(data = plot3_medians,
                aes(x = platform, y = med_f2, ymin = med_f2, ymax = med_f2),
                width = 0.5, linewidth = 0.4, colour = "grey50",
                inherit.aes = FALSE) +
  scale_size_area(breaks = c(5, 10, 20, 30, 50), max_size = 8, name = "No of ROIs \nper imaged slice") +
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = NULL, y = "ROI Fraction (-) score = 2") +
  theme_minimal() +
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank())

p3


# --- Colocalisation score fraction (imaging) --------
ggsave(paste0("colocalisation_Score_fraction_",PLATFORM_FILTER, ".jpeg"),
       plot = p1, device = "jpeg",
       width = 3, height = 4, dpi = 300)  #<---- CHANGE DEPENDING IF YOU PLOT SINGLE PLATFORM OR BOTH
# --- Per slice score=2 fraction (imaging) --------
ggsave(paste0("per_slice_score_2_fraction_Q_",PLATFORM_FILTER, ".jpeg"),
       plot = p2, device = "jpeg",
       width = 8, height = 6, dpi = 300)
# --- Slice fraction + median (imaging) --------
ggsave(paste0("score_2_fractions_median",PLATFORM_FILTER, ".jpeg"),
       plot = p3, device = "jpeg",
       width = 6, height = 4, dpi = 300)
# --- Per slice score=2 fraction (imaging) --------
ggsave(paste0("per_slice_score_2_fraction_Q_incl_Adh",PLATFORM_FILTER, ".jpeg"),
       plot = p_combined, device = "jpeg",
       width = 10, height = 6, dpi = 300)