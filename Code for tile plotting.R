# =====================================================================
# Seal-outcome tile plot — data cleaning, classification & cell counts
# Gate 2: sealed subset only (Seal == "Y"), outcome = Sealing lost (Y/N)
#
# This script ONLY cleans + classifies + reports counts/flags.
# The ggplot tile figure is the NEXT step, once the counts look sane.
#
# NOTE: untested against the full sheet (built from a 9-row snippet).
#       Verify the rename block matches your actual column order.
# =====================================================================
#install.packages("scales")
library(tidyverse)
library(readxl)   # swap for readr::read_csv if your source is a CSV
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(scales)
library(vipor)     # ships with ggbeeswarm; provides offsetX()

# ---------------------------------------------------------------------
# ---- 0. OPEN DECISIONS — set these two, everything else follows ----
# ---------------------------------------------------------------------
setwd ("C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA")
# Device grouping (confirmed): V2 | V9 | {V5,V7} | V10(γ) | {all V13.x}
# -> 5 device levels. Encoded in group_version() below.

# Pressure band edges. TRUE => [0,40] (40,80] (>80]  (40=low, 80=intermediate)
#                          FALSE => [0,40) [40,80) [>=80]  (40=int, 80=high)
PRESSURE_RIGHT_INCLUSIVE <- TRUE

# Pressure band cutpoints (mbar, magnitude)
PRESSURE_BREAKS <- c(-Inf, 40, 80, Inf)
PRESSURE_LABELS <- c("low", "intermediate", "high")

DATA_PATH <- "C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 3_ResultsChapter2_PA\\Sealing_spreadshit.xlsx"  # <-- edit
SHEET     <- 1


TISSUE_FILTER <- "4434"   # or "Liver" — set, then re-run
FLOWRATE_EXCLUDE <- "EXP-IA"   # ID (or prefix) excluded from flowrate analysis only, "EXP-IA"
                          # inactivate if you want to include all data including isras

DEVICE_COLOURS <- c(
  "V2"    = "#F8766D",   # the default hues, now locked to names
  "V9"    = "#A3A500",
  "V5/V7" = "#00BF7D",
  "V10"   = "#00B0F6",
  "V13.x" = "#E76BF3"
)

# ---------------------------------------------------------------------
# ---- 1. READ + RENAME (assumes column order A..L as in the snippet — verify) -----
# ---------------------------------------------------------------------
raw <- read_excel(DATA_PATH, sheet = SHEET)

# Rename by position so the messy header text doesn't bite. Check this maps
# correctly against your file before trusting the output.

# READ ME! MAKE SURE IF ANY COLUM  IS ADDED IS ADDED AFTER COLUMN 12 - IF NEEDS TO BE INCLUDED EDIT THE COLUMN INDEC [1:12]

names(raw)[1:16] <- c(
  "experimental_id", "run_no", "version", "tissue_type",
  "thickness_um", "diameter_mm", "seal", "sealing_lost",
  "asp_pressure", "cross_flow_pressure", "perfusion_mode", 
  "v_based_flowrate","mean_measured_flowrate","measurement_pressure",
  "duration_raw", "clamp_leak"
)

# Your manual short/longer column must be readable as `duration_bin`.
# If Excel named it something else, either rename it there or add it to the
# vector above (e.g. append "duration_bin" as element 13).
stopifnot("duration_bin" %in% names(raw))

# ---------------------------------------------------------------------
# ---- 2. HELPERS -----
# ---------------------------------------------------------------------

group_version <- function(v) {
  vt <- str_trim(v)
  case_when(
    str_starts(vt, "V13")           ~ "V13.x",   # all V13.* variants collapse
    str_starts(vt, "V10")           ~ "V10",     # catches V10 and V10γ
    vt %in% c("V5", "V7")           ~ "V5/V7",
    vt == "V2"                      ~ "V2",
    vt == "V9"                      ~ "V9",
    TRUE                            ~ NA_character_   # anything else -> flagged
  )
}

# Fixed device order for the x-axis / factor levels
DEVICE_LEVELS <- c("V2", "V9", "V5/V7", "V10", "V13.x")

# ---------------------------------------------------------------------
# ---- 3. CLEAN + CLASSIFY ----
#    all_explants = every explant, fully classified, ALL tissues
#                   (this is what the section-4 reports read from)
#    clean        = sealed subset only, ALL tissues
#    Tissue filter is applied LATER, only on the figure-feeding objects,
#    so data-quality reports stay whole-sheet while figures are per-tissue.
# ---------------------------------------------------------------------
all_explants <- raw %>%
  mutate(
    seal         = str_trim(seal),
    sealing_lost = str_trim(sealing_lost),
    explant_id   = paste(experimental_id, run_no, sep = "_"),
    
    # ---- pressure sign (row facet) from perfusion mode ----
    pressure_sign = case_when(
      str_detect(str_to_lower(perfusion_mode), "^negative") ~ "Negative pressure",
      str_detect(str_to_lower(perfusion_mode), "^positive") ~ "Positive pressure",
      TRUE ~ NA_character_
    ),
    
    # ---- pressure magnitude (gradient) — keeps only numeric part ----
    pressure_mag             = as.numeric(str_extract(cross_flow_pressure,  "[0-9]+\\.?[0-9]*")),
    measurement_pressure_num = as.numeric(str_extract(measurement_pressure, "[0-9]+\\.?[0-9]*")),
    pressure_band = cut(
      pressure_mag,
      breaks = PRESSURE_BREAKS, labels = PRESSURE_LABELS,
      right = PRESSURE_RIGHT_INCLUSIVE
    ),
    
    # ---- device ----
    device = factor(group_version(version), levels = DEVICE_LEVELS),
    
    # ---- duration (manual short/longer column) ----
    duration_bin = str_to_lower(str_trim(duration_bin)),
    duration_bin = if_else(duration_bin %in% c("short", "longer"),
                           duration_bin, NA_character_)
  )

#### gate-2 subset: only explants that formed a seal (still ALL tissues)####
clean <- all_explants %>%
  filter(seal == "Y")

# ---------------------------------------------------------------------
# ---- 4. FLAG / REPORT (whole-sheet: reads from all_explants, ALL tissues) ----
# ---------------------------------------------------------------------
# Exclusion flags — computed on the sealed subset (gate-2 relevance)
flagged <- clean %>%
  mutate(flag = case_when(
    is.na(pressure_sign)             ~ "no_pressure_sign",
    is.na(pressure_mag)              ~ "missing_pressure_magnitude",
    is.na(pressure_band)             ~ "pressure_band_NA",
    is.na(duration_bin)              ~ "duration_bin_not_short/longer",
    is.na(device)                    ~ "unknown_version",
    !sealing_lost %in% c("Y", "N")   ~ "sealing_lost_not_Y/N",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(flag)) %>%
  select(explant_id, tissue_type, version, device, perfusion_mode, pressure_sign,
         cross_flow_pressure, pressure_mag, pressure_band,
         duration_bin, sealing_lost, flag)

cat("\n=== FLAGGED RECORDS (review; excluded from counts) ===\n")
if (nrow(flagged) == 0) cat("none\n") else print(flagged, n = Inf)

# Pressure cross-check: computed pressure_mag vs measurement_pressure_num
pressure_check <- clean %>%
  mutate(pressure_discrepancy = pressure_mag != measurement_pressure_num) %>%
  filter(pressure_discrepancy %in% TRUE) %>%   # strictly TRUE; NA/FALSE excluded
  select(explant_id, tissue_type, cross_flow_pressure, pressure_mag,
         measurement_pressure, measurement_pressure_num)

cat("\n=== PRESSURE CROSS-CHECK (mag vs measurement differ) ===\n")
if (nrow(pressure_check) == 0) cat("none\n") else print(pressure_check, n = Inf)

# Blank measurement pressure list
missing_measurement <- clean %>%
  filter(is.na(measurement_pressure_num)) %>%
  select(explant_id, tissue_type, cross_flow_pressure, pressure_mag, measurement_pressure)

cat("\n=== MISSING MEASUREMENT PRESSURE ===\n")
if (nrow(missing_measurement) == 0) cat("none\n") else print(missing_measurement, n = Inf)

# ---------------------------------------------------------------------
# ---- 5. ANALYSIS SET (figures) — TISSUE FILTER APPLIED HERE ONLY ----
# ---------------------------------------------------------------------
plot_ready <- clean %>%
  filter(
    !is.na(pressure_sign), !is.na(pressure_band),
    !is.na(duration_bin), !is.na(device),
    sealing_lost %in% c("Y", "N")
  ) %>%
  filter(tissue_type == TISSUE_FILTER)   # <-- figures show one tissue

counts <- plot_ready %>%
  group_by(pressure_sign, duration_bin, device) %>%
  summarise(
    total = n(),
    lost  = sum(sealing_lost == "Y"),
    held  = sum(sealing_lost == "N"),
    label = paste0(lost, "/", total),
    .groups = "drop"
  ) %>%
  complete(
    pressure_sign = c("Positive pressure", "Negative pressure"),
    duration_bin  = c("short", "longer"),
    device        = DEVICE_LEVELS,
    fill = list(total = 0, lost = 0, held = 0)
  ) %>%
  mutate(label = if_else(total == 0, "no data", paste0(lost, "/", total)))

cat("\n=== CELL COUNTS (lost / total per device column) — tissue:", TISSUE_FILTER, "===\n")
print(counts %>% arrange(pressure_sign, duration_bin, device), n = Inf)

cat("\n=== PANEL SUMMARY (empty cells flagged) ===\n")
counts %>%
  group_by(pressure_sign, duration_bin) %>%
  summarise(n_devices = n(),
            empty_cells = sum(total == 0),
            explants = sum(total), .groups = "drop") %>%
  print(n = Inf)

# `plot_ready` (one row per sealed explant, with pressure_sign, duration_bin,
# device, pressure_band, sealing_lost) is what feeds the geom_tile step next.

##### TILE Plotting####

tiles <- plot_ready %>%
  mutate(device = factor(device, levels = DEVICE_LEVELS)) %>%   
  mutate(
    outcome = if_else(sealing_lost == "Y", "lost", "held"),
    outcome = factor(outcome, levels = c("held", "lost")), # held = level 1
    pressure_sign = factor(pressure_sign, levels = c("Negative pressure", "Positive pressure")),
    #duration_bin  = factor(duration_bin,  levels = c("short", "longer")),
    fill_key = case_when(
      outcome == "held" ~ "held",
      outcome == "lost" ~ paste("lost", pressure_band),   # append the band
      TRUE ~ NA_character_
    ),
    fill_key = factor(fill_key, levels = c(
      "held", "lost low", "lost intermediate", "lost high"
    ))
  ) %>%
  group_by(pressure_sign, device) %>%          # which three columns define one stacking column? | pressure_sign, duration_bin, device
  arrange(outcome, .by_group = TRUE) %>%   # what do we sort by so held lands at the bottom?
  mutate(idx = row_number()) %>%
  ungroup()
  
labels_df <- tiles %>%
  count(pressure_sign, device, outcome) %>%   # rows per outcome | pressure_sign, duration_bin, device, outcome
  group_by(pressure_sign, device) %>%     # | pressure_sign, duration_bin, device
  summarise(
    total = sum(n),
    lost  = sum(n[outcome == "lost"]),
    .groups = "drop"
  )
labels_df <- labels_df %>%
  tidyr::complete(
    pressure_sign,  device,    #duration_bin,
    fill = list(total = 0, lost = 0)
  ) %>%
  mutate(
    label = if_else(total == 0, "NA", paste0(lost, "/", total))
  )

lost_dur <- tiles %>%
  filter(outcome == "lost") %>%
  mutate(dur_short = dplyr::recode(duration_bin,
                                   short = "short", longer = "long"))

# short  = "\u25CB",   # ○ hollow circle
# longer = "\u25CF"))  # ● filled circle

######### Gate 2. PLOT BY PRESSURE (ADD DURATION BACK) ###############

p_by_pressure <- ggplot(tiles, aes(x = device, y = idx, fill=fill_key)) +
   geom_tile(width = 0.9, height = 0.9, colour = "white", linewidth = 0.5)+
  facet_grid(~pressure_sign )+  # pressure_sign~ duration_bin
  scale_fill_manual(
    name   = "Seal outcome",
    values = c(
      "held"              = "#457B9D",
      "lost low"          = "#F4A5A5",   # lightest red
      "lost intermediate" = "#E63946",   # mid red
      "lost high"         = "#9D1D28"    # darkest red
    ),
    labels = c(
      "held"              = "Sealing preserved",
      "lost low"          = "Lost — low pressure",
      "lost intermediate" = "Lost — mid pressure",
      "lost high"         = "Lost — high pressure"
    )
  )+
  geom_text(
    data = labels_df,
    aes(x = device, y = total+2, label = label),
    inherit.aes = FALSE,
    vjust = 1, size = 3
  )+
  geom_text(
    data = lost_dur,
    aes(x = device, y = idx, label = dur_short),
    inherit.aes = FALSE,
    size = 2.3, colour = "white", fontface = "bold"
  ) +
  scale_x_discrete(limits = DEVICE_LEVELS) +
  labs(
    title = "Seal outcomes by device and pressure",
    x = "Device versions", y = NULL
  ) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   margin = margin(6, 6, 6, 6)),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x     = element_text(face = "bold"),
    panel.grid      = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )
p_by_pressure

# =====================================================================
# ---- GATE 1 — Seal / no-seal figure (all perfused explants) ----
# Self-contained: builds all_explants from raw, then the plot.
# Requires that `raw`, group_version(), DEVICE_LEVELS, PRESSURE_BREAKS,
# PRESSURE_LABELS and PRESSURE_RIGHT_INCLUSIVE already exist in the
# session (they come from the top of the cleaning script).
# =====================================================================

# # ---- 1. Full classified set (every explant, NO seal filter) ----------
# all_explants <- raw %>%
#   mutate(
#     seal         = str_trim(seal),
#     sealing_lost = str_trim(sealing_lost),
#     explant_id   = paste(experimental_id, run_no, sep = "_"),
#     
#     pressure_sign = case_when(
#       str_detect(str_to_lower(perfusion_mode), "^negative") ~ "Negative pressure",
#       str_detect(str_to_lower(perfusion_mode), "^positive") ~ "Positive pressure",
#       TRUE ~ NA_character_
#     ),
#     
#     pressure_mag = as.numeric(str_extract(cross_flow_pressure, "[0-9]+\\.?[0-9]*")),
#     pressure_band = cut(
#       pressure_mag,
#       breaks = PRESSURE_BREAKS, labels = PRESSURE_LABELS,
#       right = PRESSURE_RIGHT_INCLUSIVE
#     ),
#     
#     device = factor(group_version(version), levels = DEVICE_LEVELS),
#     
#     duration_bin = str_to_lower(str_trim(duration_bin)),
#     duration_bin = if_else(duration_bin %in% c("short", "longer"),
#                            duration_bin, NA_character_)
#   )

# ---- 2. Gate-1 tiles: keep only explants with a known seal outcome ---
tiles_seal <- all_explants %>%
  filter(!is.na(seal), seal %in% c("Y", "N")) %>%
  filter(tissue_type == TISSUE_FILTER) %>%  
  mutate(
    seal_outcome  = if_else(seal == "Y", "sealed", "no seal"),
    seal_outcome  = factor(seal_outcome, levels = c("sealed", "no seal")),
    pressure_sign = factor(pressure_sign,
                           levels = c("Negative pressure", "Positive pressure"))
  ) %>%
  group_by(pressure_sign, device) %>%
  arrange(seal_outcome, .by_group = TRUE) %>%
  mutate(idx = row_number()) %>%
  ungroup()

# ---- 3. Column labels: no-seal / total, empties as "no data" ---------
labels_df_seal <- tiles_seal %>%
  count(pressure_sign, device, seal_outcome) %>%
  group_by(pressure_sign, device) %>%
  summarise(
    total   = sum(n),
    no_seal = sum(n[seal_outcome == "no seal"]),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    pressure_sign, device,
    fill = list(total = 0, no_seal = 0)
  ) %>%
  mutate(
    label = if_else(total == 0, "NA", paste0(no_seal, "/", total))
  )

# ---- 4. Plot ---------------------------------------------------------
p_by_seal <- ggplot(tiles_seal, aes(x = device, y = idx, fill = seal_outcome)) + 
  geom_tile(width = 0.9, height = 0.9, colour = "white", linewidth = 0.5) +
  facet_grid(~ pressure_sign) +                                                 
  scale_fill_manual(
    name   = "Seal outcome",
    values = c("sealed" = "#457B9D", "no seal" = "#E63946"),
    labels = c("sealed" = "Sealed", "no seal" = "Not sealed")
  ) +
  geom_text(
    data = labels_df_seal,
    aes(x = device, y = total + 2, label = label),
    inherit.aes = FALSE,
    vjust = 1, size = 3
  ) +
  scale_x_discrete(limits = DEVICE_LEVELS) +
  labs(
    title = "Seal formation by device and pressure",
    x = "Device versions", y = NULL
  ) +
  theme(
    plot.title          = element_text(face = "bold", hjust = 0.5,
                                       margin = margin(6, 6, 6, 6)),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x      = element_text(face = "bold"),
    panel.grid       = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank()
  )
p_by_seal

# =====================================================================
#  ---- Companion outputs for thickness / diameter metadata ----
#   (1) failure_table  — shared lookup table (negative pressure failures)
#   (2) p_geometry     — personal-use scatter of geometry vs outcome
# Requires all_explants (full classified set). Adjust filters as needed.
# =====================================================================

# ---------------------------------------------------------------------
# (1) LOOKUP TABLE — one row per failed explant at negative pressure
#     "failed" = did not seal (gate 1) OR sealed then lost (gate 2)
# ---------------------------------------------------------------------
failure_table <- all_explants %>%
  filter(pressure_sign == "Negative pressure") %>%                 # <------ Change between pressure modes
  filter(
    seal == "N" |                       # never sealed
      (seal == "Y" & sealing_lost == "Y") # sealed then lost
  ) %>%
  mutate(
    outcome = case_when(
      seal == "N"                       ~ "no seal",
      seal == "Y" & sealing_lost == "Y" ~ "lost",
      TRUE ~ NA_character_
    ),
    # pressure_band is only meaningful for lost (sealed) explants
    pressure_band = if_else(outcome == "lost", as.character(pressure_band), NA_character_)
  ) %>%
  transmute(
    explant_id,
    device,
    outcome,
    thickness_um,
    diameter_mm,
    pressure_band
  ) %>%
  arrange(outcome, thickness_um, diameter_mm)

print(failure_table, n = Inf)

# Optional: write it out for sharing (uncomment to use)
# readr::write_csv(failure_table, "negative_pressure_failures.csv")


# ---------------------------------------------------------------------
# ---- Plot. (2) Does geometry relate to outcome? ----
#     diameter is only 6/7/8 (treat as discrete), thickness is the spread.
#     x = outcome, y = thickness, shape/facet = diameter, jittered points.
# ---------------------------------------------------------------------

geom_df <- all_explants %>%
  filter(pressure_sign == "Negative pressure",                 #<--- CHANGE BETWEEN PRESSURE MODES !!!!!
         !is.na(seal), seal %in% c("Y", "N")) %>%
  filter(tissue_type == TISSUE_FILTER) %>%
  #filter(!device == "V10")%>%
  
  mutate(
    outcome = case_when(
      seal == "N"                       ~ "No seal",
      seal == "Y" & sealing_lost == "Y" ~ "Lost",
      seal == "Y" & sealing_lost == "N" ~ "Held",
      TRUE ~ NA_character_
    ),
    band_short = dplyr::recode(as.character(pressure_band), low = " Low", intermediate = " Mid", high = "High"),
    outcome     = factor(outcome, levels = c("Held", "Lost", "No seal")),
    diameter_mm = factor(diameter_mm),   # 6 / 7 / 8 as discrete groups
    x_pos       = c("Held" = 1, "Lost" = 3, "No seal" = 5)[as.character(outcome)]  # <-- ADD
  )

# p_geometry <- ggplot(geom_df,
#                      aes(x = outcome, y = thickness_um, colour = diameter_mm, shape=device)) +
#   geom_jitter(width = 0.3, height = 0, size = 3, alpha = 0.8) +
#   scale_shape_manual(values = c(16, 17, 15, 18, 8)) +
#   labs(
#     title = "Explant geometry by outcome (positive pressure)",
#     x = NULL, y = "Thickness (µm)", colour = "Diameter (mm)"
#   ) +
#   theme_minimal()
# 
# p_geometry 

# Geometry figure — device shown as the TEXT label at each datapoint
# (instead of point shape). Same data (geom_df) as before.

p_geometry_lab <- ggplot(geom_df,
                         aes(x = x_pos, y = thickness_um,
                              label = diameter_mm)) +  #x = outcome, colour = diameter_mm, x_pos was defined in geom_df to allow for mor spaceing
  ggrepel::geom_label_repel(
    aes(fill = device),
    colour        = "white",     # TEXT colour (fixed) — not the box edge
    fontface      = "bold",
    label.r       = unit(0.2, "lines"),  # corner radius; set 0 for square
    position      = position_jitter(width = 0.35, height = 0, seed = 1),
    min.segment.length = 0,
    segment.colour = "grey70",         # no connector line
    segment.size = 0.5,
    max.overlaps  = 40,
    size          = 2.5
  ) +
  scale_fill_manual(values = DEVICE_COLOURS, breaks = names(DEVICE_COLOURS)) +
  scale_x_continuous(                                                          # <-- ADD this whole layer
    breaks = c(1, 3, 5),
    labels = c("Preserved", "Lost", "No seal"),
    limits = c(0.5, 5.5)
  ) +
  scale_y_continuous(breaks = scales::breaks_width(100)) +
  labs(
    title = paste0("Explant geometry by outcome (Negative pressure) \u2014 ", TISSUE_FILTER),         # <----- CHANGE TITLE!
    x = NULL, y = "Thickness (µm)", fill = "Device version (-)"
  ) +
  theme_bw()

p_geometry_lab

########
p_geometry_lab_pressure <- ggplot(geom_df,
                         aes(x = x_pos, y = thickness_um,
                             label = band_short)) +  #x = outcome, colour = diameter_mm, x_pos was defined in geom_df to allow for mor spaceing
  ggrepel::geom_label_repel(
    aes(fill = device),
    colour        = "white",     # TEXT colour (fixed) — not the box edge
    fontface      = "bold",
    label.r       = unit(0.2, "lines"),  # corner radius; set 0 for square
    position      = position_jitter(width = 0.35, height = 0, seed = 1),
    min.segment.length = 0,
    segment.colour = "grey70",         # no connector line
    segment.size = 0.5,
    max.overlaps  = 40,
    size          = 2.5
  ) +
  scale_fill_manual(values = DEVICE_COLOURS, breaks = names(DEVICE_COLOURS)) +
  scale_x_continuous(                                                          # <-- ADD this whole layer
    breaks = c(1, 3, 5),
    labels = c("Preserved", "Lost", "No seal"),
    limits = c(0.5, 5.5)
  ) +
  scale_y_continuous(breaks = scales::breaks_width(100)) +
  labs(
    title = paste0("Explant geometry by outcome (Negative pressure) \u2014 ", TISSUE_FILTER),         # <----- CHANGE TITLE!
    x = NULL, y = "Thickness (µm)", fill = "Device version (-)"
  ) +
  theme_bw()

p_geometry_lab_pressure

# =====================================================================
#  ---- FLOWRATE SUB-ANALYSIS  (log scale, quasirandom spread) ----
# ---------------------------------------------------------------------
# One point = one sealed explant.
#   y      : flowrate, log10  (coalesced: whichever flow column has a value)
#   facet  : perfusion mode (pressure_sign)
#   colour : device (version)
#   shape  : seal outcome     (held = filled dot, lost = hollow ring)
#   size   : pressure band    (low / intermediate / high)
#   spread : ggbeeswarm quasirandom — density-adaptive, x carries no meaning
#
# Excluded: no-seal explants (no flowrate); EXP-IA runs; other tissues.
# NQ flow values (L-NQ / H-NQ) are parked at axis edges as flagged sentinels.
# Points with no pressure band (e.g. an unquantified H-NQ pressure) are dropped.
# =====================================================================

# ---------------------------------------------------------------------
# 1. Build + classify the flow value
# ---------------------------------------------------------------------
flow_df <- all_explants %>%
  filter(seal == "Y", sealing_lost %in% c("Y", "N")) %>%      # sealed explants only
  #filter(!str_starts(experimental_id, FLOWRATE_EXCLUDE)) %>%  # drop EXP-IA runs
  filter(tissue_type == TISSUE_FILTER) %>%                    # one tissue, like other figures
  filter(!device == "V10") %>%                                # inactivate if you want to include all versions
  mutate(
    # take whichever flow column has a value (they never both have one)
    flow_raw = coalesce(as.character(mean_measured_flowrate),
                        as.character(v_based_flowrate)),
    flow_raw = str_trim(flow_raw),
    
    # classify each cell: number / low-NQ / high-NQ / blank
    flow_class = case_when(
      str_detect(str_to_upper(flow_raw), "L\\s*-?\\s*NQ") ~ "nq_low",
      str_detect(str_to_upper(flow_raw), "H\\s*-?\\s*NQ") ~ "nq_high",
      str_detect(flow_raw, "[0-9]")                       ~ "quantified",
      TRUE                                                ~ NA_character_   # blank -> dropped
    ),
    flow_num = if_else(flow_class == "quantified",
                       as.numeric(str_extract(flow_raw, "[0-9]+\\.?[0-9]*")),
                       NA_real_),
    
    outcome = factor(if_else(sealing_lost == "Y", "lost", "held"),
                     levels = c("held", "lost")),
    pressure_sign = factor(pressure_sign,
                           levels = c("Negative pressure", "Positive pressure"))
  ) %>%
  filter(!is.na(flow_class))     # drop truly blank flow

# ---------------------------------------------------------------------
# 2. Sentinel positions for NQ points (multiplicative -> log-safe, stays > 0)
# ---------------------------------------------------------------------
q_min <- min(flow_df$flow_num, na.rm = TRUE)
q_max <- max(flow_df$flow_num, na.rm = TRUE)

flow_df <- flow_df %>%
  mutate(
    flow_plot = case_when(
      flow_class == "quantified" ~ flow_num,
      flow_class == "nq_low"     ~ q_min / 2,   # just below smallest real value
      flow_class == "nq_high"    ~ q_max * 2    # just above largest real value
    ),
    is_nq = flow_class != "quantified"
  )

# ---------------------------------------------------------------------
# 3. Plot data: drop points with no pressure band, and droplevels()
#    so the empty "NA" level disappears from the size legend too.
# ---------------------------------------------------------------------
flow_df_plot <- flow_df %>%
  filter(!is.na(pressure_band)) %>%
  mutate(pressure_band = droplevels(pressure_band))

# ---------------------------------------------------------------------
# ---- 4. Figure - flow - V1 -----
# ---------------------------------------------------------------------
p_flow_log <- ggplot(flow_df_plot,
                     aes(x = 1, y = flow_plot,
                         colour = device, shape = outcome,size = pressure_band)) +   #,size = pressure_band)<---change x=1 to x=device if you want to categorise by device, as well as all the other x=
  # quasirandom: spreads by local density; width caps the max spread
  ggbeeswarm::geom_quasirandom(width =0.05, alpha = 0.6, stroke = 0.8) +
  # label the off-scale NQ points, nudged consistently so they read as tethered
  ggrepel::geom_text_repel(
    data = dplyr::filter(flow_df_plot, is_nq),
    aes(x = 1, y = flow_plot, label = flow_raw),
    inherit.aes = FALSE,
    size = 2.6, colour = "grey30",
    nudge_x = 0.05, direction = "y",
    min.segment.length = 0, segment.colour = "grey50",
    max.overlaps = 30
  ) +
  ggrepel::geom_text_repel(
    data = dplyr::filter(flow_df_plot, clamp_leak == "Y"),
    aes(x = 1, y = flow_plot, label = "leak"),
    inherit.aes = FALSE,
    size = 2.4, colour = "grey30", fontface = "italic",
    nudge_x =  -0.25,direction = "y",
    min.segment.length = 0, segment.colour = "grey60",
    max.overlaps = 30
  ) +
  facet_grid(~ pressure_sign) +
 # scale_y_log10(labels = scales::label_log(), breaks = scales::breaks_log(n = 8)) +
  scale_y_log10(breaks = function(x) 10^seq(floor(log10(x[1])), ceiling(log10(x[2]))),
               labels = scales::label_log())+
  annotation_logticks(sides = "l", colour = "grey70") +
  scale_shape_manual(name = "Seal outcome",
                     values = c(held = 16, lost = 1),
                     labels = c(held = "Preserved", lost = "Lost")) +
  scale_size_manual(name = "Pressure band (mbar)",
                    values = c(low = 2, intermediate = 4, high  = 6),
                    labels = c(low = "Low [0-40]",
                               intermediate = "Mid (40-80]",
                               high = "High (> 80) ")) +
  scale_x_continuous(breaks = NULL) +
  scale_colour_manual(name = "Device", values = DEVICE_COLOURS, drop = TRUE) +
  labs(title = paste0("Measured Flowrate \u2014 ", TISSUE_FILTER),
       x = NULL, y = "Flowrate (nL/min)", colour = "Device") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
        axis.title.y.left  = element_text( size = 13, face = "bold"),
        axis.text.y.left  = element_text(size=12, face = "bold"))

p_flow_log

# =====================================================================
# ---- 4. Figure - flow- V2 ------ 
#
#FLOWRATE (log) — pre-computed spread so repel labels tether exactly.
# vipor::offsetX() is the same engine ggbeeswarm::geom_quasirandom uses,
# so storing the offset in a column gives identical spacing while letting
# the label layers read the SAME x as the points (segment lands on point).
# =====================================================================

# ---- pre-compute the x offset (per facet, on log scale) -------------
# offsets are density-based; compute on log10(y) so spacing matches the
# log axis, and within each perfusion mode since facets are independent.
flow_df_plot <- flow_df_plot %>%
  group_by(pressure_sign) %>%
  mutate(x_off = 1 + vipor::offsetX(log10(flow_plot), width = 0.1)) %>%
  ungroup()

# ---- plot -----------------------------------------------------------
p_flow_log <- ggplot(flow_df_plot,
                     aes(x = x_off, y = flow_plot,
                         colour = device, shape = outcome, size = pressure_band)) +
  geom_point(width = 0.1, alpha = 0.6, stroke = 0.8) +
  
  # NQ labels: anchored on x_off, nudged to one side (kept as before)
  ggrepel::geom_text_repel(
    data = dplyr::filter(flow_df_plot, is_nq),
    aes(x = x_off, y = flow_plot, label = flow_raw),
    inherit.aes = FALSE,
    size = 2.6, colour = "grey30",
    nudge_x = 0.35, direction = "y",
    min.segment.length = Inf, segment.colour = "grey50",
    max.overlaps = 30
  ) +
  
  # CLAMP LEAK: "*" label, NO nudge — repel finds its own spot and the
  # segment runs straight to the point (both read x_off, so it aligns).
  ggrepel::geom_text_repel(
    data = dplyr::filter(flow_df_plot, clamp_leak == "Y"),
    aes(x = x_off, y = flow_plot, label = "leak"),
    inherit.aes = FALSE,
    size = 4, fontface = "italic", colour = "black",
    min.segment.length = 0, segment.colour = "grey40", segment.size = 0.5,
    max.overlaps = 30
  ) +
  
  facet_grid(~ pressure_sign) +
  scale_y_log10(
    breaks = function(x) 10^seq(floor(log10(x[1])), ceiling(log10(x[2]))),
    labels = scales::label_log()
  ) +
  annotation_logticks(sides = "l", colour = "grey70") +
  scale_shape_manual(name = "Seal outcome",
                     values = c(held = 16, lost = 1),
                     labels = c(held = "Preserved", lost = "Lost")) +
  scale_size_manual(name = "Pressure band (mbar)",
                    values = c(low = 2, intermediate = 4, high  = 6),
                    labels = c(low = "Low [0-40]",
                               intermediate = "Mid (40-80]",
                               high = "High (> 80) ")) +
  scale_x_continuous(breaks = NULL) +
  scale_colour_manual(name = "Device", values = DEVICE_COLOURS, drop = TRUE) +
  labs(title = paste0("Measured Flowrate \u2014 ", TISSUE_FILTER),
       x = NULL, y = "Flowrate (nL/min)", colour = "Device") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold"),
        panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
        axis.title.y.left  = element_text( size = 13, face = "bold"),
        axis.text.y.left  = element_text(size=12, face = "bold"))

p_flow_log

# =====================================================================
# ---- PRESSURE DIFFERENTIAL PLOT ----
#   delta_p = |cross_flow_pressure| - |asp_pressure|   (magnitude - magnitude)
#   positive Δ => cross-flow exceeds aspiration.
#
#   x      : outcome  (Preserved = 1, Lost = 3)   [strip: width = 0 for now]
#   y      : delta_p, centred on the Δ = 0 reference line
#   colour : device
#   shape  : pressure_sign  (open vs filled circle)
#
#   Sealed explants only (held + lost; no-seals removed). EXP-IA excluded.
#   Obeys TISSUE_FILTER. Rows with no computable Δ are reported, not plotted.
# =====================================================================

# ---------------------------------------------------------------------
# 1. Build the differential
# ---------------------------------------------------------------------
diff_df <- all_explants %>%
  filter(seal == "Y", sealing_lost %in% c("Y", "N")) %>%      # keep held + lost
  #filter(!str_starts(experimental_id, FLOWRATE_EXCLUDE)) %>%  # drop EXP-IA runs
  filter(tissue_type == TISSUE_FILTER) %>%
  #filter(!device == "V10") %>%      # inactivate if you want to include all versions
  #filter(!device %in% c("V10", "V2", "V5/V7")) %>%   #inactivate if you want to include all versions or edit to add specific versions

  mutate(
    asp_pressure_num = as.numeric(str_extract(asp_pressure, "[0-9]+\\.?[0-9]*")),
    delta_p = abs(pressure_mag) - abs(asp_pressure_num),      # |cross| - |asp|
    
    outcome = factor(if_else(sealing_lost == "Y", "lost", "held"),
                     levels = c("held", "lost")),
    x_pos   = c(held = 1, lost = 2)[as.character(outcome)],
    
    pressure_sign = factor(pressure_sign,
                           levels = c("Negative pressure", "Positive pressure")),
    mode_short = case_when(
      pressure_sign == "Negative pressure" ~ "withdrawal",
      pressure_sign == "Positive pressure" ~ "infusion",
      TRUE ~ NA_character_
    ),
    shape_key = paste(outcome, mode_short),      # e.g. "held withdrawal"
    shape_key = factor(shape_key, levels = c(
      "held withdrawal", "held infusion",
      "lost withdrawal", "lost infusion"))
  )

# ---------------------------------------------------------------------
# 2. Report rows with no computable Δ (missing cross or aspiration value)
# ---------------------------------------------------------------------
delta_missing <- diff_df %>%
  filter(is.na(delta_p)) %>%
  select(explant_id, tissue_type, cross_flow_pressure, pressure_mag,
         asp_pressure, asp_pressure_num)

cat("\n=== DELTA-P NOT COMPUTABLE (missing cross or aspiration) ===\n")
if (nrow(delta_missing) == 0) cat("none\n") else print(delta_missing, n = Inf)

diff_plot <- diff_df %>% filter(!is.na(delta_p))

# ---------------------------------------------------------------------
# 3. Plot
# ---------------------------------------------------------------------
p_delta <- ggplot(diff_plot,
                  aes(x = x_pos, y = delta_p,
                      colour = device, shape = pressure_sign)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 1),
             size = 3, alpha = 0.85, stroke = 1) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Preserved", "Lost"),
                     limits = c(0.5, 2.5)) +
  scale_shape_manual(name = "Perfusion mode",
                     values = c("Negative pressure" = 1,   # open circle
                                "Positive pressure" = 16), # filled circle
                     labels = c("Negative", "Positive")) +
  scale_colour_manual(name = "Device", values = DEVICE_COLOURS, drop = TRUE) +
  labs(title = paste0("Pressure differential by outcome \u2014 ", TISSUE_FILTER),
       x = NULL,
       y = "\u0394 (Cross \u2212 Aspiration) P  (mbar)",
       colour = "Device") +
  theme_bw() +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"))

p_delta

##### Alternative scaterplot for p delta

p_delta_1 <- ggplot(diff_plot,
                    aes(x = asp_pressure_num, y = pressure_mag,
                        colour = device, shape = shape_key)) +
 # geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_point(size = 3, alpha = 0.85, stroke = 1) +
  geom_abline(intercept = 0, slope = 1)+
  # #scale_x_continuous(breaks = c(1, 2),
  #                    labels = c("Preserved", "Lost"),
  #                    limits = c(0.5, 2.5)) +
  scale_shape_manual(
    name = "Outcome / mode",
    values = c(
      "held withdrawal" = 16,   # filled circle
      "held infusion"   = 17,   # filled triangle
      "lost withdrawal" = 1,    # hollow circle
      "lost infusion"   = 2     # hollow triangle
    ),
    labels = c(
      "held withdrawal" = "Preserved · Withdrawal",
      "lost withdrawal" = "Lost · Withdrawal",
      "held infusion"   = "Preserved · Infusion",
      "lost infusion"   = "Lost · Infusion"
    )
  )+
  scale_colour_manual(name = "Device", values = DEVICE_COLOURS, drop = TRUE) +
  labs(title = paste0("Pressure differential by outcome \u2014 ", TISSUE_FILTER),
       x = "Abs(Aspiration pressure) P (mbar)",
       y = "Abs (Cross pressure) P  (mbar)",
       colour = "Device") +
  theme_bw() +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"))

p_delta_1
#######

#############################################
# ---- Aspiration data frame and plot ----
#############################################

asp_df <- all_explants %>%
  filter(tissue_type == TISSUE_FILTER) %>%
  filter(!device == "V10") %>%                                # inactivate if you want to include all versions
  mutate(
    asp_pressure_num = as.numeric(str_extract(asp_pressure, "[0-9]+\\.?[0-9]*")),
    asp_pressure_signed = -asp_pressure_num,  # always-negative aspiration
    outcome = case_when(
      seal == "N"                       ~ "No seal",
      seal == "Y" & sealing_lost == "Y" ~ "Lost",
      seal == "Y" & sealing_lost == "N" ~ "Held",
      TRUE ~ NA_character_
    ),
    outcome     = factor(outcome, levels = c("Held", "Lost", "No seal")),
    x_pos   = c("Held" = 1, "Lost" = 2, "No seal" =3)[as.character(outcome)],
    
    pressure_sign = factor(pressure_sign,
                           levels = c("Negative pressure", "Positive pressure"))
  )%>% 
filter(!is.na(asp_pressure_num))

# plot
p_aspiration <- ggplot(asp_df,
                              aes(x = x_pos, y = asp_pressure_signed,
                                  colour = device, shape = pressure_sign)) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 1),
             size = 3, alpha = 0.5, stroke = 1) +
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c("Preserved", "Lost", "No Seal"),
                     limits = c(0.5, 3.5)) +
  scale_shape_manual(name = "Perfusion mode",
                     values = c("Negative pressure" = 1,   # open circle
                                "Positive pressure" = 16), # filled circle
                     labels = c("Negative", "Positive")) +
  scale_colour_manual(name = "Device", values = DEVICE_COLOURS, drop = TRUE) +
  labs(title = paste0("Aspriration Pressure across attempts \u2014 ", TISSUE_FILTER),
       x = NULL,
       y = "Aspiration Pressure  (mbar)",
       colour = "Device") +
  theme_bw() +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"))

p_aspiration

# =====================================================================
# ---- SAVE PLOTS AS JPEG — filename  -----
# carries TISSUE_FILTER so tissues don't
# overwrite each other. Put each line directly under its plot object.
# (paste::0 keeps the tissue in the name, e.g. "flowrate_4434.jpeg")
# =====================================================================

# --- gate 2: seal preservation by device & pressure (p_by_pressure) --------
ggsave(paste0("seal_preservation_", TISSUE_FILTER, ".jpeg"),
       plot = p_by_pressure, device = "jpeg",
       width = 8, height = 6, dpi = 300)

# --- gate 1: seal / no-seal (p_by_seal) -------------------------------
ggsave(paste0("seal_formation_", TISSUE_FILTER, ".jpeg"),
       plot = p_by_seal, device = "jpeg",
       width = 7, height = 6, dpi = 300)

# --- geometry lookup figure (p_geometry_lab) --------------------------
ggsave(paste0("geometry_by_diameter_", TISSUE_FILTER, ".jpeg"),
       plot = p_geometry_lab, device = "jpeg",
       width = 6, height = 4, dpi = 300)

# --- geometry lookup figure (p_geometry_lab_by pressure) --------------------------
ggsave(paste0("geometry_by_pressure_", TISSUE_FILTER, ".jpeg"),
       plot = p_geometry_lab_pressure, device = "jpeg",
       width = 6, height = 4, dpi = 300)

# --- flowrate, log scale (p_flow_log) ---------------------------------
ggsave(paste0("flowrate_log_", TISSUE_FILTER, ".jpeg"),
       plot = p_flow_log, device = "jpeg",
       width = 9, height = 5, dpi = 300)

# --- pressure differential (p_delta) ----------------------------------
ggsave(paste0("pressure_delta_", TISSUE_FILTER, ".jpeg"),
       plot = p_delta, device = "jpeg",
       width = 6, height = 4, dpi = 300)

# --- pressure differential (p_aspiration) ----------------------------------
ggsave(paste0("pressure_asp_", TISSUE_FILTER, ".jpeg"),
       plot = p_aspiration, device = "jpeg",
       width = 6, height = 4, dpi = 300)
