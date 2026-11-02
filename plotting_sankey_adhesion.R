# =====================================================================
# Alluvial: sealing -> bubbles -> reservoir/flow
# Faceted by tissue_type (rows) x operation_mode (cols)
# =====================================================================

library(tidyverse)
library(readxl)
library(scales)
library(ggalluvial)
library(ggplot2)

# ---------------------------------------------------------------------
# ---- 0. CONFIG — set these ----
# ---------------------------------------------------------------------
DATA_PATH      <- "C:\\Users\\scharalambous\\Box\\One Drive To Box Migration\\PhD folder\\Thesis writing\\2026\\Chapter 2_ResultsChapter1_Constriction\\FOR COLUMN PLOTTING EXTRUSION_BUBBLE_FLOW.xlsx"
SHEET          <- 1
CONTROL_SYSTEM <- "Flow driven"   # or "Pressure driven"
DESIGN         <- "Mandrels"
BARRIER_FILTER <- "Yes"            # "Yes" or "No" — fixes the extrusion barrier

# stratum fill (keyed by the RECODED sealing names — see step 2)
fill_cols <- c(
  # bubbles
  "No bubbles"="#0046FF","Small bubbles"="#7EC8FF","Extended bubbles"="#E4211C",
  # sealing (drives the ribbon fill)
  "Good"="#0046FF","Imperfect"="#F2C300",
  "Extrusion"="#E4211C","Compressed"="#8E6FB0",
  # flow
  "No flow"="#E4211C","Inconsistent"="#F2C300","Consistent"="#8E6FB0")

# ---------------------------------------------------------------------
# ---- 1. READ + RENAME by POSITION (16 cols, A..P) ----
# ---------------------------------------------------------------------
raw <- read_excel(DATA_PATH, sheet = SHEET)
stopifnot(ncol(raw) >= 17)
names(raw)[1:17] <- c(
  "unique_ID","experimental_id","explant_ID","date","chip_design",
  "control_system","operation_mode","flow_rate","extrusion_barrier",
  "duration","tissue_type","tissue_size","adhesion",
  "sealing","bubbles","reservoir_level", "dettachment_pressure"          # N, O, P
)

# ---------------------------------------------------------------------
# ---- 2. CLEAN, MERGE, RECODE, FILTER ----
# ---------------------------------------------------------------------
d <- raw %>%
  mutate(across(c(sealing, bubbles, reservoir_level, tissue_type,
                  control_system, operation_mode, chip_design,
                  extrusion_barrier), str_trim)) %>%
  # merge the two extrusion grades into one
  mutate(sealing = if_else(sealing %in% c("Slight extrusion","Extrusion"),
                           "Extrusion", sealing)) %>%
  # shorten the stratum labels (these strings ARE what prints in each box)
  mutate(
    sealing = recode(sealing,
                     "Good sealing"      = "Good",
                     "Imperfect sealing" = "Imperfect"),
    reservoir_level = recode(reservoir_level,
                             "Inconsistent flow" = "Inconsistent",
                             "Consistent flow"   = "Consistent")
  ) %>%
  filter(control_system   == CONTROL_SYSTEM,
         chip_design      == DESIGN,
         extrusion_barrier == BARRIER_FILTER) %>%
  mutate(operation_mode = factor(operation_mode,
                                 levels = c("Withdraw", "Infusion")))

# ---------------------------------------------------------------------
# ---- 3. PLOT ----
# ---------------------------------------------------------------------
p <- ggplot(d, aes(axis1 = sealing, axis2 = bubbles, axis3 = reservoir_level)) +
  geom_alluvium(aes(fill = sealing), width = 0.45) +
  geom_stratum(width = 0.45) +
  geom_text(stat = "stratum", size = 2.5, fontface = "bold",
            aes(label = paste0(str_wrap(after_stat(stratum), 10),
                               ":", after_stat(count)))) +
  scale_fill_manual(values = fill_cols) +
  scale_x_discrete(limits = c("Sealing","Bubbles","Flow"),
                   expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  facet_grid(tissue_type ~ operation_mode, drop = TRUE) +
  labs(y = NULL) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border    = element_rect(colour = "grey30", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = "grey30"),
    strip.text      = element_text(face = "bold"),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.text.x   = element_text(size =12),
    legend.position = "none")

p

# ---- save (uncomment, set size) ----
ggsave("alluvial_barrier-withEB_FlowDriven_Mandrels.png", p,
       width = 10, height = 7, dpi = 300)



# =====================================================================
# Detachment outcome — bar chart of FRACTIONS
# Subset: chip_design = Silicon tubes, Pressure driven, Infusion,
#         adhesion = Yes
# Outcomes (3, from the "Tissue placement" column = `sealing`):
#         Good sealing / Partial Dettachment / Dettached
# Assumes `raw` already read in (17 cols) by the main script.
# =====================================================================

library(tidyverse)
library(scales)

# ---- CONFIG ----
TUBE_DESIGN <- "Silicon tubes"    # exact string in chip_design (col E)
OUTCOME_COL <- "sealing"          # col N "Tissue placement" -> renamed sealing

# keyed by CLEANED display names (data is recoded to these below)
det_cols <- c(
  "Good sealing"       = "#0046FF",
  "Partial detachment" = "#F2C300",
  "Detached"           = "#E4211C")

# ---------------------------------------------------------------------
# ---- 1. FILTER, RECODE (verbatim -> clean), SUMMARISE ----
# ---------------------------------------------------------------------
det <- raw %>%
  mutate(across(c(chip_design, control_system, operation_mode,
                  adhesion, all_of(OUTCOME_COL)), str_trim)) %>%
  filter(chip_design    == TUBE_DESIGN,
         control_system == "Pressure driven",
         operation_mode == "Infusion",
         adhesion       == "Yes") %>%
  mutate(outcome = recode(.data[[OUTCOME_COL]],
                          "Dettached"           = "Detached",
                          "Partial Dettachment" = "Partial detachment",
                          "Good sealing"        = "Good sealing")) %>%
  filter(!is.na(outcome), outcome != "")

det_summ <- det %>%
  count(outcome, name = "k") %>%
  complete(outcome = names(det_cols), fill = list(k = 0)) %>%   # keep 0-count bars
  mutate(
    frac    = k / sum(k),
    outcome = factor(outcome, levels = names(det_cols))
  )

n_total <- sum(det_summ$k)
det_summ                                        # eyeball the numbers

# ---------------------------------------------------------------------
# ---- 2. BAR CHART ----
# ---------------------------------------------------------------------
p_det <- ggplot(det_summ, aes(x = "silicone tubes", y = frac, fill = outcome)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(frac, 2), ":\n", k)),
            position = position_stack(vjust = 0.5),
           size = 6, colour = "white", fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = det_cols, name="Outcome") +
  scale_y_continuous(limits = c(0, 1.0),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Fraction of explants",
       subtitle = paste0("Silicon tubes \u00b7 Pressure driven \u00b7 Infusion \u00b7 ",
                         "adhesion = Yes  (n = ", n_total, ")")) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "grey30", fill = NA),
    axis.text  = element_text(size = 14),   # tick labels
    axis.title = element_text(size = 16),    # axis titles
    axis.text.y = element_blank(),
    legend.text  = element_text(size = 11),
    legend.title = element_text(size = 12)
)

p_det

#---- save (set size when exporting) ----
ggsave("detachment_fractions_silicon_infusion.png", p_det,
       width = 5, height = 4, dpi = 300)