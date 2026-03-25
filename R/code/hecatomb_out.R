## ── Packages ─────────────────────────────────────────────────────────────
# install.packages("rstatix")  # run once if needed

library(tidyr)
library(dplyr)
library(ggplot2)
library(rstatix)
library(data.table)
library(readxl)

## ── Paths ────────────────────────────────────────────────────────────────
data_dir <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results"

results_dir <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results/R/results"

## ── Load data (FULL tables) ──────────────────────────────────────────────
bigtable_full <- fread(file.path(data_dir, "bigtable.tsv"))
meta          <- fread(file.path(data_dir, "allsamples.csv"))
taxonCounts   <- fread(file.path(data_dir, "taxonLevelCounts.tsv"))

head (bigtable_full, n=2)

## ── Sanity checks ────────────────────────────────────────────────────────
stopifnot("sampleID" %in% colnames(bigtable_full))
stopifnot("sampleID" %in% colnames(meta))

## ── Merge metadata (FULL merged table) ───────────────────────────────────
dataMeta_full <- merge(bigtable_full, meta, by = "sampleID")

head(dataMeta_full, n=2)

####################################
##Total reads per sample
###################################


############################################################
## HECATOMB FILTERING WORKFLOW — FINAL VERSION
############################################################

library(dplyr)
library(ggplot2)

############################################################
## 1. Extract viral hits
############################################################

viruses <- dataMeta_full %>%
  filter(kingdom == "Viruses")

cat("Total viral alignments:", nrow(viruses), "\n")
print(table(viruses$alnType))

############################################################
## 2. Strong statistical pre-filter
############################################################

viruses_prefilter <- viruses %>%
  filter(
    evalue < 1e-20,
    alnlen >= 150
  )

cat("After evalue + length filter:", nrow(viruses_prefilter), "\n")

############################################################
## 3. Calculate total reads and viral reads per sample.type
############################################################

# total reads per sample type
total_reads <- dataMeta_full %>%
  group_by(sample.type) %>%
  summarise(total_reads = sum(count), .groups = "drop")

# viral reads after filtering
viral_reads <- viruses_prefilter %>%
  group_by(sample.type) %>%
  summarise(viral_reads_after_filter = sum(count), .groups = "drop")

############################################################
## 4. Combine and calculate proportion
############################################################

summary_table <- total_reads %>%
  left_join(viral_reads, by = "sample.type") %>%
  mutate(
    viral_reads_after_filter = ifelse(is.na(viral_reads_after_filter), 0, viral_reads_after_filter),
    viral_proportion = viral_reads_after_filter / total_reads
  )

############################################################
## 5. Print summary table
############################################################

print(summary_table)


############################################################
## 3. Visual inspection — Top 20 families
############################################################

top20_families <- viruses_prefilter %>%
  group_by(family) %>%
  summarise(
    total_hits = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_hits)) %>%
  slice_head(n = 20) %>%
  pull(family)

viruses_plot <- viruses_prefilter %>%
  filter(family %in% top20_families)

############################################################
## Alignment quality quadrant plot
############################################################

plot_quadrant <- ggplot(viruses_plot) +
  geom_point(
    aes(
      x = alnlen,
      y = pident,
      color = alnType,
      size = count
    ),
    alpha = 0.15
  ) +
  facet_wrap(~family, scales = "free", ncol = 5) +
  geom_vline(
    xintercept = 150,
    linetype = "longdash",
    colour = "red",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 60,
    linetype = "longdash",
    colour = "blue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 75,
    linetype = "longdash",
    colour = "darkgreen",
    linewidth = 0.6
  ) +
  scale_size_continuous(range = c(0.3, 2)) +
  theme_bw(base_size = 18) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Alignment Length",
    y = "Percent Identity",
    color = "Alignment Type",
    size = "Read Count"
  )

############################################################
## Save supplemental figure
############################################################

ggsave(
  filename = file.path(results_dir,
                       "Supplemental_Alignment_Quadrant_Top20Families.tiff"),
  plot = plot_quadrant,
  width = 20,
  height = 24,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

############################################################
## 4. FINAL biological identity filtering
############################################################
## Protein (aa):  ≥60%
## Nucleotide (nt): ≥75%
############################################################

viruses_flagged <- viruses_prefilter %>%
  mutate(
    filter_flag = case_when(
      alnType == "aa" & pident >= 60 ~ "pass",
      alnType == "nt" & pident >= 75 ~ "pass",
      TRUE ~ "filter"
    )
  )

print(table(viruses_flagged$filter_flag))

############################################################
## 5. Create final filtered dataset
############################################################

dataMeta_pf <- viruses_flagged %>%
  filter(filter_flag == "pass") %>%
  mutate(
    family = ifelse(is.na(family) | family == "",
                    "Unclassified",
                    family)
  )

cat("Final filtered alignments:", nrow(dataMeta_pf), "\n")

############################################################
## End filtering workflow
############################################################
############################################################
## 6. Export filtered dataset
############################################################

library(readr)

out_file <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results/R/data/dataMeta_pf_filtered.csv"

write_csv(dataMeta_pf, out_file)

cat("Filtered dataset exported to:", out_file, "\n")


############################################################
## Viral Read Summary (RAW vs Final Filtered)
############################################################

# Raw viral alignments (before QC + identity filtering)
viral_raw <- dataMeta_full %>%
  filter(kingdom == "Viruses")

viral_raw_counts <- viral_raw %>%
  group_by(sample.type) %>%
  summarise(
    viral_reads_raw = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

# Final filtered viral alignments
viral_filtered_counts <- dataMeta_pf %>%
  group_by(sample.type) %>%
  summarise(
    viral_reads_filtered = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

# Join raw + filtered
viral_summary <- viral_raw_counts %>%
  left_join(viral_filtered_counts, by = "sample.type") %>%
  mutate(
    viral_reads_filtered = ifelse(is.na(viral_reads_filtered), 0, viral_reads_filtered),
    pct_retained = round(100 * viral_reads_filtered / viral_reads_raw, 2)
  )

viral_summary



colnames(dataMeta_pf)



############################################################
## Calculate viral fraction per metagenome and mean ± SD
############################################################

library(dplyr)

## 1. Total reads per sample
total_reads_sample <- dataMeta_full %>%
  group_by(sampleID, sample.type) %>%
  summarise(
    total_reads = sum(count),
    .groups = "drop"
  )

## 2. Viral reads per sample (after final filtering)
viral_reads_sample <- dataMeta_pf %>%
  group_by(sampleID) %>%
  summarise(
    viral_reads = sum(count),
    .groups = "drop"
  )

## 3. Merge totals and viral reads, calculate viral fraction
viral_fraction_sample <- total_reads_sample %>%
  left_join(viral_reads_sample, by = "sampleID") %>%
  mutate(
    viral_reads = ifelse(is.na(viral_reads), 0, viral_reads),
    viral_fraction = viral_reads / total_reads
  )

## 4. Calculate mean ± SD per sample type
viral_summary <- viral_fraction_sample %>%
  group_by(sample.type) %>%
  summarise(
    mean_fraction = mean(viral_fraction, na.rm = TRUE),
    sd_fraction = sd(viral_fraction, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mean_percent = mean_fraction * 100,
    sd_percent = sd_fraction * 100
  )

## 5. Print results
print(viral_summary)

## 6. Save table for manuscript
write.csv(
  viral_summary,
  file = file.path(results_dir, "viral_fraction_summary_mean_sd.csv"),
  row.names = FALSE
)




############################################################
## Export Viral Families (Final Filtered)
############################################################

virus_families <- dataMeta_pf %>%
  distinct(family) %>%
  arrange(family)

write.csv(
  virus_families,
  file = file.path(results_dir, "virus_families_list.csv"),
  row.names = FALSE
)


############################################################
## Export Viral Genera
############################################################

virus_genera <- dataMeta_pf %>%
  filter(!is.na(genus), genus != "") %>%
  distinct(genus) %>%
  arrange(genus)

write.csv(
  virus_genera,
  file = file.path(results_dir, "virus_genus_list.csv"),
  row.names = FALSE
)


########################################
## Load virus family → host annotation from https:\\www.genome.jp\virushostdb\view\
########################################

library(dplyr)
library(readr)

# -------------------------
# 1. Read the host annotation table
# -------------------------
host_df <- read_csv(
  "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results/R/results/virus_families_list.csv",
  show_col_types = FALSE
)

# -------------------------
# 2. Clean whitespace
# -------------------------
host_df <- host_df %>%
  mutate(
    family = trimws(family),
    host   = trimws(host)
  )

dataMeta_pf <- dataMeta_pf %>%
  mutate(family = trimws(family))

# -------------------------
# 3. Remove ANY existing host columns
# -------------------------
dataMeta_pf <- dataMeta_pf %>%
  select(-matches("^host"))

# -------------------------
# 4. Join host table (creates ONE column named "host")
# -------------------------
dataMeta_pf <- dataMeta_pf %>%
  left_join(host_df, by = "family")

# -------------------------
# 5. Sanity checks
# -------------------------

# Confirm host column exists
grep("host", colnames(dataMeta_pf), value = TRUE)

# Check join success
dataMeta_pf %>%
  filter(kingdom == "Viruses") %>%
  summarise(
    total_viral_rows  = n(),
    rows_with_host    = sum(!is.na(host)),
    rows_missing_host = sum(is.na(host))
  )

# Confirm 1 host per viral family
dataMeta_pf %>%
  filter(kingdom == "Viruses") %>%
  group_by(family) %>%
  summarise(n_hosts = n_distinct(host)) %>%
  filter(n_hosts > 1)


colnames(dataMeta_pf)




########################################
## Host domain composition by sample type
## Viral reads only (100% stacked)
########################################

library(dplyr)
library(ggplot2)
library(scales)

# -------------------------
# 1. Prepare virus-only data
# -------------------------
host_df <- dataMeta_pf %>%
  filter(
    kingdom == "Viruses",
    host %in% c("Eukaryota", "Bacteria", "Unknown"),
    !is.na(sample.type)
  ) %>%
  group_by(sample.type, host) %>%
  summarise(
    reads = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sample.type) %>%
  mutate(
    rel_abundance = reads / sum(reads)
  ) %>%
  ungroup()

# -------------------------
# 2. Factor order
# -------------------------
host_df$host <- factor(
  host_df$host,
  levels = c("Eukaryota", "Bacteria", "Unknown")
)

# -------------------------
# 3. Colorblind-safe palette
# -------------------------
host_palette <- c(
  "Eukaryota" = "#0072B2",
  "Bacteria"  = "#D55E00",
  "Unknown"   = "#999999"
)

# -------------------------
# 4. Plot (100% stacked)
# -------------------------
p_host <- ggplot(
  host_df,
  aes(x = sample.type, y = rel_abundance, fill = host)
) +
  geom_col(width = 0.8) +
  
  # Percentage labels (hide <1%)
  geom_text(
    aes(label = ifelse(rel_abundance >= 0.01,
                       percent(rel_abundance, accuracy = 0.1),
                       "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(values = host_palette) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 13)
  ) +
  labs(
    x = NULL,
    y = "Relative abundance of viral reads (%)",
    fill = "Virus Host domain"
  )

# -------------------------
# 5. Save
# -------------------------
out_dir <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results/R/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  "Figure_host_by_sample_type_percent.pdf",
  p_host,
  path = out_dir,
  width = 12,
  height = 7,
  device = cairo_pdf
)

ggsave(
  "Figure_host_by_sample_type_percent.tiff",
  p_host,
  path = out_dir,
  width = 12,
  height = 7,
  dpi = 600,
  compression = "lzw"
)

# -------------------------
# 6. View
# -------------------------
p_host



############################################################
## Clean Baltimore annotations in filtered dataset
############################################################

library(dplyr)
library(stringr)

dataMeta_pf <- dataMeta_pf %>%
  mutate(
    baltimoreType  = str_trim(baltimoreType),
    baltimoreGroup = str_trim(baltimoreGroup),
    
    baltimoreType = case_when(
      is.na(baltimoreType) ~ "Unclassified",
      baltimoreType == "" ~ "Unclassified",
      baltimoreType == "NA" ~ "Unclassified",
      TRUE ~ baltimoreType
    ),
    
    baltimoreGroup = case_when(
      is.na(baltimoreGroup) ~ "Unclassified",
      baltimoreGroup == "" ~ "Unclassified",
      baltimoreGroup == "NA" ~ "Unclassified",
      TRUE ~ baltimoreGroup
    )
  )

# Confirm cleaning
table(dataMeta_pf$baltimoreType)
table(dataMeta_pf$baltimoreGroup)



############################################################
## Baltimore genome type composition (NO collapsing)
############################################################

library(dplyr)
library(ggplot2)
library(scales)

baltimore_df <- dataMeta_pf %>%
  filter(
    kingdom == "Viruses",
    !is.na(sample.type)
  ) %>%
  group_by(sample.type, baltimoreType) %>%
  summarise(
    reads = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sample.type) %>%
  mutate(
    rel_abundance = reads / sum(reads)
  ) %>%
  ungroup()

############################################################
## Set biological order EXACTLY matching your dataset
############################################################

baltimore_df$baltimoreType <- factor(
  baltimore_df$baltimoreType,
  levels = c(
    "dsDNA",
    "ssDNA",
    "dsRNA",
    "ssRNA(+)",
    "ssRNA(-)",
    "ssRNA-RT",
    "dsDNA-RT",
    "Unclassified"
  )
)

############################################################
## Custom 8-color palette
############################################################

baltimore_palette <- c(
  "dsDNA"        = "#0072B2",
  "ssDNA"        = "#E69F00",
  "dsRNA"        = "#009E73",
  "ssRNA(+)"     = "#CC79A7",
  "ssRNA(-)"     = "#D55E00",
  "ssRNA-RT"     = "#56B4E9",
  "dsDNA-RT"     = "#F0E442",
  "Unclassified" = "#999999"
)

############################################################
## Plot
############################################################

p_baltimore <- ggplot(
  baltimore_df,
  aes(x = sample.type,
      y = rel_abundance,
      fill = baltimoreType)
) +
  geom_col(width = 0.8) +
  
  geom_text(
    aes(label = ifelse(rel_abundance >= 0.02,
                       percent(rel_abundance, accuracy = 0.1),
                       "")),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  
  scale_fill_manual(values = baltimore_palette) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 11)
  ) +
  
  labs(
    x = NULL,
    y = "Relative abundance of viral reads (%)",
    fill = "Baltimore genome type"
  )

p_baltimore



############################################################
## Inspect all unique Baltimore types in dataset
############################################################

sort(unique(dataMeta_pf$baltimoreType))

# Also see counts
dataMeta_pf %>%
  count(baltimoreType, sort = TRUE)



############################################################
## Combine Host + Baltimore (A | B)
############################################################

library(patchwork)

final_AB <- (p_host | p_baltimore) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(
      size = 26,
      face = "bold"
    )
  )

final_AB


ggsave(
  filename = file.path(out_dir, "Figure_Viral_Structure_Host_Baltimore.pdf"),
  plot = final_AB,
  width = 18,
  height = 8,
  device = cairo_pdf
)

ggsave(
  filename = file.path(out_dir, "Figure_Viral_Structure_Host_Baltimore.tiff"),
  plot = final_AB,
  width = 18,
  height = 8,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)




########################################
## Stacked barplots by SPECIES
## EUKARYOTIC VIRUSES ONLY
## Top 20 families → top 1 species per family
## Color fixed by FAMILY
########################################

library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(patchwork)
library(colorspace)

# -------------------------
# 1. Virus-only data
# -------------------------
viruses_pf <- dataMeta_pf %>%
  filter(
    kingdom == "Viruses",
    host == "Eukaryota",
    !is.na(family),
    !is.na(species),
    family != "",
    species != "",
    !is.na(sample.name),
    !is.na(collection.date),
    !is.na(sample.type)
  ) %>%
  mutate(
    collection.date = as.Date(collection.date, format = "%m/%d/%Y"),
    sample.name = fct_inorder(sample.name)
  )

# -------------------------
# 2. Helper: prep species-level data
# -------------------------
prep_panel_species <- function(df, sample_types) {
  
  df_sub <- df %>%
    filter(sample.type %in% sample_types)
  
  # top 20 families
  top_families <- df_sub %>%
    group_by(family) %>%
    summarise(total_reads = sum(count, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_reads)) %>%
    slice_head(n = 20) %>%
    pull(family)
  
  df_fam <- df_sub %>%
    filter(family %in% top_families)
  
  # top 1 species per family
  top_species <- df_fam %>%
    group_by(family, species) %>%
    summarise(species_reads = sum(count, na.rm = TRUE), .groups = "drop") %>%
    group_by(family) %>%
    slice_max(species_reads, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  df_fam %>%
    semi_join(top_species, by = c("family","species")) %>%
    mutate(family_species = paste(family, species, sep=": ")) %>%
    group_by(collection.date, sample.name, sample.type, family, family_species) %>%
    summarise(reads = sum(count, na.rm=TRUE), .groups="drop") %>%
    group_by(collection.date, sample.name, sample.type) %>%
    mutate(rel_abundance = reads / sum(reads)) %>%
    ungroup() %>%
    mutate(
      collection.date = factor(collection.date,
                               levels = sort(unique(collection.date))
      )
    )
}

# -------------------------
# 3. Build panel datasets
# -------------------------
df_A <- prep_panel_species(
  viruses_pf,
  c("dolphin feces","dolphin serum",
    "sealion feces","sealion serum")
)

df_B <- prep_panel_species(
  viruses_pf,
  c("mussel","San Diego Bay")
)

df_C <- prep_panel_species(
  viruses_pf,
  c("oyster","Barataria Bay")
)

# -------------------------
# 4. Create FAMILY color palette
# -------------------------
all_families <- c(df_A$family, df_B$family, df_C$family) %>%
  unique() %>%
  sort()

family_colors <- setNames(
  qualitative_hcl(length(all_families), palette = "Dark 3"),
  all_families
)

# create color map for family_species
all_family_species <- c(
  df_A$family_species,
  df_B$family_species,
  df_C$family_species
) %>%
  unique()

family_species_colors <- setNames(
  family_colors[sub(":.*","",all_family_species)],
  all_family_species
)

# -------------------------
# 5. Plot function
# -------------------------
make_plot_species <- function(df){
  
  ggplot(df,
         aes(sample.name,
             rel_abundance,
             fill = family_species)) +
    
    geom_col(width=0.9) +
    
    facet_wrap(~collection.date,
               scales="free_x",
               nrow=1) +
    
    scale_y_continuous(
      labels = percent_format(accuracy=1)
    ) +
    
    scale_fill_manual(
      values = family_species_colors,
      drop = TRUE
    ) +
    
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=90,hjust=1,size=15),
      axis.text.y = element_text(size=15),
      axis.title.y = element_text(size=20,face="bold"),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size=18,face="bold"),
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(0.6,"lines"),
      legend.title = element_text(size=18,face="bold"),
      legend.text = element_text(size=16)
    ) +
    
    labs(
      x=NULL,
      y="Relative abundance",
      fill="Virus family : species"
    )
}

# -------------------------
# 6. Panels A, B, C
# -------------------------
pA <- make_plot_species(df_A)
pB <- make_plot_species(df_B)
pC <- make_plot_species(df_C)

# -------------------------
# 7. Combine panels
# -------------------------
combined_plot <- (pA / pB / pC) +
  plot_annotation(tag_levels="A") &
  theme(plot.tag = element_text(size=28,face="bold"))

# -------------------------
# 8. Save figures
# -------------------------
out_dir <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results/R/results"

dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

ggsave(
  filename=file.path(out_dir,
                     "Figure_temporal_Eukaryotic_viral_species_top20families.tiff"),
  plot=combined_plot,
  width=25,
  height=22,
  dpi=600,
  compression="lzw",
  bg="white"
)

ggsave(
  filename=file.path(out_dir,
                     "Figure_temporal_Eukaryotic_viral_species_top20families.pdf"),
  plot=combined_plot,
  width=25,
  height=22,
  device=cairo_pdf
)

combined_plot




############################################################
# Temporal proportions for taxa shown in the figure
# + mean aa and nt percent identity
############################################################

# --- proportions from plotted taxa ---
prop_table <- bind_rows(df_A, df_B, df_C) %>%
  
  group_by(sample.type,
           collection.date,
           family,
           family_species) %>%
  
  summarise(
    reads = sum(reads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  group_by(sample.type, collection.date) %>%
  
  mutate(
    total_reads = sum(reads),
    proportion = reads / total_reads
  ) %>%
  
  ungroup()


# --- identity statistics ---
identity_table <- dataMeta_pf %>%
  
  filter(
    kingdom == "Viruses",
    host == "Eukaryota",
    !is.na(family),
    !is.na(species)
  ) %>%
  
  group_by(family, species) %>%
  
  summarise(
    mean_pident_aa = mean(pident[alnType == "aa"], na.rm = TRUE),
    mean_pident_nt = mean(pident[alnType == "nt"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    mean_pident_aa = ifelse(is.nan(mean_pident_aa), NA, round(mean_pident_aa,2)),
    mean_pident_nt = ifelse(is.nan(mean_pident_nt), NA, round(mean_pident_nt,2)),
    family_species = paste(family, species, sep=": ")
  )


# --- join identities to proportion table ---
final_temporal_table <- prop_table %>%
  
  left_join(identity_table,
            by = c("family","family_species")) %>%
  
  arrange(sample.type,
          collection.date,
          desc(proportion))


print(final_temporal_table, n = 200)


############################################################
# Export table
############################################################

write.csv(
  final_temporal_table,
  file = file.path(
    out_dir,
    "Table_temporal_top20family_species_proportion_identity.csv"
  ),
  row.names = FALSE
)







############################################################
# Table: Top 20 families → top 1 species per family
# Mean aa and nt percent identity
############################################################

library(dplyr)

############################################################
# Table: Top family:species + identity + read support
############################################################

top_species_identity_table <- dataMeta_pf %>%
  
  filter(
    kingdom == "Viruses",
    host == "Eukaryota",
    !is.na(family),
    !is.na(species),
    family != "",
    species != ""
  ) %>%
  
  group_by(sample.type, family, species) %>%
  
  summarise(
    
    total_reads_aligned = sum(count, na.rm = TRUE),
    
    mean_pident_aa = mean(pident[alnType == "aa"], na.rm = TRUE),
    
    mean_pident_nt = mean(pident[alnType == "nt"], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    mean_pident_aa = ifelse(is.nan(mean_pident_aa), NA, mean_pident_aa),
    mean_pident_nt = ifelse(is.nan(mean_pident_nt), NA, mean_pident_nt)
  ) %>%
  
  arrange(sample.type, desc(total_reads_aligned))

print(top_species_identity_table, n = 200)

############################################################
# Export CSV
############################################################

write.csv(
  top_species_identity_table,
  file = file.path(out_dir,
                   "Table_top20_family_species_identity_reads.csv"),
  row.names = FALSE
)












############################################################
# Final Table: Top 20 viral family:species per sample.type
############################################################

identity_summary <- dataMeta_pf %>%
  filter(
    kingdom == "Viruses",
    host == "Eukaryota",
    !is.na(family),
    !is.na(species),
    family != "",
    species != ""
  ) %>%
  group_by(sample.type, family, species) %>%
  summarise(
    total_reads_aligned = sum(count, na.rm = TRUE),
    mean_pident_aa = mean(pident[alnType == "aa"], na.rm = TRUE),
    mean_pident_nt = mean(pident[alnType == "nt"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_pident_aa = ifelse(is.nan(mean_pident_aa), NA, round(mean_pident_aa, 2)),
    mean_pident_nt = ifelse(is.nan(mean_pident_nt), NA, round(mean_pident_nt, 2))
  ) %>%
  group_by(sample.type) %>%
  arrange(desc(total_reads_aligned), family, species, .by_group = TRUE) %>%
  mutate(rank_within_sample = row_number()) %>%
  filter(rank_within_sample <= 20) %>%
  ungroup() %>%
  arrange(sample.type, rank_within_sample)

print(identity_summary, n = 300)

############################################################
# Optional check: should all be <= 20
############################################################

identity_summary %>%
  count(sample.type)

############################################################
# Export CSV
############################################################

write.csv(
  identity_summary,
  file = file.path(
    out_dir,
    "Table_top20_family_species_identity_reads_by_sampletype.csv"
  ),
  row.names = FALSE
)




############################################################
## vOTU Diversity Analysis (95% ANI Clustering)
## Corrected for actual coverage table column names
############################################################

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(vegan)
library(patchwork)
library(tibble)

############################################################
## 0. Set directories
############################################################

base_dir <- "E:/viral_metagenomics_project/katie/hecatomb_out_host/results/results"
votu_dir <- file.path(base_dir, "R/data/vOTU")
results_dir <- file.path(base_dir, "R/results")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

############################################################
## 1. Parse CD-HIT 95% ANI cluster file
############################################################

clstr_lines <- readLines(file.path(votu_dir, "vOTUs_95.fasta.clstr"))

votu_table <- tibble(line = clstr_lines) %>%
  mutate(
    cluster_id = cumsum(str_detect(line, "^>Cluster")),
    contig = str_extract(line, ">[^\\.]+") %>% str_remove("^>")
  ) %>%
  filter(!str_detect(line, "^>Cluster")) %>%
  mutate(vOTU = paste0("vOTU_", cluster_id)) %>%
  select(contig, vOTU)

############################################################
## 2. Load Hecatomb coverage table
############################################################

cov <- read_tsv(file.path(base_dir, "sample_coverage.tsv"))

cat("\nCoverage table columns:\n")
print(colnames(cov))

############################################################
## 3. Join contigs to vOTUs
############################################################

votu_cov <- votu_table %>%
  left_join(cov, by = c("contig" = "Contig"))

# Check join
cat("\nJoin preview:\n")
print(head(votu_cov))

############################################################
## 4. Aggregate counts per sample per vOTU
############################################################

votu_abundance <- votu_cov %>%
  group_by(Sample, vOTU) %>%
  summarise(count = sum(Count, na.rm = TRUE), .groups = "drop")

############################################################
## 5. Remove singleton vOTUs (present in only 1 sample)
############################################################

votu_abundance <- votu_abundance %>%
  group_by(vOTU) %>%
  filter(sum(count > 0) >= 2) %>%
  ungroup()

############################################################
## 6. Build vOTU abundance matrix
############################################################

votu_matrix <- votu_abundance %>%
  pivot_wider(
    names_from = vOTU,
    values_from = count,
    values_fill = 0
  )

votu_mat <- votu_matrix %>%
  column_to_rownames("Sample") %>%
  as.matrix()

cat("\nMatrix dimensions (samples x vOTUs):\n")
print(dim(votu_mat))

############################################################
## 7. Alpha Diversity
############################################################

richness <- rowSums(votu_mat > 0)
shannon  <- diversity(votu_mat, index = "shannon")

alpha_df <- tibble(
  Sample = names(richness),
  richness = richness,
  shannon  = shannon
)



cat("\nTotal vOTUs detected:\n")
print(ncol(votu_mat))

cat("\nRichness statistics:\n")
print(data.frame(
  min = min(richness),
  max = max(richness),
  median = median(richness),
  mean = mean(richness)
))
############################################################
## 8. Presence/Absence Matrix for Jaccard
############################################################

votu_pa <- votu_mat
votu_pa[votu_pa > 0] <- 1

############################################################
## 9. Beta Diversity (Jaccard NMDS)
############################################################

dist_jaccard <- vegdist(votu_pa, method = "jaccard")

nmds <- metaMDS(dist_jaccard, k = 2)

nmds_df <- as.data.frame(nmds$points)
nmds_df$Sample <- rownames(nmds_df)

############################################################
## Remove problematic sample
############################################################

bad_sample <- "S20_d_EXP-NBD104_barcode04"

# Remove from alpha diversity
alpha_df <- alpha_df %>%
  filter(Sample != bad_sample)

# Remove from NMDS dataframe
nmds_df <- nmds_df %>%
  filter(Sample != bad_sample)

# Remove from distance matrix
dist_jaccard_clean <- as.dist(
  as.matrix(dist_jaccard)[
    nmds_df$Sample,
    nmds_df$Sample
  ]
)


############################################################
## 10. Add sample metadata (from dataMeta_pf.csv)
############################################################

meta_full <- read_csv(file.path(base_dir, "dataMeta_pf.csv"),
                      show_col_types = FALSE)

meta <- meta_full %>%
  select(sampleID, sample.type) %>%
  distinct()

# Join metadata
alpha_df <- alpha_df %>%
  left_join(meta, by = c("Sample" = "sampleID"))

nmds_df <- nmds_df %>%
  left_join(meta, by = c("Sample" = "sampleID"))

############################################################
## 11. Remove samples without metadata
############################################################

# Remove NA samples everywhere
alpha_df <- alpha_df %>%
  filter(!is.na(sample.type))

nmds_df <- nmds_df %>%
  filter(!is.na(sample.type))

# Subset distance matrix to cleaned samples
dist_jaccard_clean <- as.dist(
  as.matrix(dist_jaccard)[
    nmds_df$Sample,
    nmds_df$Sample
  ]
)

############################################################
## 12. PERMANOVA
############################################################

perm <- adonis2(dist_jaccard_clean ~ sample.type,
                data = nmds_df)

write.csv(perm,
          file.path(results_dir, "PERMANOVA_vOTU_Jaccard.csv"),
          row.names = FALSE)

############################################################
## 13. Plot A — vOTU Richness
############################################################

p_rich <- ggplot(alpha_df,
                 aes(sample.type, richness, fill = sample.type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = "Sample type",
    y = "vOTU richness (95% ANI)",
    title = "B"
  )


############################################################
## 14. Alpha Diversity Statistical Test (Kruskal–Wallis)
############################################################

# Ensure sample.type is a factor
alpha_df$sample.type <- factor(alpha_df$sample.type)

# Run Kruskal–Wallis test for richness
kw_rich <- kruskal.test(richness ~ sample.type, data = alpha_df)

cat("\nKruskal–Wallis test for vOTU richness:\n")
print(kw_rich)

# Optional: pairwise comparisons (Benjamini-Hochberg correction)
pairwise_rich <- pairwise.wilcox.test(
  alpha_df$richness,
  alpha_df$sample.type,
  p.adjust.method = "BH"
)

cat("\nPairwise Wilcoxon tests (BH corrected):\n")
print(pairwise_rich)

# Save outputs
write.csv(
  data.frame(
    statistic = kw_rich$statistic,
    df = kw_rich$parameter,
    p_value = kw_rich$p.value
  ),
  file.path(results_dir, "KruskalWallis_vOTU_richness.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(pairwise_rich$p.value),
  file.path(results_dir, "Pairwise_Wilcoxon_vOTU_richness.csv")
)



############################################################
## 15. Plot B — NMDS with ellipses (Colorblind Safe)
############################################################
library(paletteer)

# Get exactly 8 Okabe-Ito colors
cb_palette <- as.character(paletteer_d("colorblindr::OkabeIto"))

# Assign names in the same order as factor levels
nmds_df$sample.type <- factor(
  nmds_df$sample.type,
  levels = sort(unique(nmds_df$sample.type))
)

names(cb_palette) <- levels(nmds_df$sample.type)

############################################################
## 16. Plot B — NMDS with ellipses (paletteer)
############################################################

library(paletteer)

ellipse_df <- nmds_df %>%
  group_by(sample.type) %>%
  filter(n() >= 3)

p_nmds <- ggplot(nmds_df,
                 aes(MDS1, MDS2)) +
  
  stat_ellipse(
    data = ellipse_df,
    aes(fill = sample.type),
    geom = "polygon",
    alpha = 0.25,
    level = 0.95,
    color = NA
  ) +
  
  geom_point(aes(color = sample.type),
             size = 3,
             alpha = 0.9) +
  
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Sample type",
    fill  = "Sample type",
    title = "B"
  )
############################################################
## 17. Combine panels
############################################################

final_fig <- p_rich | p_nmds

############################################################
## 18. Save outputs
############################################################

ggsave(
  file.path(results_dir, "Figure_vOTU_diversity_95ANI.pdf"),
  final_fig,
  width = 16,
  height = 7,
  device = cairo_pdf
)

ggsave(
  file.path(results_dir, "Figure_vOTU_diversity_95ANI.tiff"),
  final_fig,
  width = 16,
  height = 7,
  dpi = 600,
  compression = "lzw"
)

############################################################
## 19. Summary output
############################################################

cat("\nPERMANOVA results:\n")
print(perm)

cat("\nRichness summary:\n")
print(summary(alpha_df$richness))

final_fig






