
library(tidyverse)
library(ggplot2)
library(readr)
library(ggplot2)
library(dplyr)
library(patchwork)  # For arranging multiple plots
library(tidyr)
library(readxl)

# Load the .csv file (adjust file path accordingly)
data <- read_csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/abundance_table_species_virus.csv")

data_long <- data %>%
  select(family, genus, species, host, type, all_of(c(
    "01_dolphin_feces_SQK-NBD114-24_barcode15", "02_sealion_feces_SQK-NBD114-24_barcode16", 
    "03_dolphin_serum_SQK-NBD114-24_barcode17", "04_sealion_serum_SQK-NBD114-24_barcode18",
    "S02_SQK-NBD114-24_barcode01", "S07_SQK-NBD114-24_barcode02", 
    "S09_SQK-NBD114-24_barcode03", "S14_SQK-NBD114-24_barcode04", 
    "S17_SQK-NBD114-24_barcode05", "S19_SQK-NBD114-24_barcode06", 
    "S20_SQK-NBD114-24_barcode07", "S20_d_EXP-NBD104_barcode04", 
    "S26_SQK-NBD114-24_barcode08", "S31_SQK-NBD114-24_barcode09", 
    "S32_SQK-NBD114-24_barcode10", "m03_SQK-NBD114-24_barcode02", 
    "m04_SQK-NBD114-24_barcode03", "m05_SQK-NBD114-24_barcode04", 
    "m06_SQK-NBD114-24_barcode05", "m07_SQK-NBD114-24_barcode06", 
    "m08_SQK-NBD114-24_barcode07", "m09_SQK-NBD114-24_barcode08", 
    "m10_SQK-NBD114-24_barcode09", "m11_SQK-NBD114-24_barcode10", 
    "m12_SQK-NBD114-24_barcode11", "m13_SQK-NBD114-24_barcode12", 
    "m14_SQK-NBD114-24_barcode13", "m15_SQK-NBD114-24_barcode14", 
    "m16_SQK-NBD114-24_barcode15", "m17_SQK-NBD114-24_barcode16", 
    "m19_SQK-NBD114-24_barcode18", "m24_SQK-NBD114-24_barcode02", 
    "m25_SQK-NBD114-24_barcode03", "m26_SQK-NBD114-24_barcode04", 
    "m27_SQK-NBD114-24_barcode05", "m28_SQK-NBD114-24_barcode06", 
    "m29_SQK-NBD114-24_barcode07", "m30_SQK-NBD114-24_barcode08", 
    "m31_SQK-NBD114-24_barcode09", "m32_SQK-NBD114-24_barcode10", 
    "m33_SQK-NBD114-24_barcode11", "m34_SQK-NBD114-24_barcode12", 
    "m35_SQK-NBD114-24_barcode13", "m36_SQK-NBD114-24_barcode14", 
    "oy09_SQK-NBD114-24_barcode01", "oy10_SQK-NBD114-24_barcode02", 
    "oy11_SQK-NBD114-24_barcode03", "oy12_SQK-NBD114-24_barcode04", 
    "oy13_SQK-NBD114-24_barcode05", "oy14_SQK-NBD114-24_barcode06", 
    "oy15_SQK-NBD114-24_barcode07", "oy16_SQK-NBD114-24_barcode08", 
    "oy20_SQK-NBD114-24_barcode09", "oy21_SQK-NBD114-24_barcode10", 
    "oy22_SQK-NBD114-24_barcode11", "oy23_SQK-NBD114-24_barcode12", 
    "oy24_SQK-NBD114-24_barcode13", "oy25_SQK-NBD114-24_barcode14", 
    "oy26_SQK-NBD114-24_barcode15", "oy27_SQK-NBD114-24_barcode16", 
    "oy31_SQK-NBD114-24_barcode17", "oy32_SQK-NBD114-24_barcode18", 
    "oy33_SQK-NBD114-24_barcode19", "oy34_SQK-NBD114-24_barcode20", 
    "oy35_SQK-NBD114-24_barcode21", "oy36_SQK-NBD114-24_barcode22", 
    "oy37_SQK-NBD114-24_barcode23", "oy38_SQK-NBD114-24_barcode24", 
    "oy42_SQK-NBD114-24_barcode02", "oy43_SQK-NBD114-24_barcode03", 
    "oy44_SQK-NBD114-24_barcode04", "oy45_SQK-NBD114-24_barcode05", 
    "oy46_SQK-NBD114-24_barcode06", "oy47_SQK-NBD114-24_barcode07", 
    "oy49_SQK-NBD114-24_barcode08", "oy50_SQK-NBD114-24_barcode09", 
    "oy51_SQK-NBD114-24_barcode10", "oy52_SQK-NBD114-24_barcode11", 
    "oy53_SQK-NBD114-24_barcode12", "oy54_SQK-NBD114-24_barcode13", 
    "oy55_SQK-NBD114-24_barcode14", "oy56_SQK-NBD114-24_barcode15", 
    "oy58_SQK-NBD114-24_barcode16", "oy59_SQK-NBD114-24_barcode17", 
    "oy60_SQK-NBD114-24_barcode18", "oy61_SQK-NBD114-24_barcode19", 
    "oy63_SQK-NBD114-24_barcode21", "oy64_SQK-NBD114-24_barcode22", 
    "sd1_SQK-NBD114-24_barcode16", "sd2_SQK-NBD114-24_barcode17", 
    "sd3_SQK-NBD114-24_barcode18", "sd4_SQK-NBD114-24_barcode19"
  ))
  ) %>%
  mutate(across(c(family, genus, species, host), as.character)) %>%
  pivot_longer(
    cols = -c(family, genus, species, host, type),
    names_to = "sample",
    values_to = "value"
  )

write.csv(
  data_long, 
  file = "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_data_long.csv", 
  row.names = FALSE
)


# Load the cleaned up data_long and add in all the sample names
virus_data <- read_csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_data_long.csv")



###Water quality parameters graphs

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Load the readxl package
library(readxl)


# Read the CSV file
wq_data <- read.csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/allsamples.csv")

# Reshape to long format
wq_data_long <- wq_data %>%
  pivot_longer(cols = starts_with("temp.C"):starts_with("DO.ppm"),  # Adjust the columns to match your data
               names_to = "parameter", 
               values_to = "value")

# Define a plotting function to streamline chart creation
plot_wq <- function(param, title, y_label) {
  ggplot(wq_data_long %>% 
           filter(parameter == param, sample.site != "San Diego"),
         aes(x = as.factor(collection.date), y = value, color = sample.site, shape = sample.site)) +
    geom_point(size = 3) +
    labs(title = title, x = "Collection Date", y = y_label) +
    scale_x_discrete(labels = function(x) format(as.Date(x), "%Y-%m-%d")) +  # Show only actual dates
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Generate individual plots
chart_temp <- plot_wq("temp.C", "Temperature (°C)", "Temperature (°C)")
chart_ph <- plot_wq("ph", "pH", "pH")
chart_conductivity <- plot_wq("conductivity", "Conductivity (μS/cm)", "Conductivity (μS/cm)")
chart_tds <- plot_wq("tds.ppm", "TDS (ppm)", "TDS (ppm)")
chart_salinity <- plot_wq("salinity.psu", "Salinity (PSU)", "Salinity (PSU)")
chart_DO <- plot_wq("DO.ppm", "Dissolved Oxygen (ppm)", "Dissolved Oxygen (ppm)")

# Combine the charts using patchwork
combined_plot <- (chart_temp + chart_ph) / (chart_conductivity + chart_tds) / (chart_salinity + chart_DO)

# Save the combined plot as a high-resolution PNG
ggsave("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/wq_combined_scatterplot.png", 
       plot = combined_plot, width = 12, height = 16, dpi = 300)





##Facet together the samples from Barataria Bay and San Diego

# Ensure all combinations of 'type' and 'sample.type' are included
type_counts <- virus_data %>%
  group_by(type, sample.type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(type, sample.type, fill = list(count = 0))  # Ensure all combinations are included

# Log-transform the counts of types (adding 1 to avoid log(0) issues)
type_counts <- type_counts %>%
  mutate(
    log_count = ifelse(count == 0, 0, log(count + 1))  # Ensure log_count is 0 for zero counts
  )

# Add the region grouping with handling for "mussel"
type_counts <- type_counts %>%
  mutate(
    region = case_when(
      sample.type %in% c("bay seawater", "oyster") ~ "Barataria Bay",
      sample.type %in% c("dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel") ~ "San Diego",  # Include mussel in San Diego
      TRUE ~ "Other"  # Handle any other unexpected sample types
    ),
    sample.type = factor(sample.type, levels = c(
      "bay seawater", "oyster",
      "dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel"
    )),
    region = factor(region, levels = c("Barataria Bay", "San Diego"))
  )

# Filter out unwanted regions if necessary
type_counts <- type_counts %>%
  filter(region %in% c("Barataria Bay", "San Diego"))

# Re-plot with cleaned data
ggplot(type_counts, aes(x = sample.type, y = type, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(
    colors = c("white", "blue", "red"),
    name = "Log(Virus Counts)",
    na.value = "gray90"
  ) +
  labs(
    title = NULL,
    x = "Sample Type",
    y = "Virus Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")




##violin plots for read data from all samples
# Read the CSV file
wq_data <- read.csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/allsamples.csv", stringsAsFactors = FALSE)

# Check for NA values before pivoting
wq_data <- wq_data %>%
  mutate(
    total.bases = as.numeric(gsub(",", "", total.bases)),  
    total.reads = as.numeric(gsub(",", "", total.reads)),
    max.read.length.bp = as.numeric(gsub("[^0-9]", "", max.read.length.bp)),  
    mean.read.length = as.numeric(mean.read.length),
    mean.read.quality = as.numeric(mean.read.quality),  # Ensure numeric
    read.length.N50 = as.numeric(read.length.N50)
  ) %>%
  mutate(
    log_total_bases = log10(total.bases),
    log_total_reads = log10(total.reads),
    log_max_read_length = log10(max.read.length.bp),  
    log_read_length_N50 = log10(read.length.N50)
  )

# Check for NAs
print(colSums(is.na(wq_data[, c("mean.read.quality", "mean.read.length", 
                                "log_total_bases", "log_total_reads", 
                                "log_max_read_length", "log_read_length_N50")])))

# Replace NAs with a placeholder if necessary
wq_data <- wq_data %>%
  mutate(mean.read.quality = ifelse(is.na(mean.read.quality), 0, mean.read.quality))

# Pivot to long format
wq_data_long <- wq_data %>%
  pivot_longer(cols = c(log_total_bases, log_total_reads, log_max_read_length, 
                        mean.read.length, mean.read.quality, log_read_length_N50),
               names_to = "Metric", values_to = "Value")

# Check if mean.read.quality is still present
table(wq_data_long$Metric)

# Load ggplot2 library
library(ggplot2)

# Create the violin plot
ggplot(wq_data_long, aes(x = sample.type, y = Value, fill = sample.type)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot instead of boxplot
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +  # Add jitter points
  facet_wrap(~ Metric, scales = "free_y", labeller = as_labeller(c(
    log_total_bases = "Log10 Total Bases (bp)",
    log_total_reads = "Log10 Total Reads",
    log_max_read_length = "Log10 Max Read Length (bp)",
    mean.read.length = "Mean Read Length (bp)",
    mean.read.quality = "Mean Read Quality",
    log_read_length_N50 = "Log10 Read Length N50 (bp)"
  ))) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Sample Type",
    y = "Value"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),  # White background
    plot.background = element_rect(fill = "white")   # White background for the whole plot
  )

# Define the file path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/violinplot_metrics.png"

# Save the plot
ggsave(output_path, width = 10, height = 8, dpi = 300)



# Calculate the mean for each metric and sample.type
highest_mean <- wq_data_long %>%
  group_by(Metric, sample.type) %>%
  summarize(mean_value = mean(Value, na.rm = TRUE)) %>%
  # Find the sample.type with the highest mean for each Metric
  group_by(Metric) %>%
  slice(which.max(mean_value)) %>%
  ungroup()  # Remove grouping

# Print the result
print(highest_mean)


# Count the number of unique sample.types
sample_type_count <- wq_data_long %>%
  summarize(unique_sample_types = n_distinct(sample.type))

# Print the result
print(sample_type_count)



##Violin Plots for total viral read data

# Read the CSV file
alignment_data <- read.csv("C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/alignment_stats_merged.csv", stringsAsFactors = FALSE)


# Create the violin plot with log transformation and mean bars
#viral reads
viral_reads <- ggplot(alignment_data, aes(x = sample.type, y = log(number.of.reads + 1), fill = sample.type)) +  
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming
 # geom_jitter(width = 0.2, alpha = 0.5, size = 1) +  # Add jitter points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "black", fatten = 2) +  # Add mean bars
  scale_y_continuous(
    breaks = seq(0, ceiling(max(log(alignment_data$number.of.reads + 1))), by = 1)  # Set tick marks at intervals of 1
  ) +
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Log(Aligned Reads + 1)",
    fill = "Sample Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"  # Remove legend if unnecessary
  )

# Display the plot
viral_reads



##viral coverage violin plot
viral_coverage <- ggplot(alignment_data, aes(x = sample.type, y = log(covbases + 1), fill = sample.type)) +  
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming
  # geom_jitter(width = 0.2, alpha = 0.5, size = 1) +  # Add jitter points
  stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "black", fatten = 2) +  # Add mean bars
  scale_y_continuous(
    breaks = seq(0, ceiling(max(log(alignment_data$number.of.reads + 1))), by = 1)  # Set tick marks at intervals of 1
  ) +
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Log(Total Aligned Bases(bp) + 1)",
    fill = "Sample Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"  # Remove legend if unnecessary
  )
 viral_coverage
 
 
 
 # Viral percent coverage plot with log transformation
 viral_percent_coverage <- ggplot(alignment_data, aes(x = sample.type, y = log(pc.coverage + 1), fill=sample.type)) +  
   geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming
   stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "black", fatten = 2) +  # Add mean bars
   scale_y_continuous(
     breaks = seq(0, log(100 + 1), by = 0.5),  # Set tick marks at log-based intervals
     labels = scales::comma_format()  # Optional: Format labels as commas (e.g., 10,000)
   ) +
   theme_minimal() +
   labs(
     x = "Sample Type",
     y = "Log(Percent Coverage + 1)",
     fill = "Sample Type"
   ) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
     axis.title = element_text(size = 14),
     axis.text = element_text(size = 12),
     legend.position = "none"  # Remove legend if unnecessary
   )
 
 # Display the plot
 viral_percent_coverage
 

 
 # Viral mean depth violin plot with log transformation
 viral_mean_depth <- ggplot(alignment_data, aes(x = sample.type, y = log10(meandepth + 0.1), fill = sample.type)) +  
   geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming
   stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "black", fatten = 2) +  # Add mean bars
   scale_y_continuous(
     breaks = log10(c(0.1, 0.5,1, 10, 100, 1000, 10000, 100000)),  # Customize breaks to show more numbers on y-axis
     labels = scales::comma_format()  # Format labels as commas (e.g., 10,000)
   ) +
   theme_minimal() +
   labs(
     x = "Sample Type",
     y = "Log10(Mean Depth + 0.1)",  # Updated y-axis label
     fill = "Sample Type"
   ) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
     axis.title = element_text(size = 14),
     axis.text = element_text(size = 12),
     legend.position = "none"  # Remove legend if unnecessary
   )
 
 # Display the plot
 viral_mean_depth
 
 ###add all the viral read alignments together into one figure
 # Load required library
 library(gridExtra)
 library(grid)
 
 # Combine the plots
 combined_plot <- grid.arrange(
   arrangeGrob(viral_reads + ggtitle("A"), 
               viral_coverage + ggtitle("B"), 
               viral_percent_coverage + ggtitle("C"), 
               viral_mean_depth + ggtitle("D"), 
               ncol = 2, nrow = 2)  # Layout: 2 columns, 2 rows
 )
 
 # Save the combined plot
 ggsave(
   "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/aligned_viral_reads_violin_plot_log_with_mean_combined.png",
   plot = combined_plot,
   width = 14,  # Adjust the width and height as needed
   height = 10,
   dpi = 300
 )
 



 
 
 ####Bubble plot with alignments and abundance data
 # Filter the data
 filtered_data <- virus_data %>%
   filter(host == "vertebrate")
 
 # Create the bubble plot for host
 a <- ggplot(virus_data, aes(x = sample.type, y = host, size = counts, color = sample.type)) +
   geom_point(alpha = 0.7) +  # Creates the bubble plot
   scale_size_continuous(range = c(3, 20)) +  # Adjust the bubble sizes
   labs(
     title = "D",
     x = "Sample Type", 
     y = "Vertebrate Virus Host", 
     size = "Alignment Count"
   ) +
   theme_minimal() +  # Optional: for a cleaner look
   theme(
     axis.text.x = element_text(angle = 45, size = 16, face = "bold"),
     axis.text.y = element_text(size = 16, face = "bold"),
     axis.title.x = element_text(size = 16, face = "bold"),
     axis.title.y = element_text(size = 16, face = "bold"),
     plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
     strip.placement = "outside",
     strip.background = element_blank(),
     strip.text = element_text(size = 15, face = "bold"),
     legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
     legend.text = element_text(size = 15)  # Increase legend text size
   ) +
   guides(size = guide_legend(title = "Alignment Count"), 
          fill = guide_legend(title = "Sample Type"))  # Add "Sample Type" legend title
 
 # Create the bubble plot for virus type
 b <- ggplot(virus_data, aes(x = sample.type, y = type, size = counts, color = sample.type)) +
   geom_point(alpha = 0.7) +  # Creates the bubble plot
   scale_size_continuous(range = c(3, 20)) +  # Adjust the bubble sizes
   labs(title = "B",
        x = "Sample Type", 
        y = "Vertebrate Virus Type", 
        size = "Alignment Count") +
   theme_minimal() +  # Optional: for a cleaner look
   theme(
     axis.text.x = element_text(angle = 45, size = 16, face = "bold"),
     axis.text.y = element_text(size = 16, face = "bold"),
     axis.title.x = element_text(size = 16, face = "bold"),
     axis.title.y = element_text(size = 16, face = "bold"),
     plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
     strip.placement = "outside",
     strip.background = element_blank(),
     strip.text = element_text(size = 15, face = "bold"),
     legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
     legend.text = element_text(size = 15)  # Increase legend text size
   ) +
   guides(size = guide_legend(title = "Alignment Count"), 
          fill = guide_legend(title = "Sample Type"))  # Add "Sample Type" legend title
 
 
 # Create the bubble plot for only vertebrate families
 c <- ggplot(filtered_data, aes(x = sample.type, y = family, size = counts, color = sample.type)) +
   geom_point(alpha = 0.7) +  # Creates the bubble plot
   scale_size_continuous(range = c(3, 20)) +  # Adjust the bubble sizes
   labs(title = "F",
        x = "Sample Type", 
        y = "Vertebrate Virus Family", 
        size = "Alignment Count") +
   theme_minimal() +  # Optional: for a cleaner look
   theme(
     axis.text.x = element_text(angle = 45, size = 16, face = "bold"),
     axis.text.y = element_text(size = 16, face = "bold"),
     axis.title.x = element_text(size = 16, face = "bold"),
     axis.title.y = element_text(size = 16, face = "bold"),
     plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
     strip.placement = "outside",
     strip.background = element_blank(),
     strip.text = element_text(size = 15, face = "bold"),
     legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
     legend.text = element_text(size = 15)  # Increase legend text size
   ) +
   guides(size = guide_legend(title = "Alignment Count"), 
          fill = guide_legend(title = "Sample Type"))  # Add "Sample Type" legend title
 
 
 # Create the bubble plot for only vertebrate genus
 d <- ggplot(filtered_data, aes(x = sample.type, y = genus, size = counts, color = sample.type)) +
   geom_point(alpha = 0.7) +  # Creates the bubble plot
   scale_size_continuous(range = c(3, 20)) +  # Adjust the bubble sizes
   labs(title = "H",
        x = "Sample Type", 
        y = "Vertebrate Virus Genus", 
        size = "Alignment Count") +
   theme_minimal() +  # Optional: for a cleaner look
   theme(
     axis.text.x = element_text(angle = 45, size = 16, face = "bold"),
     axis.text.y = element_text(size = 16, face = "bold"),
     axis.title.x = element_text(size = 16, face = "bold"),
     axis.title.y = element_text(size = 16, face = "bold"),
     plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
     strip.placement = "outside",
     strip.background = element_blank(),
     strip.text = element_text(size = 15, face = "bold"),
     legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
     legend.text = element_text(size = 15)  # Increase legend text size
   ) +
   guides(size = guide_legend(title = "Alignment Count"), 
          fill = guide_legend(title = "Sample Type"))  # Add "Sample Type" legend title
 
 
 

## Heatmap with virus type and sample type

# Ensure all combinations of 'type' and 'sample.type' are included
type_counts <- virus_data %>%
  group_by(type, sample.type) %>%
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%  # Sum existing counts
  complete(type, sample.type, fill = list(count = 0))  # Ensure all combinations are included

# Log-transform the counts of virus types (adding 1 to avoid log(0) issues)
type_counts <- type_counts %>%
  mutate(
    log_count = ifelse(count == 0, 0, log(count + 1))  # Ensure log_count is 0 for zero counts
  )

# Add the region grouping
type_counts <- type_counts %>%
  mutate(
    region = case_when(
      sample.type %in% c("bay seawater", "oyster") ~ "Barataria Bay",
      sample.type %in% c("dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel") ~ "San Diego",  # Include mussel in San Diego
      TRUE ~ "Other"  # Handle any other unexpected sample types
    ),
    sample.type = factor(sample.type, levels = c(
      "bay seawater", "oyster",
      "dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel"
    )),
    region = factor(region, levels = c("Barataria Bay", "San Diego"))
  )

# Filter out unwanted regions if necessary
type_counts <- type_counts %>%
  filter(region %in% c("Barataria Bay", "San Diego"))

# Re-plot with cleaned data
ggplot(type_counts, aes(x = sample.type, y = type, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(
    colors = c("white", "blue", "red"),
    name = "Log(Virus Counts)",
    na.value = "gray90"
  ) +
  labs(
    title = NULL,
    x = "Sample Type",
    y = "Virus Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")




###Heatmap with virus host and sample type

# Ensure all combinations of 'host' and 'sample.type' are included
host_counts <- virus_data %>%
  group_by(host, sample.type) %>%
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%  # Sum existing counts
  complete(host, sample.type, fill = list(count = 0))  # Ensure all combinations are included

# Log-transform the counts of hosts (adding 1 to avoid log(0) issues)
host_counts <- host_counts %>%
  mutate(
    log_count = ifelse(count == 0, 0, log(count + 1))  # Ensure log_count is 0 for zero counts
  )

# Add the region grouping
host_counts <- host_counts %>%
  mutate(
    region = case_when(
      sample.type %in% c("bay seawater", "oyster") ~ "Barataria Bay",
      sample.type %in% c("dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel") ~ "San Diego",  # Include mussel in San Diego
      TRUE ~ "Other"  # Handle any other unexpected sample types
    ),
    sample.type = factor(sample.type, levels = c(
      "bay seawater", "oyster",
      "dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel"
    )),
    region = factor(region, levels = c("Barataria Bay", "San Diego"))
  )

# Filter out unwanted regions if necessary
host_counts <- host_counts %>%
  filter(region %in% c("Barataria Bay", "San Diego"))

# Re-plot with cleaned data
ggplot(host_counts, aes(x = sample.type, y = host, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(
    colors = c("white", "blue", "red"),
    name = "Log(Virus Counts)",
    na.value = "gray90"
  ) +
  labs(
    title = NULL,
    x = "Sample Type",
    y = "Virus Host"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")


###Heatmap with vertebrate virus family and sample type

# Filter data to include only rows where host is "vertebrate"
family_counts <- virus_data %>%
  filter(host == "vertebrate") %>%  # Filter to only vertebrate hosts
  group_by(family, sample.type) %>%
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%  # Sum existing counts
  complete(family, sample.type, fill = list(count = 0))  # Ensure all combinations are included

# Log-transform the counts of families (adding 1 to avoid log(0) issues)
family_counts <- family_counts %>%
  mutate(
    log_count = ifelse(count == 0, 0, log(count + 1))  # Ensure log_count is 0 for zero counts
  )

# Add the region grouping
family_counts <- family_counts %>%
  mutate(
    region = case_when(
      sample.type %in% c("bay seawater", "oyster") ~ "Barataria Bay",
      sample.type %in% c("dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel") ~ "San Diego",  # Include mussel in San Diego
      TRUE ~ "Other"  # Handle any other unexpected sample types
    ),
    sample.type = factor(sample.type, levels = c(
      "bay seawater", "oyster",
      "dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel"
    )),
    region = factor(region, levels = c("Barataria Bay", "San Diego"))
  )

# Remove 'unclassified' from family and reorder the remaining families alphabetically
family_counts <- family_counts %>%
  filter(family != "unclassified") %>%
  mutate(
    family = factor(family, levels = sort(unique(family)))  # Reorder alphabetically
  )

# Filter out unwanted regions if necessary
family_counts <- family_counts %>%
  filter(region %in% c("Barataria Bay", "San Diego"))

# Re-plot with cleaned data
ggplot(family_counts, aes(x = sample.type, y = family, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(
    colors = c("white", "blue", "red"),
    name = "Log(Virus Counts)",
    na.value = "gray90"
  ) +
  labs(
    title = NULL,
    x = "Sample Type",
    y = "Virus Family"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  )+
  facet_grid(. ~ region, scales = "free_x", space = "free_x")


###heatmap with vertebrate virus genus
genus_counts <- virus_data %>%
  filter(host == "vertebrate") %>%  # Filter to only vertebrate hosts
  group_by(genus, sample.type) %>%
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%  # Sum existing counts
  complete(genus, sample.type, fill = list(count = 0))  # Ensure all combinations are included

# Log-transform the counts of genera (adding 1 to avoid log(0) issues)
genus_counts <- genus_counts %>%
  mutate(
    log_count = log(count + 1)  # Log-transform to avoid log(0)
  )

# Add the region grouping
genus_counts <- genus_counts %>%
  mutate(
    region = case_when(
      sample.type %in% c("bay seawater", "oyster") ~ "Barataria Bay",
      sample.type %in% c("dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel") ~ "San Diego",  # Include mussel in San Diego
      TRUE ~ "Other"  # Handle any other unexpected sample types
    ),
    sample.type = factor(sample.type, levels = c(
      "bay seawater", "oyster",
      "dolphin feces", "dolphin serum", "sd seawater", "sealion feces", "sealion serum", "mussel"
    )),
    region = factor(region, levels = c("Barataria Bay", "San Diego"))
  )

# Remove 'unclassified' or NA values from genus and reorder the remaining genera alphabetically
genus_counts <- genus_counts %>%
  filter(!is.na(genus) & genus != "unclassified") %>%
  mutate(
    genus = factor(genus, levels = sort(unique(genus)))  # Reorder alphabetically
  )

# Filter out unwanted regions if necessary
genus_counts <- genus_counts %>%
  filter(region %in% c("Barataria Bay", "San Diego"))

# Re-plot with cleaned data
 ggplot(genus_counts, aes(x = sample.type, y = genus, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(
    colors = c("white", "blue", "red"),
    name = "Log(Virus Counts)",
    na.value = "gray90"
  ) +
  labs(
    x = "Sample Type",
    y = "Virus Genus"
  ) +
  theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
     axis.text.y = element_text(size = 16, face = "bold"),
     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
     strip.placement = "outside",
     strip.background = element_blank(),
     strip.text = element_text(size = 12, face = "bold")
   ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")




##Facet all the heatmaps together

library(patchwork)

# Plot for Virus Counts by Type and Sample Type
plot_A <- ggplot(type_counts, aes(x = sample.type, y = type, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(colors = c("white", "blue", "red"), name = "Log(Virus Counts)", na.value = "gray90") +
  labs(title = "A", x = "Sample Type", y = "Virus Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")

# Plot for Virus Counts by Host and Sample Type
plot_B <- ggplot(host_counts, aes(x = sample.type, y = host, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(colors = c("white", "blue", "red"), name = "Log(Virus Counts)", na.value = "gray90") +
  labs(title = "C", x = "Sample Type", y = "Virus Host") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")

# Plot for Virus Counts by Family (Vertebrate Hosts Only) and Sample Type
plot_C <- ggplot(family_counts, aes(x = sample.type, y = family, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(colors = c("white", "blue", "red"), name = "Log(Virus Counts)", na.value = "gray90") +
  labs(title = "E", x = "Sample Type", y = "Vertebrate Virus Family") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")

# Plot for Virus Counts by Genus (Vertebrate Hosts Only) and Sample Type
plot_D <- ggplot(genus_counts, aes(x = sample.type, y = genus, fill = log_count)) +
  geom_tile(color = "gray80") +
  scale_fill_gradientn(colors = c("white", "blue", "red"), name = "Log(Virus Counts)", na.value = "gray90") +
  labs(title = "G", x = "Sample Type", y = "Vertebrate Virus Genus") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 14),  # Increase legend text size
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x")

# Combine all plots into one grid
combined_plot <- plot_A + plot_B + plot_C + plot_D + plot_layout(ncol = 2)

# Define the file path where you want to save the image
file_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/combined_heatmap.png"

# Save the combined plot as a high-resolution image (e.g., PNG) at the specified location
ggsave(file_path, combined_plot, width = 16, height = 12, dpi = 300)



##facet bubble plot and heatmaps together

bubble_heatmap <- plot_A + b + plot_B + a + plot_C + c + plot_D + d + plot_layout(ncol = 2) 

# Define the file path where you want to save the image
file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/bubble_plot_heatmap.png"

# Save the combined plot as a high-resolution image (e.g., PNG) at the specified location
ggsave(file, bubble_heatmap, width = 30, height = 42, dpi = 650)


ggsave(file, bubble_heatmap, width = 25, height = 42, dpi = 700, device = "tiff")







##########stacked bar plots for the different sample types and dates
head(virus_data$collection.date)

# Filter and summarize data, including only vertebrate viruses and excluding unclassified viruses
mammal <- virus_data %>%
  filter(sample.name %in% c("dolphin feces", "dolphin serum", "sealion feces", "sealion serum")) %>%
  filter(family != "unclassified") %>%  # Exclude unclassified viruses
  filter(host == "vertebrate") %>%  # Keep only vertebrate family
  group_by(sample.name, family) %>%  # Group by sample name and family
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  filter(count > 0)  # Exclude zero counts

# Define the custom color mapping for the virus families
mammal_colors <- c(
  "Adenoviridae" = "olivedrab3",
  "Alloherpesviridae" = "navajowhite3",
  "Arenaviridae" = "black",
  "Circoviridae" = "mediumorchid",
  "Orthoherpesviridae" = "darkseagreen4",
  "Papillomaviridae" = "blue",
  "Poxviridae" = "gold",
  "Retroviridae" = "lightyellow4"
)

# Ensure the 'family' column only includes families from the color palette
mammal$family <- factor(mammal$family, levels = names(mammal_colors))

# Percent stacked barplot
ggplot(mammal, aes(x = sample.name, y = count, fill = family)) +  
  geom_bar(position = "fill", stat = "identity") +  # Use position = "fill" for percentage
  scale_fill_manual(values = mammal_colors, drop = FALSE) +  
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Percentage of Viral Families",
    fill = "Vertebrate Virus Family"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

##stacked barplot for mussels and sd seawater

# Define the custom color mapping for the virus families
colors <- c(
  "Adenoviridae" = "olivedrab3",
  "Alloherpesviridae" = "navajowhite3",
  "Arenaviridae" = "black",
  "Circoviridae" = "mediumorchid",
  "Flaviviridae" = "orange",
  "Iridoviridae" = "turquoise4",
  "Orthoherpesviridae" = "darkseagreen4",
  "Papillomaviridae" = "blue",
  "Parvoviridae" = "tan4",
  "Peribunyaviridae" = "#17becf",
  "Phenuiviridae" = "pink",
  "Poxviridae" = "gold",
  "Retroviridae" = "lightyellow4",
  "Tobaniviridae" = "lightsalmon"
)

mussel_seawater <- virus_data %>%
 filter(sample.type %in% c("mussel", "sd seawater")) %>%
  filter(family != "unclassified") %>%  # Exclude unclassified viruses
  filter(host == "vertebrate") %>%  # Keep only vertebrate family
  group_by(sample.name, family, collection.date ) %>%  # Group by sample name and family
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  filter(count > 0)  # Exclude zero counts

# Convert collection.date to yyyy-mm-dd format
mussel_seawater$collection.date <- as.Date(mussel_seawater$collection.date)


# Create the stacked barplot with sample on the bottom x-axis and collection.date on the top x-axis
ggplot(mussel_seawater, aes(x = sample.name, y = count, fill = family)) +
  geom_bar(position="fill", stat = "identity") +  # Use actual counts and stack bars
  facet_grid(~ collection.date, scales = "free_x") +  # Separate by collection.date on the top x-axis
  scale_fill_manual(values = colors) +  # Apply the custom color palette
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Vertebrate Virus Family Counts",
    title = NULL,
    fill = "Vertebrate Virus Family"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),  # Adjust size as needed
    axis.text.x = element_text(angle = 90, hjust = 1, size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.position = "right",
    strip.text.x = element_text(size = 15),
    strip.background = element_blank()
  )




##stacked barplot for oysters and bay seawater
# Filter and summarize data for oyster and bay seawater
oyster_seawater <- virus_data %>%
filter(sample.type %in% c("oyster", "bay seawater")) %>%
  filter(family != "unclassified") %>%  # Exclude unclassified viruses
  filter(host == "vertebrate") %>%  # Keep only vertebrate family
  group_by(sample.name, family, collection.date ) %>%  # Group by sample name and family
  summarise(count = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  filter(count > 0)  # Exclude zero counts


# Create the stacked barplot with sample on the bottom x-axis and collection.date on the top x-axis
ggplot(oyster_seawater, aes(x = sample.name, y = count, fill = family)) +
  geom_bar(position="fill", stat = "identity") +  # Use actual counts and stack bars
  facet_grid(~ collection.date, scales = "free_x") +  # Separate by collection.date on the top x-axis
  scale_fill_manual(values = colors) +  # Apply the custom color palette
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Vertebrate Virus Family Counts",
    title = NULL,
    fill = "Vertebrate Virus Family"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),  # Adjust size as needed
    axis.text.x = element_text(angle = 90, hjust = 1, size = 18),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.position = "right",
    strip.text.x = element_text(size = 15),
    strip.background = element_blank()
  )



### add all the stacked barplots together
# Load the patchwork library
library(patchwork)

# Mammal plot
mammal_plot <- ggplot(mammal, aes(x = sample.name, y = count, fill = family)) +  
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = mammal_colors, drop = FALSE) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Viral Abundance",
    fill = "Vertebrate Virus Family",
    title = "A" # Optional: add a title label
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Uniform title size for all plots
    axis.text.x = element_text(angle = 90, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.position = "right",
    strip.text.x = element_text(size = 18),
    strip.background = element_blank()
  )

# Mussel and seawater plot
mussel_seawater_plot <- ggplot(mussel_seawater, aes(x = sample.name, y = count, fill = family)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~ collection.date, scales = "free_x") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Viral Abundance",
    fill = "Vertebrate Virus Family",
    title = "B"  # Optional: add a title label
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Uniform title size for all plots
    axis.text.x = element_text(angle = 90, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.position = "right",
    strip.text.x = element_text(size = 18),
    strip.background = element_blank()
  )

# Oyster and seawater plot
oyster_seawater_plot <- ggplot(oyster_seawater, aes(x = sample.name, y = count, fill = family)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~ collection.date, scales = "free_x") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(
    x = "Sample Type",
    y = "Viral Abundance",
    fill = "Vertebrate Virus Family",
    title = "C"  # Optional: add a title label
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Uniform title size for all plots
    axis.title.x=element_text(size = 20),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.position = "right",
    strip.text.x = element_text(size = 18),
    strip.background = element_blank()
  )

# Combine plots vertically with labels A, B, C
combined_plot <- (mammal_plot / mussel_seawater_plot / oyster_seawater_plot) 

# Display the combined plot
combined_plot


# Save the combined plot as a PNG
ggsave(
  "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/combined_plot_vertical_with_legends_removed_for_B_and_C.png",
  plot = combined_plot,
  width = 20,  # Adjust width as needed for vertical layout
  height = 30,  # Increase height for the stacked plots
  dpi = 300  # High-resolution output
)







####ALPHA DIVERSITY PLOTS

# Load necessary packages
library(vegan)
library(ggplot2)

# Ensure only numeric columns for diversity calculation
virus_data_matrix <- virus_data[, sapply(virus_data, is.numeric)]

# Replace negative values with 0
virus_data_matrix[virus_data_matrix < 0] <- 0

# Replace NA values with 0
virus_data_matrix[is.na(virus_data_matrix)] <- 0

# Check the data
summary(virus_data_matrix)

# Calculate alpha diversity (Shannon Index)
alpha_diversity <- diversity(virus_data_matrix, index = "shannon")

# Add alpha diversity scores to your data
virus_data_with_alpha <- data.frame(sample = sample_data, alpha_diversity = alpha_diversity)

# View the alpha diversity results
head(virus_data_with_alpha)

# Add the sample.site column to the alpha diversity data
virus_data_with_alpha$sample.site <- virus_data$sample.site

# Plot the alpha diversity by sample site using a boxplot
library(ggplot2)

alpha_diversity_plot <- ggplot(virus_data_with_alpha, aes(x = sample.site, y = alpha_diversity)) +
  geom_boxplot(aes(fill = sample.site)) +  # Fill by sample site
  labs(
    title = NULL,
    x = "Sample Site",
    y = "Shannon Index"
  ) +
  theme_minimal(base_size = 15) +  # Set a minimal theme with base size for legibility
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    legend.background = element_rect(fill = "white")  # Set legend background to white
  )

print(alpha_diversity_plot)

# Save the plot in high resolution
ggsave(
  filename = "alpha_diversity_by_sample_site.png",  # File name
  plot = alpha_diversity_plot,                     # Plot object
  path = "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results",  # Path
  dpi = 300,  # Resolution
  width = 10,  # Width in inches
  height = 8   # Height in inches
)

# **Kruskal-Wallis test** to compare Shannon Index across sample sites
kruskal_test <- kruskal.test(alpha_diversity ~ sample.site, data = virus_data_with_alpha)
print(kruskal_test)

# **Post-hoc test** if Kruskal-Wallis is significant (pairwise comparisons)
if (kruskal_test$p.value < 0.05) {
  pairwise_comparisons <- pairwise.wilcox.test(virus_data_with_alpha$alpha_diversity, virus_data_with_alpha$sample.site, p.adjust.method = "BH")
  print(pairwise_comparisons)
}

# Identify the sample site with the highest Shannon Diversity
max_diversity_site <- virus_data_with_alpha %>%
  filter(alpha_diversity == max(alpha_diversity)) %>%
  select(sample.site, alpha_diversity)

# Print the site with highest diversity
print(max_diversity_site)



# Calculate the mean Shannon diversity for each sample site
mean_diversity_by_site <- virus_data_with_alpha %>%
  group_by(sample.site) %>%
  summarize(mean_diversity = mean(alpha_diversity, na.rm = TRUE))

# Print the mean diversity for each sample site
print(mean_diversity_by_site)

# Identify the sample site with the highest mean Shannon Diversity
max_mean_diversity_site <- mean_diversity_by_site %>%
  filter(mean_diversity == max(mean_diversity)) %>%
  select(sample.site, mean_diversity)

# Print the site with the highest mean diversity
print(max_mean_diversity_site)

# Calculate the mean Shannon diversity for each sample site
mean_diversity_by_site <- virus_data_with_alpha %>%
  group_by(sample.site) %>%
  summarise(mean_diversity = mean(alpha_diversity))

# View the mean diversity values
print(mean_diversity_by_site)

# Identify the sample site with the highest mean diversity
max_diversity_site <- mean_diversity_by_site %>%
  filter(mean_diversity == max(mean_diversity))

# Print the site with the highest mean diversity
print(max_diversity_site)






# Visualize the distribution of alpha diversity within the sample site with high variability
library(ggplot2)

# Plot histogram or density plot for the sample site with high outliers
ggplot(virus_data_with_alpha, aes(x = alpha_diversity)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ sample.site) +  # Separate histograms for each sample site
  labs(title = "Distribution of Alpha Diversity across Sample Sites", x = "Shannon Diversity", y = "Frequency")

# Boxplot with outliers marked
ggplot(virus_data_with_alpha, aes(x = sample.site, y = alpha_diversity)) +
  geom_boxplot(aes(fill = sample.site), outlier.colour = "red") +  # Highlight outliers in red
  labs(title = "Alpha Diversity with Outliers", x = "Sample Site", y = "Shannon Diversity")

# Visual Inspection: Histogram
ggplot(virus_data_with_alpha, aes(x = alpha_diversity)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ sample.site) + 
  labs(title = "Histogram of Alpha Diversity", x = "Shannon Diversity", y = "Frequency")

# Q-Q plot for normality check
ggplot(virus_data_with_alpha, aes(sample = alpha_diversity)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ sample.site) +
  labs(title = "Q-Q Plot of Alpha Diversity", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Apply Shapiro-Wilk test for normality to each sample.site
shapiro_results <- virus_data_with_alpha %>%
  group_by(sample.site) %>%
  summarise(
    shapiro_test_p_value = shapiro.test(alpha_diversity)$p.value,
    .groups = "drop"
  )

# Print the results
print(shapiro_results)












##### BETA DIVERSITY PLOTS Principal coordinate analysis (PCoA)

# Inspect the structure of your data
str(virus_data)

# Check for missing or infinite values
any(is.na(virus_data))       # Should return FALSE
any(!is.finite(virus_data))  # Should return FALSE

# Select only numeric columns
numeric_cols <- sapply(virus_data, is.numeric)
numeric_data <- virus_data[, numeric_cols]

# Check for missing or infinite values in numeric data
any(is.na(numeric_data))       # Should return FALSE
any(!is.finite(as.matrix(numeric_data)))  # Should return FALSE


library(dplyr)

# Aggregate counts by sample.type and family
aggregated_data <- virus_data %>%
  group_by(sample.type, genus) %>%
  summarise(total_counts = sum(counts, na.rm = TRUE)) %>%
  ungroup()

# Pivot data to wide format for Bray-Curtis calculation
wide_data <- aggregated_data %>%
  pivot_wider(names_from = genus, values_from = total_counts, values_fill = 0)


library(vegan)

# Extract numeric matrix for Bray-Curtis calculation
count_matrix <- as.matrix(wide_data[,-1])  # Exclude sample.type column

# Add a small pseudocount to avoid zero issues (if needed)
count_matrix <- count_matrix + 1e-6

# Compute Bray-Curtis dissimilarity
bray_curtis_matrix <- vegdist(count_matrix, method = "bray")

# Perform PCoA
pcoa_result <- cmdscale(bray_curtis_matrix, eig = TRUE, k = 2)

# Combine PCoA results with metadata for plotting
pcoa_coordinates <- as.data.frame(pcoa_result$points)
colnames(pcoa_coordinates) <- c("PCoA1", "PCoA2")
pcoa_coordinates$sample.type <- wide_data$sample.type

library(ggplot2)

# Define the file path and name
output_file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/pcoa_plot_high_res.png"

# Create the plot
pcoa_plot <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type)) +
  geom_point(size = 7, alpha = 1.0) +
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot with high resolution
ggsave(output_file, plot = pcoa_plot, dpi = 300, width = 10, height = 8, units = "in")






##adding in sample.site Beta diversity plot


# Aggregate counts by sample.type, genus, and sample.site
aggregated_data <- virus_data %>%
  group_by(sample.type, genus, sample.site) %>%
  summarise(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# Pivot data to wide format for Bray-Curtis calculation
wide_data <- aggregated_data %>%
  pivot_wider(names_from = genus, values_from = total_counts, values_fill = 0)

library(vegan)

# Extract numeric matrix for Bray-Curtis calculation
count_matrix <- as.matrix(wide_data[,-c(1, 2)])  # Exclude sample.type and sample.site columns

# Add a small pseudocount to avoid zero issues (if needed)
count_matrix <- count_matrix + 1e-6

# Compute Bray-Curtis dissimilarity
bray_curtis_matrix <- vegdist(count_matrix, method = "bray")

# Perform PCoA
pcoa_result <- cmdscale(bray_curtis_matrix, eig = TRUE, k = 2)

# Combine PCoA results with metadata for plotting
pcoa_coordinates <- as.data.frame(pcoa_result$points)
colnames(pcoa_coordinates) <- c("PCoA1", "PCoA2")
pcoa_coordinates$sample.type <- wide_data$sample.type
pcoa_coordinates$sample.site <- wide_data$sample.site

library(ggplot2)

# Define custom colors for sample.type
custom_colors <- c(
  "red",       # Sample type 1
  "blue",      # Sample type 2
  "green",     # Sample type 3
  "purple",    # Sample type 4
  "yellow3",    # Sample type 5
  "black",      # Sample type 6
  "brown",     # Sample type 7
  "cyan"       # Sample type 8
)

# Update ggplot with custom colors
ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 6, alpha = 0.9) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "PCoA of Viral Communities (Bray-Curtis Dissimilarity)",
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



# Define the file path and name
output_file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/pcoa_sites_plot_high_res.png"

# Create the plot
pcoa_plot_site <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 10, alpha = 2.0) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Save the plot with high resolution
ggsave(
  filename = output_file,  # File path and name
  plot = pcoa_plot_site,        # Plot object
  dpi = 300,               # Resolution
  width = 10,              # Width in inches
  height = 8,              # Height in inches
  units = "in"             # Units for dimensions
)




#####COmbining Alpha and Beta diversity plots PCoA
# Ensure all required libraries are loaded
library(ggplot2)
library(patchwork)

# Create plot A: Alpha diversity
alpha_diversity_plot <- alpha_diversity_plot  # Use the previously defined alpha diversity plot

# Create plot B: PCoA with sample.type only
pcoa_plot_sample_type <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type)) +
  geom_point(size = 7, alpha = 1.0) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Create plot C: PCoA with sample.type and sample.site
pcoa_plot_sample_type_site <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 10, alpha = 2.0) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Combine all three plots and add labels A, B, and C
combined_plot <- (alpha_diversity_plot + ggtitle("A")) /
  (pcoa_plot_sample_type + ggtitle("B")) /
  (pcoa_plot_sample_type_site + ggtitle("C"))

# Adjust layout and save the combined plot
output_file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/combined_alpha_beta_diversity.png"

ggsave(
  filename = output_file,
  plot = combined_plot,
  dpi = 300,   # High resolution
  width = 15,  # Adjust width for proportions
  height = 20  # Adjust height for proportions
)
# Create plot B: PCoA with sample.type only and ellipses around sample types
pcoa_plot_sample_type <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type)) +
  geom_point(size = 7, alpha = 1.0) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  stat_ellipse(aes(color = sample.type), type = "t", level = 0.95) +  # Add ellipses
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Create plot C: PCoA with sample.type and sample.site, with ellipses
pcoa_plot_sample_type_site <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 10, alpha = 2.0) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  stat_ellipse(aes(color = sample.type), type = "t", level = 0.95) +  # Add ellipses
  labs(
    title = NULL,
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Combine all three plots and add labels A, B, and C
combined_plot <- (alpha_diversity_plot + ggtitle("A")) /
  (pcoa_plot_sample_type + ggtitle("B")) /
  (pcoa_plot_sample_type_site + ggtitle("C"))

# Adjust layout and save the combined plot
output_file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/combined_alpha_beta_diversity.png"

ggsave(
  filename = output_file,
  plot = combined_plot,
  dpi = 300,   # High resolution
  width = 15,  # Adjust width for proportions
  height = 20  # Adjust height for proportions
)








##NMDS with Bray Curtis
library(vegan)
library(ggplot2)
library(dplyr)

# Aggregate counts by sample.type and genus
aggregated_data <- virus_data %>%
  group_by(sample.type, genus, sample.site) %>%
  summarise(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# Pivot data to wide format
wide_data <- aggregated_data %>%
  pivot_wider(names_from = genus, values_from = total_counts, values_fill = 0)

# Select only numeric columns for the count matrix
count_matrix <- as.matrix(wide_data %>% select(-sample.type, -sample.site))

# Add a small pseudocount to avoid zero issues
count_matrix <- count_matrix + 1e-6



# Compute Bray-Curtis dissimilarity matrix
bray_curtis_matrix <- vegdist(count_matrix, method = "bray")

# Perform NMDS
set.seed(42)  # For reproducibility
nmds_result <- metaMDS(bray_curtis_matrix, k = 2, trymax = 100)

# Extract NMDS scores
nmds_coordinates <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_coordinates$sample.type <- wide_data$sample.type
nmds_coordinates$sample.site <- wide_data$sample.site

# Define custom colors for sample types
custom_colors <- c(
  "red", "blue", "green", "purple", 
  "yellow3", "black", "brown", "cyan"
)

# Plot NMDS without ellipsoids
nmds_plot <- ggplot(nmds_coordinates, aes(x = NMDS1, y = NMDS2, color = sample.type, shape = sample.site)) +
  geom_point(size = 6, alpha = 0.9) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "NMDS of Viral Communities (Bray-Curtis Dissimilarity)",
    x = "NMDS1",
    y = "NMDS2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Define the file path and name
output_file_nmds <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/nmds_plot_without_ellipsoids.png"

# Save the NMDS plot
ggsave(
  filename = output_file_nmds,
  plot = nmds_plot,
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)












###Adding alpha and beta diversity plots together into one

library(ggplot2)
library(cowplot)

# Generate Plot A: Alpha Diversity Plot
plot_A <- ggplot(virus_data_with_alpha, aes(x = sample.site, y = alpha_diversity)) +
  geom_boxplot(aes(fill = sample.site)) +
  labs(
    title = "A: Alpha Diversity (Shannon Index)",
    x = "Sample Site",
    y = "Shannon Index"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Generate Plot B: PCoA Plot for Bray-Curtis Dissimilarity
plot_B <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type)) +
  geom_point(size = 7, alpha = 1.0) +
  labs(
    title = "B: PCoA of Viral Communities",
    x = "PCoA1",
    y = "PCoA2"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Generate Plot C: PCoA with Custom Colors and Shapes
plot_C <- ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 6, alpha = 0.9) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "C: PCoA with Sample Types and Sites",
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Combine the plots into a single figure
combined_plot <- plot_grid(
  plot_A, plot_B, plot_C, 
  labels = c("A", "B", "C"), 
  ncol = 1, 
  rel_heights = c(1, 1, 1)  # Adjust heights if needed
)



# Define the output file path
output_file <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/combined_alpha_beta_diversity_plots.png"

# Save the combined plot
ggsave(output_file, plot = combined_plot, dpi = 300, width = 10, height = 15, units = "in")
















####Beta PCoA adding more dots, need more Gb of data and run this on HPC



library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

numeric_cols <- sapply(wide_data, is.numeric)
count_matrix <- as.matrix(wide_data[, numeric_cols])



# Load required libraries
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# Pivot data to wide format for Bray-Curtis calculation
wide_data <- virus_data %>%
  pivot_wider(names_from = family, values_from = counts, values_fill = 0)

# Extract numeric columns for Bray-Curtis calculation
numeric_cols <- sapply(wide_data, is.numeric)
count_matrix <- as.matrix(wide_data[, numeric_cols])

# Add a small pseudocount to avoid zero issues
count_matrix <- count_matrix + 1e-6

# Check and handle negative values
if (any(count_matrix < 0)) {
  count_matrix[count_matrix < 0] <- 0
  warning("Negative values in count matrix were set to zero.")
}

# Filter out low-abundance features to reduce size
threshold <- 10  # Adjust this threshold as needed
filtered_matrix <- count_matrix[, colSums(count_matrix) > threshold]

# Compute Bray-Curtis dissimilarity
bray_curtis_matrix <- vegdist(filtered_matrix, method = "bray")

# Perform PCoA
pcoa_result <- cmdscale(bray_curtis_matrix, eig = TRUE, k = 2)

# Combine PCoA results with metadata for plotting
pcoa_coordinates <- as.data.frame(pcoa_result$points)
colnames(pcoa_coordinates) <- c("PCoA1", "PCoA2")
pcoa_coordinates <- cbind(pcoa_coordinates, wide_data[, c("sample.type", "sample.site")])

# Plot with ellipses
ggplot(pcoa_coordinates, aes(x = PCoA1, y = PCoA2, color = sample.type, shape = sample.site)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = sample.type), geom = "polygon", alpha = 0.2, color = NA) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "brown", "cyan")) +
  labs(
    title = "PCoA of Viral Communities (Bray-Curtis Dissimilarity)",
    x = "PCoA1",
    y = "PCoA2",
    color = "Sample Type",
    shape = "Sample Site",
    fill = "Sample Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )










##Sankey chart showing sample type and virus counts with taxonomy


library(dplyr)
library(networkD3)



# Limit to top 23 virus families based on total counts
virus_data_sankey <- virus_data %>%
  group_by(sample.type, family) %>%
  summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(counts)) %>%
  slice_head(n = 25)  # Top 23 families

# Filter to keep only counts > 0
virus_zero <- virus_data_sankey %>%
  filter(counts > 0)

# Apply log transformation to counts
virus_zero$log_counts <- log(virus_zero$counts)

# Create nodes (unique values from sample.type and family)
nodes <- data.frame(name = unique(unlist(virus_zero[, c("sample.type", "family")])))
nodes$id <- 0:(nrow(nodes) - 1)

# Assign node group based on type (sample.type or family)
nodes$group <- ifelse(nodes$name %in% unique(virus_zero$sample.type), "sample_type", "family")


# Define colors for sample types
sample_type_colors <- c(
  "oyster" = "#FFB3BA",  # Pastel pink
  "mussel" = "#FFDFBA",  # Pastel orange
  "sd seawater" = "#FFFFBA",  # Pastel yellow
  "bay seawater" = "#BAFFD1",  # Soft light green
  "dolphin feces" = "#BAFFC9",  # Pastel green
  "sealion feces" = "#BAE1FF",  # Pastel blue
  "sealion serum" = "#D1BAFF",  # Pastel purple
  "dolphin serum" = "#FFBAFF"   # Pastel magenta
)


# Define colors for virus families
family_colors <- c(
  "unclassified" = "#D3D3D3",  # Light gray for "Unknown"
  "Baculoviridae" = "#8B0000",  # Dark red
  "Nudiviridae" = "#FF6347",  # Tomato red
  "Poxviridae" = "#32CD32",  # Lime green
  "Alloherpesviridae" = "#FFD700",  # Gold
  "Steitzviridae" = "#20B2AA",  # Light sea green
  "Polydnaviriformidae" = "#9370DB",  # Medium purple
  "Kyanoviridae" = "#FF4500",  # Orange red
  "Autographiviridae" = "#DAA520",  # Goldenrod
  "Orthoherpesviridae" = "#C71585"  # Medium violet red
)


# Filter to keep only counts > 0
virus_zero <- virus_data_sankey %>%
  filter(counts > 0)

# Apply log transformation to counts
virus_zero$log_counts <- log(virus_zero$counts)

# Create nodes (unique values from sample.type and family)
nodes <- data.frame(name = unique(unlist(virus_zero[, c("sample.type", "family")])))
nodes$id <- 0:(nrow(nodes) - 1)

# Assign node group based on type (sample.type or family)
nodes$group <- ifelse(nodes$name %in% names(sample_type_colors), "sample_type", "family")

# Assign node colors based on group (sample_type or family)
nodes$color <- sapply(nodes$name, function(x) {
  if (x %in% names(sample_type_colors)) {
    return(sample_type_colors[x])  # Use the sample type color
  } else if (x %in% names(family_colors)) {
    return(family_colors[x])  # Use the family color
  } else {
    return("#CCCCCC")  # Default to gray for unmatched nodes
  }
})

# Debug: Check node color assignments
print("Node color assignments:")
print(nodes)

# Create links from virus_zero (filtered data with counts > 0)
links <- virus_zero
links$source <- match(links$sample.type, nodes$name) - 1
links$target_family <- match(links$family, nodes$name) - 1

# Assign link colors based on sample.type
links$color <- sample_type_colors[links$sample.type]

# Combine links for Sankey (flow between sample.type -> family) using log-transformed counts
sankey_links <- data.frame(source = links$source, 
                           target = links$target_family, 
                           value = links$log_counts, 
                           color = links$color)

# Sankey diagram with custom color assignments for nodes and links
sankey <- sankeyNetwork(
  Links = sankey_links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "Log(Counts)",  # Update units to reflect log transformation
  fontSize = 20,
  nodeWidth = 30,
  LinkGroup = "color",   # Link colors based on 'color' column
  NodeGroup = "color",   # Node colors based on 'color' column
  colourScale = 'd3.scaleOrdinal().domain(["oyster", "mussel", "seawater", "dolphin feces", "sealion feces", "sealion serum", "dolphin serum"]).range(["#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", "#D1BAFF", "#FFBAFF"])'
)

# Plot the Sankey diagram
sankey



# Save the Sankey diagram as an HTML file
library(htmlwidgets)

output_html <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/sankey_diagram.html"
saveWidget(sankey, file = output_html, selfcontained = TRUE)


# Install webshot2 if not already installed
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2")
}

# Use webshot2 to save the Sankey diagram as a PNG
library(webshot2)

output_png <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/results/sankey_diagram.png"
webshot2::webshot(output_html, file = output_png, vwidth = 1200, vheight = 800, zoom = 2)









####What are the top vertebrate viruses from the different sample.types (All the taxonomy) 
##Creating a table for the paper with abundance hits
# Load necessary library
library(dplyr)

# Assuming your data is in a data frame called 'virus_data'

# Step 1: Filter for host == vertebrate
vertebrate_viruses <- virus_data %>%
  filter(host == "vertebrate")

# Step 2: Group by sample.type, family, genus, and species, then summarize counts
top_viruses <- vertebrate_viruses %>%
  group_by(sample.type, family, genus, species) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# Step 3: Identify the top viral hits for each sample.type
top_viruses_table <- top_viruses %>%
  group_by(sample.type) %>%
  slice_max(order_by = total_counts, n = 5) %>%
  ungroup()

# View the result
print(top_viruses_table)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/top_vertebrate_viruses.csv"

# Save the table as a CSV file
write.csv(top_viruses_table, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)





####What are the top vertebrate viruses from the different sample.types?, vertebrate family only
library(dplyr)

# Assuming your data is in a data frame called 'virus_data'

# Step 1: Filter for host == vertebrate
vertebrate_viruses <- virus_data %>%
  filter(host == "vertebrate")

# Step 2: Group by sample.type and family, then summarize total counts
vertebrate_family_counts <- vertebrate_viruses %>%
  group_by(sample.type, family) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# View the result
print(vertebrate_family_counts)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_family_counts.csv"

# Save the table as a CSV file
write.csv(vertebrate_family_counts, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)


##What are the top vertebrate viral read counts by date?

library(dplyr)

# Step 1: Filter for vertebrate viruses
vertebrate_viruses <- virus_data %>%
  filter(host == "vertebrate")

# Step 2: Group by collection.date, sample.type, and family, then summarize total counts
vertebrate_family_counts_by_date <- vertebrate_viruses %>%
  group_by(collection.date, sample.type, family) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# View the result
print(vertebrate_family_counts_by_date)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_family_counts_by_date.csv"

# Save the table as a CSV file
write.csv(vertebrate_family_counts_by_date, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)


##vertebrate viruses top familes by sample.type

# Step 1: Filter for vertebrate viruses
vertebrate_viruses <- virus_data %>%
  filter(host == "vertebrate")

# Step 2: Group by collection.date, sample.type, and family, then summarize total counts
vertebrate_family_genus_species <- vertebrate_viruses %>%
  group_by(sample.type,family, genus, species) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")


# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/vertebrate_family_genus_species.csv"

# Save the table as a CSV file
write.csv(vertebrate_family_genus_species, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)






## What are the top virus hits for all the virus hosts?
library(dplyr)
library(stringr)

## What are the top virus hits for all the viruses, family, genus and specie?
# Assuming your data is in a data frame called 'virus_data'

# Step 1: Filter out rows where family, genus, or species are "unknown" or "unclassified"
virus_data_filtered <- virus_data %>%
  filter(
    tolower(str_trim(family)) != "unknown" & tolower(str_trim(family)) != "unclassified",
    tolower(str_trim(genus)) != "unknown" & tolower(str_trim(genus)) != "unclassified",
    tolower(str_trim(species)) != "unknown" & tolower(str_trim(species)) != "unclassified"
  )

# Step 2: Group by sample.type, family, genus, and species, then summarize counts
top_viruses <- virus_data_filtered %>%
  group_by(sample.type, family, genus, species) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# Step 3: Identify the top viral hits for each sample.type
top_viruses_table <- top_viruses %>%
  group_by(sample.type) %>%
  slice_max(order_by = total_counts, n = 1) %>%
  ungroup()

# View the result
print(top_viruses_table)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/top_all_hosts_viruses.csv"

# Save the table as a CSV file
write.csv(top_viruses_table, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)







###Virus sample type by virus type counts
# Step 1: Group by sample.type and host, then count total occurrences
virus_host_counts <- virus_data %>%
  group_by(sample.type, type) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# View the result
print(virus_host_counts)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_type_counts_by_sample_type.csv"

# Save the table as a CSV file
write.csv(virus_host_counts, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)



###Virus sample type by host counts
library(dplyr)

# Step 1: Group by sample.type and host, then count total occurrences
virus_host_counts <- virus_data %>%
  group_by(sample.type, host) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# View the result
print(virus_host_counts)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_host_counts_by_sample_type.csv"

# Save the table as a CSV file
write.csv(virus_host_counts, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)
# View the result
print(virus_host_counts)

# Define the output path
output_path <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_host_counts_by_sample_type.csv"

# Save the table as a CSV file
write.csv(virus_host_counts, output_path, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path)


##Table for virus family and genus counts across sample types
# Step 1: Group by sample.type and host, then count total occurrences
virus_genus_family_counts <- virus_data %>%
  group_by(sample.type, family, genus) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")




library(dplyr)

library(dplyr)

# Step 1: Sum counts for each sample.type and virus type
virus_host_counts <- virus_data %>%
  group_by(sample.type, host) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop")

# Step 2: Get total counts for each sample.type
sample_totals <- virus_host_counts %>%
  group_by(sample.type) %>%
  summarize(sample_total = sum(total_counts), .groups = "drop")

# Step 3: Calculate percentages
virus_host_percentage <- virus_host_counts %>%
  left_join(sample_totals, by = "sample.type") %>%
  mutate(percentage = (total_counts / sample_total) * 100)

# Step 4: Select the top 3 virus types with the highest percentages per sample.type
top_virus_percentages <- virus_host_percentage %>%
  group_by(sample.type) %>%
  slice_max(order_by = percentage, n = 3, with_ties = FALSE)  # Top 3 per sample.type

# Print the result
print(top_virus_percentages, n=100)





library(dplyr)

# Virus sample type by virus type counts with percentage calculation for each sample.type
virus_host_counts_type <- virus_data %>%
  group_by(sample.type, type) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  group_by(sample.type) %>%
  mutate(percentage = (total_counts / sum(total_counts) * 100)) %>%
  ungroup()

# View the result for virus type counts with percentages
print(virus_host_counts_type)

# Define the output path for virus type counts
output_path_type <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_type_counts_by_sample_type_with_percentage.csv"

# Save the table as a CSV file
write.csv(virus_host_counts_type, output_path_type, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path_type)





# Virus sample type by host counts with percentage calculation for each sample.type
virus_host_counts_host <- virus_data %>%
  group_by(sample.type, host) %>%
  summarize(total_counts = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  group_by(sample.type) %>%
  mutate(percentage = (total_counts / sum(total_counts) * 100)) %>%
  ungroup()

# View the result for virus host counts with percentages
print(virus_host_counts_host)

# Define the output path for virus host counts
output_path_host <- "C:/Users/kvigil/OneDrive - Oxford Nanopore Technologies/results/katie/epi2me_out/R/data/virus_host_counts_by_sample_type_with_percentage.csv"

# Save the table as a CSV file
write.csv(virus_host_counts_host, output_path_host, row.names = FALSE)

# Confirm the file was saved
cat("Table successfully exported to:", output_path_host)







