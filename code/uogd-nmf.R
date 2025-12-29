library(NMF)
library(tidyverse)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
library(patchwork)

# read data from GitHub repo

url <- "https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/main/data/merged/merged_for_nmf.csv"
hourly_data <- read_csv(url)

# some pre-processing (create total radioactivity, and remove 1-3 butadiene outlier)
hourly_data$total_radioactivity<-hourly_data$radon_B+hourly_data$rd_particle_B
hourly_data<-hourly_data[hourly_data$`1_3-butadiene`<=0.1,]

# list vocs and non-vocs
vocs <- c("ethane", "ethene", "propane", "propene",
          "1_3-butadiene", "i-butane", "n-butane",
          "acetylene", "cyclopentane", "i-pentane",
          "n-pentane", "n-hexane", "isoprene", "n-heptane",
          "benzene", "n-octane", "toluene", "ethyl-benzene", 
          "m&p-xylene", "o-xylene")

non_vocs <- c('ch4', 'co2', 'co', 'h2s', 'so2', 
              'nox', 'o3','total_radioactivity')


# remove rows with missing obs for any chemical this has all necessary data for NMF and plots
hourly_full_nona <- hourly_data %>% 
  dplyr::select(any_of(c('day', 'datetime_mountain', vocs, non_vocs, 'wdr_deg', 'wsp_ms'))) %>%
  na.omit()

# separate data with only the vocs, removing everything else except the vocs
hourly_vocs <- hourly_nona %>% dplyr::select(any_of(vocs))

#  separate data with only the non-vocs: co2_ppm, nox, ch4, h2s, so2, o3
hourly_non_vocs <- hourly_nona %>% dplyr::select(any_of(non_vocs)) 

# combine voc and non-voc datasets
hourly_nona <- cbind(hourly_non_vocs, hourly_vocs)


# Define LOD for each chemical
LOD_non_voc <- c('ch4' = 0.9, 
                 'co2' = 0.0433, 
                 'co' = 20,
                 'h2s' = 0.4, 
                 'so2' = 0.4, 
                 'nox' = 0.05, 
                 'o3' = 1,
                 'total_radioactivity'=4) 

#LOD_voc_avg <- read_xlsx('https://raw.githubusercontent.com/meredithfranklin/UOGD-source-contributions/main/data/VOC-sampling-window/LNM_VOC_Uncertainties.xlsx', skip = 1)

LOD_voc_avg <- LOD_voc_avg %>%
  dplyr::select(1, 4) %>%
  rename('LOD' = 2, 'chemical' = 1) %>%
  head(20)

# Background concentration correction
# take the minimum concentration for each compound as the background value
# Adjustment from Schade & Roest 2018 section 2.2 and Guha et al 2015 section 3.3

# find the min for background-levels
background_levels <- sapply(hourly_nona, min)

adjusting_neg_bg_from_lod <- function(chemical, LOD, background, hourly_data){ 
  # get min and max
  min_value <- min(hourly_data[chemical], na.rm = TRUE)
  max_value <- max(hourly_data[chemical], na.rm = TRUE)
  # if min less than double LOD or max > 100 times LOD
  if (min_value < 2 * LOD & max_value > 100 * LOD ){
    return (0)
  }
  return (background)
}

# adjust background for non-vocs
background_lod_non_voc <- tibble(chemical = non_vocs,
                                 LOD = LOD_non_voc,
                                 background = unname(background_levels[non_vocs]))

adjusted_background_non_voc <- background_lod_non_voc %>%
  rowwise() %>%
  mutate(min = min(hourly_nona[chemical], na.rm = TRUE),
         LODx2 = 2 * LOD,
         criterion1 = min(hourly_nona[chemical], na.rm = TRUE) < 2 * LOD,
         max = max(hourly_nona[chemical], na.rm = TRUE),
         LODx100 = 100 * LOD,
         criterion2 = max(hourly_nona[chemical], na.rm = TRUE) > 100 * LOD,
         adjusted_background = adjusting_neg_bg_from_lod(chemical, LOD, background, 
                                                         hourly_nona))

# adjust background for vocs
background_lod_voc <- LOD_voc_avg %>%
  left_join(tibble(chemical = setdiff(names(background_levels), non_vocs),
                   background = background_levels[setdiff(names(background_levels), 
                                                          non_vocs)]))
adjusted_background_voc <- background_lod_voc %>%
  rowwise() %>%
  mutate(min = min(hourly_nona[chemical], na.rm = TRUE),
         LODx2 = 2 * LOD,
         criterion1 = min(hourly_nona[chemical], na.rm = TRUE) < 2 * LOD,
         max = max(hourly_nona[chemical], na.rm = TRUE),
         LODx100 = 100 * LOD,
         criterion2 = max(hourly_nona[chemical], na.rm = TRUE) > 100 * LOD,
         adjusted_background = adjusting_neg_bg_from_lod(chemical, LOD, background, 
                                                         hourly_nona))
# now we have the adjusted background concentrations
hourly_nona_bgrm <- hourly_nona %>%
  mutate(across(adjusted_background_non_voc$chemical, 
                ~  .x - adjusted_background_non_voc$adjusted_background[
                  adjusted_background_non_voc$chemical == cur_column()]))
hourly_nona_bgrm <- hourly_nona_bgrm %>%
  mutate(across(adjusted_background_voc$chemical, 
                ~  .x - adjusted_background_voc$adjusted_background[
                  adjusted_background_voc$chemical == cur_column()]))

# replace zeros with a random number between 0 and 1/2 the LOD
set.seed(123)
replace_zero_with_random <- function(column, name, LOD_df){
  LOD <- LOD_df$LOD[LOD_df$chemical == name]
  column <- if_else(column == 0, round(runif(length(column), 0, 0.5 * LOD), 3), column)
  return (column)
}

hourly_nona_bgrm_zerorepl <- hourly_nona_bgrm %>%
  mutate(across(adjusted_background_non_voc$chemical,
                ~ replace_zero_with_random(.x, cur_column(), adjusted_background_non_voc)))

hourly_nona_bgrm_zerorepl <- hourly_nona_bgrm_zerorepl %>%
  mutate(across(adjusted_background_voc$chemical,
                ~ replace_zero_with_random(.x, cur_column(), adjusted_background_voc)))

# normalizing function to normalize the data
normalize_column <- function(column){
  background <- quantile(column, 0)
  max <- quantile(column, 1) 
  return ((column - background)/(max - background))
}

# apply normalizing function
hourly_nona_bgrm_zerorepl_norm <- as_tibble(sapply(as.list(hourly_nona_bgrm_zerorepl),
                                                   normalize_column))

normalized_matrix <- as.matrix(hourly_nona_bgrm_zerorepl_norm)

# remove ozone for NMF
normalized_matrix_less_o3 <- normalized_matrix[ ,setdiff(colnames(normalized_matrix), c("o3"))]

uncertainty_matrix <- matrix(0, nrow = nrow(normalized_matrix_less_o3), 
                             ncol = ncol(normalized_matrix_less_o3))
LOD_merged <- tibble(chemical = c(adjusted_background_non_voc$chemical, 
                                  adjusted_background_voc$chemical),
                     LOD = c(adjusted_background_non_voc$LOD, 
                             adjusted_background_voc$LOD))

LOD_merged <- tibble(chemical = names(hourly_nona_bgrm_zerorepl_norm)) %>%
  left_join(LOD_merged) %>%
  filter(chemical %in% colnames(normalized_matrix_less_o3))

# creating uncertainty Matrix
for (i in 1:dim(uncertainty_matrix)[1]) { 
  for (j in 1:dim(uncertainty_matrix)[2]) {
    chemical <- colnames(normalized_matrix_less_o3)[j]
    xij <- normalized_matrix_less_o3[i, j]
    LOD <- LOD_merged$LOD[LOD_merged$chemical == chemical]
    # Based on Guha Eq5a, EQ5c
    if (xij <= LOD) {
      uncertainty_matrix[i, j] <- 2 * LOD # equation 5a) in reference paper
    } else {
      uncertainty_matrix[i, j] <- sqrt(((0.1 * xij)**2 + LOD**2))  #equation 5c) in reference paper
    }
  }
}


# Convert zero uncertainties to the next smallest uncertainty of the corresponding compound
uncertainty_matrix[uncertainty_matrix==0]<-apply(uncertainty_matrix, 2, function(x) sort(x)[2])
# take inverse
weight_matrix <- 1/uncertainty_matrix

# LS-NMF + nndsvd seed
components <- 4:5
lsnmf_nndsvd_less_o3 <- nmf(
  normalized_matrix_less_o3,
  rank = components,
  nrun = 1, # since using nndsvd
  method = "ls-nmf",
  weight = weight_matrix,
  seed = 'nndsvd'
)

# Extract matrices
# Extract W (basis (nxk)) and H (coefs (kxm)) matrices 
# NMF factorizes V = WH
# dimensions: n observations, m chemical components, k rank of nmf
# extract 5-factors

nmf_result_5c_less_o3 <- lsnmf_nndsvd_less_o3$fit$`5`

basis_matrix_5c_less_o3 <- basis(nmf_result_5c_less_o3) 
coef_matrix_5c_less_o3 <- coef(nmf_result_5c_less_o3)

# Calculate leave-one-out variance explained for each factor (1:5)
# Weighted total sums of squares (wtss), weighted residual sums of squares (wrss)
wtss <- sum(0.5*(weight_matrix * (normalized_matrix_less_o3 - mean(normalized_matrix_less_o3)))^2)
wrss <- sum(0.5*(weight_matrix * (normalized_matrix_less_o3 - reconstruct))^2)
variance_explained<- 1 - (wrss / wtss) #total variance explained by all factors

# extract leave one out variance explained for each of the factors
variance_explained_factors_loo <- numeric(5)
for (i in 1:5) {
  # Compute reconstruction from the 5 factors (without the i-th)
  reconstruction_loo <- (basis_matrix_5c_less_o3[, -i, drop=FALSE] %*% coef_matrix_5c_less_o3[-i, , drop=FALSE])
  # Compute Residual Sum of Squares (RSS)
  wrss_temp <- sum(0.5*(weight_matrix * (normalized_matrix_less_o3 - reconstruction_loo))^2)
  # Compute Variance Explained without this factor
  variance_explained_loo <- 1 - (wrss_temp / wtss)
  # Compute Variance Explained gained from adding this factor
  variance_explained_factors_loo[i] <- variance_explained - variance_explained_loo
}

# order them by variance explained and print
order <- sapply(1:5, function(i) which(variance_explained_factors_loo==sort(variance_explained_factors_loo, decreasing = T)[i]))
paste('Variance explained from each factor (LOO): ',
      paste0(order, ' (',round(variance_explained_factors_loo[order_trad], 3),')', collapse = ', '))


### Plotting ###

# Contributions to each component
# Capitalized labels for nice plotting
chemical_labels <- c(
  "ethane" = "Ethane", "propane" = "Propane", "i-butane" = "i-Butane", "n-butane" = "n-Butane", 
  "i-pentane" = "i-Pentane", "n-pentane" = "n-Pentane", "n-hexane" = "n-Hexane", 
  "cyclopentane" = "Cyclopentane", "n-heptane" = "n-Heptane", "n-octane" = "n-Octane",
  "ethene" = "Ethene", "propene" = "Propene", "1_3-butadiene" = "1,3-Butadiene", "isoprene" = "Isoprene", 
  "acetylene" = "Acetylene",
  "benzene" = "Benzene", "toluene" = "Toluene", "ethyl-benzene" = "Ethyl-Benzene", 
  "o-xylene" = "o-Xylene", "m&p-xylene" = "m&p-Xylene",
  "co" = "CO", "co2" = "CO2", "nox"="NOx",
  "h2s" = "H2S",
  "so2" = "SO2",
  "o3"  = "O3",
  "ch4" = "CH4", "total_radioactivity"="Radioactivity"
)  

# Define the desired order of chemicals
desired_order <- c(
  # NMHCs - Alkanes
  "ethane", "propane", "i-butane", "n-butane", "i-pentane", "n-pentane", 
  "n-hexane", "cyclopentane", "n-heptane", "n-octane",
  
  # NMHCs - Alkenes
  "ethene", "propene", "1_3-butadiene", 'isoprene',
  
  # NMHCs - Alkynes
  "acetylene",
  
  # NMHCs - Aromatics
  "benzene", "toluene", "ethyl-benzene", "o-xylene", "m&p-xylene",
  
  "co", "co2",
  
  "nox",
  
  "h2s", "so2",
  
  "ch4",
  
  "total_radioactivity"
)

color_pal<-c("#00AFBB", "#E7B800", "#FC4E07","#0072B2","#8B4513")

get_component_plot <- function(data, component, title) {
  col <- color_pal[as.numeric(component)]
  component_data <- subset(data, Component == component) %>%
    mutate(Chemical = factor(Chemical, levels = desired_order),
           ChemicalLabel = dplyr::recode(Chemical, !!!chemical_labels)) 
  
  plot <- ggplot(component_data, aes(x = ChemicalLabel, y = Contribution)) +
    geom_bar(stat = "identity", position = "dodge", fill = col) +
    geom_text(aes(label = sprintf("%.2f", round(Contribution, 2))), 
              color = "blue", size = 6, vjust = -0.5) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      plot.title = element_text(size = 18),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    ) +
    labs(x = "", y = "Contribution", title = title)
  return(plot)
}

# prep NMF output for plotting
H_df_5c_less_o3 <- as.data.frame(coef_matrix_5c_less_o3)
H_df_5c_less_o3$Component <- rownames(H_df_5c_less_o3)
H_long_5c_less_o3 <- pivot_longer(H_df_5c_less_o3, cols = -Component, 
                                  names_to = "Chemical", values_to = "Contribution")

# plot with labels 
nmfplt_1_svd_5c_less_o3 <- get_component_plot(H_long_5c_less_o3, 
                                              '1', '1) Fugitive and Venting Emssions')
nmfplt_2_svd_5c_less_o3 <- get_component_plot(H_long_5c_less_o3, 
                                              '2', '5) Other Area Sources')
nmfplt_3_svd_5c_less_o3 <- get_component_plot(H_long_5c_less_o3, 
                                              '3', '4) Flaring')
nmfplt_4_svd_5c_less_o3 <- get_component_plot(H_long_5c_less_o3, 
                                              '4', '3) Traffic')
nmfplt_5_svd_5c_less_o3 <- get_component_plot(H_long_5c_less_o3, 
                                              '5', '2) Produced Water')

# put all plots together in one
plots <- list(
  nmfplt_1_svd_5c_less_o3 + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  nmfplt_5_svd_5c_less_o3 + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  nmfplt_4_svd_5c_less_o3 + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  nmfplt_3_svd_5c_less_o3 + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                  axis.text.x = element_blank(), axis.ticks.x = element_blank()),
  nmfplt_2_svd_5c_less_o3 + theme(axis.title.y = element_blank(), axis.title.x = element_blank())  # keep x-axis here
)

y_axis_label <- wrap_elements(
  full = textGrob("Contribution", rot = 90, gp = gpar(fontsize = 22))
)

stacked_plots <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]]

final_plot <- y_axis_label | stacked_plots

final_plot <- final_plot +
  plot_layout(widths = c(0.05, 1), guides = "collect") &
  theme(axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 22))
ggsave("factors_patchwork_5factor.png", final_plot, width = 16, height = 20)

### Fingerprint (proportions) plot ###

get_fingerprint_plot <- function(H, 
                                 factor_names = c('Factor 1', 'Factor 2',
                                                  'Factor 3', 'Factor 4', 'Factor 5'),
                                 factor_order = c(1, 2, 3, 4, 5)) {

# Reorder rows if necessary

H <- H %>% dplyr::slice(factor_order)
  factor_names <- factor_names[factor_order]
  color_pal <- color_pal[factor_order]
  
# Create color palette
  custom_colors <- setNames(color_pal, factor_names)

# Convert to proportions
contrib_prop <- apply(H[,1:(length(H)-1)], MARGIN = 2, FUN = function(x) {x / sum(x)})
  
contrib_prop <- contrib_prop %>%
    as_tibble() %>%
    dplyr::mutate(Component = factor_names) %>%
    dplyr::mutate(Component = factor(Component, levels = factor_names)) %>%
    pivot_longer(cols = -Component, names_to = "Chemical", values_to = "Contribution_prop") %>%
    dplyr::mutate(Chemical = factor(Chemical, levels = desired_order),
                  ChemicalLabel = dplyr::recode(Chemical, !!!chemical_labels))
  
factor_labels <- c(
    "Factor 1" = "Fugitive/Venting",
    "Factor 2" = "Other Area Sources",
    "Factor 3" = "Flaring",
    "Factor 4" = "Traffic",
    "Factor 5" = "Produced Water"
  )
  
return(contrib_prop %>%
           ggplot(aes(fill = Component, y = Contribution_prop, x = ChemicalLabel)) +
           geom_bar(position = "fill", stat = "identity") +
           scale_fill_manual(values = custom_colors) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           labs(x = "Chemical", y = "Contribution Proportion") +
           theme(
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank()
           ))
}


fingerprint <- get_fingerprint_plot(
  H_df_5c_less_o3,
  c('Fugitive/Venting',
    'Other Area Sources',
    'Flaring',
    'Traffic',
    'Produced Water'
  ),
  c(1, 5, 4, 3,2)
)
fingerprint

ggsave("fingerprint_5factor.png", fingerprint)