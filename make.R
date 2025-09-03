# Script to reproduce all analyses and produce results and figures

# Install dependencies
renv::restore()

# Load functions
devtools::load_all()

# Run scripts

# make map of study area
source("analysis/make_map.R")

# calculate distance from glacier front
source("analysis/calculate_glacier_distance.R")

# prepare foraminiferal community data
source("analysis/prepare_foram_data.R")

# check for size-related biases in taxonomic identification
source("analysis/test_size_identification_bias.R")

# foraminiferal abundance and size structure
source("analysis/abundance_and_size_structure.R")

# foraminiferal composition
source("analysis/community_composition.R")

# heatmaps of foraminiferal relative abundance
source("analysis/heatmaps_species_rel_abund.R")

# plot traits distribution across stations
source("analysis/traits_distribution.R")

# compute foraminiferal diversity and analyse correlations
source("analysis/diversity_computation_and_correlations.R")

# foraminiferal diversity models
source("analysis/model_diversity.R")
