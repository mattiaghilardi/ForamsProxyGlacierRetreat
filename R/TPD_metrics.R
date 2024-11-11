#' Compute functional diversity metrics with TPD when using different types of traits 
#' 
#' Compute all functional diversity metrics using trait probability density (TPD) 
#' using different type of traits (i.e. quantitative, categorical, ordinal...).
#' The function computes metrics at the community level (richness, evenness, divergence, 
#' Rao, redundancy, dissimilarity) and at the species level (uniqueness), based on 
#' the species position along the axis of a principal coordinate analysis of a
#' Gower distance matrix, computed either with \code{\link[FD:gowdis]{FD::gowdis()}} 
#' or \code{\link[gawdis:gawdis]{gawdis::gawdis()}}.
#'  
#' @param comm Community data, a site X species matrix or data frame
#' @param trait Trait data, a species x trait matrix or data frame
#' @param dist The dissimilarity matrix to be used. Possible alternatives are:
#'            "gowdis", the Gower dissimilarity computed using \code{FD::gowdis()};
#'            "gawdis", is similar to "gowdis" but the same weight is given 
#'            to each trait (see De Bello et al. 2020)
#' @param ... Additional parameters passed to \code{FD::gowdis()} or \code{gawdis::gawdis()}
#' @param k The number of PCOA eigenvalues to retain (maximum allowed to compute TPD is four)
#' @param REND Logical, whether to compute functional richness, evenness, and divergence, defaults to TRUE
#' @param rao Logical, whether to compute Rao's quadratic entropy and its partition, defaults to TRUE
#' @param redun Logical, whether to compute functional redundancy, defaults to TRUE
#' @param dissim Logical, whether to compute functional dissimilarity, defaults to TRUE
#' @param uniq Logical, whether to compute functional uniqueness, defaults to TRUE
#' @param regional Logical, only if 'rao=TRUE', whether the correction by Villeger and Mouillot (2008) 
#'                 is applied to compute Rao or not, defaults to TRUE
#' 
#' @details Species names must be the same in 'comm' and 'trait'.
#' 
#' @return A list containing the following elements:
#'         \code{PCOA}: the PCOA object
#'         \code{TPDsp}: the TPDs of each species
#'         \code{TPDcomm}: the TPDc of each community
#'         \code{diversity}: a tibble with several functional metrics computed for each community 
#'                           (FRic, FEve, FDiv, Redundancy, Redundancy relative)
#'         \code{rao}: Rao's Quadratic Entropy and its Partition
#'                     (see \code{\link[TPD:rao]{TPD::rao()}})
#'         \code{dissimilarity}: the functional dissimilarity between pairs of communities 
#'                               (see \code{\link[TPD:dissim]{TPD::dissim()}})
#'         \code{uniqueness}: the functional uniqueness of each species in each community
#'                            (see \code{\link[TPD:uniqueness]{TPD::uniqueness()}})
#' 
#' @references Villeger and Mouillot (2008) Additive partitioning of diversity 
#'             including species differences: a comment on Hardy & Senterre (2007).
#'             J. Ecol., 96, 845–848. https://doi.org/10.1111/j.1365-2745.2007.01351.x
#'             
#'             De Bello F. et al. (2020) Towards a more balanced combination of 
#'             multiple traits when computing functional differences between species.
#'             Methods Ecol. Evo., 12(3), 443-448. https://doi.org/10.1111/2041-210X.13537
#' 
func_div_TPD <- function(comm, trait, dist = c("gowdis", "gawdis"), ..., k, 
                         REND = TRUE, rao = TRUE, redun = TRUE,
                         dissim = TRUE, uniq = TRUE, regional = TRUE) {
  
  dist <- match.arg(dist)
  
  # Dissimilarity matrix
  if (dist == "gowdis") {
    # Gower with package FD
    dist <- FD::gowdis(trait, ...)
  } else {
    # Gawdis with the respective package
    dist <- gawdis::gawdis(trait, ...)
  }
  
  # PCOA (retain k axes)
  pcoa <- labdsv::pco(dist, k = k)
  
  # Replace the original trait matrix with the position of the species on the principal coordinates
  traits2 <- pcoa$points
  row.names(traits2) <- row.names(trait)
  
  # Use TPDsMean which requires the mean and sd of the traits
  # Use package ks to get bandwidth for the k traits (i.e. principal coordinates)
  bw <- diag(sqrt(ks::Hpi.diag(traits2)))
  
  # Matrix to be used as sds
  bwM <- matrix(rep(bw, each = nrow(traits2)), ncol = ncol(traits2))
  
  # TPD for each species
  TPDsp <- TPD::TPDsMean(species = rownames(traits2),
                         means = traits2,
                         sds = bwM)
  
  # TPD for each community
  TPDcomm <- TPD::TPDc(TPDs = TPDsp, sampUnit = comm)
  
  # Functional richness, evenness and divergence
  if (REND) {
    rend <- TPD::REND(TPDc = TPDcomm)
  }
  
  # Rao
  if (rao) {
    dissim_sp <- TPD::dissim(TPDsp)
    Rao <- TPD::Rao(diss = dissim_sp, TPDc = TPDcomm, regional = regional)
  }
  
  # Functional dissimilarity
  if (dissim) {
    dissim <- TPD::dissim(TPDcomm)
  }
  
  # Functional redundancy
  if (redun) {
    redundancy <- TPD::redundancy(TPDcomm)
  }
  
  # Functional uniqueness
  if (uniq) {
    uniqueness <- TPD::uniqueness(TPDs = TPDsp, TPDc = TPDcomm)
  }
  
  # Tibble with combined results 
  if (REND | redun) {
    results <- dplyr::tibble(Comm = rownames(comm))
    if (REND) {
      results$Fun_richness <- rend$communities$FRichness
      results$Fun_evenness <- rend$communities$FEvenness
      results$Fun_divergence <- rend$communities$FDivergence
    }
    if (redun) {
      results$Fun_redundancy <- redundancy$redundancy
      results$Fun_redundancy_rel <- redundancy$redundancyRelative
    }
  }
  
  # Output list
  out <- list(PCOA = pcoa,
              TPDsp = TPDsp,
              TPDcomm = TPDcomm)
  if (REND | redun) {
    out[[length(out)+1]] <- results
    names(out)[length(out)] <- "diversity"
  }
  if (rao) {
    out[[length(out)+1]] <- Rao
    names(out)[length(out)] <- "rao"
  }
  if (dissim) {
    out[[length(out)+1]] <- dissim
    names(out)[length(out)] <- "dissimilarity"
  }
  if (uniq) {
    out[[length(out)+1]] <- uniqueness
    names(out)[length(out)] <- "uniqueness"
  }
  
  out
}
