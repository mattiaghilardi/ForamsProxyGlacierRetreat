#' Function to compute species richness, Shannon index, and Pielou's evenness of communities
#' 
#' @param  comm Community data, a site X species matrix or data frame
#' 
#' @return A tibble
#' 
#' @examples 
#' \dontrun{
#' # A matrix of abundances of ten species in three communities
#' 1. Raw abundances
#' set.seed(123)
#' comm1 <- matrix(c(sample(0:10, 10, replace = TRUE, prob = 1/(1+(0:10))),# higher probability to lower abundances
#'                   sample(1:20, 10, replace = TRUE, prob = 1/(1+(0:19))),# all species present
#'                   sample(0:30, 10, replace = TRUE, prob = 1/(1+(0:30)))),# higher max abundance
#'                 ncol = 10, byrow = TRUE, dimnames = list(paste0("Comm.", 1:3),
#'                                                         paste0("sp.", 1:10)))
#' 
#' taxo_div(comm = comm1)
#' 
#' 2. Relative abundances
#' set.seed(123)
#' comm2 <- matrix(c(sample(seq(0, 1, length.out = 11), 10, replace = TRUE, prob = 1/(1+(0:10))),
#'                   sample(seq(0, 1, length.out = 20), 10, replace = TRUE, prob = 1/(1+(0:19))),
#'                   sample(seq(0, 1, length.out = 31), 10, replace = TRUE, prob = 1/(1+(0:30)))), 
#'                 ncol = 10, byrow = TRUE, dimnames = list(paste0("Comm.", 1:3),
#'                                                          paste0("sp.", 1:10)))
#' 
#' taxo_div(comm = comm2)}
#' 
#' set.seed(NULL)
taxo_div <- function(comm) {
  # species richness (S)
  richness <- apply(comm>0, 1, sum)
  
  # Shannon index (H')
  shannon <- vegan::diversity(comm, index = "shannon")
  
  # Pielou Evenness (J)
  evenness <- shannon/log(richness)
  
  # create tibble with combined results 
  dplyr::tibble(Station = rownames(comm),
                Richness = richness,
                Evenness = evenness,
                Shannon = shannon)
}

