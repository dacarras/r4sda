#' get_lrt_scf() compares the output of two MPLUS nested models, and calculates the adjusted LRT
#'
#' @param model_0 is de constrained model
#' @param model_1 is the model withe free parameter or parameters
#'
#' @return a string with results one could paste into the manuscipt
#'
#' @export
#'
#' @details simple wrapper for LRT adjusted test (see https://www.statmodel.com/chidiff.shtml)
#'
#' @examples
#'
#' get_lrt_scf(m00, m01) %>% knitr::kable()
#'
get_lrt_scf <- function(model_0, model_1){
  require(dplyr)
  require(purrr)
  options(scipen = 999)

  deviance_0 <- model_0 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(LL) %>%
    mutate(Deviance = -2*LL) %>%
    dplyr::select(Deviance) %>%
    .$Deviance %>%
    as.numeric()

  deviance_1 <- model_1 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(LL) %>%
    mutate(Deviance = -2*LL) %>%
    dplyr::select(Deviance) %>%
    .$Deviance %>%
    as.numeric()

  scaling_0 <- model_0 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(LLCorrectionFactor) %>%
    mutate(scaling = LLCorrectionFactor) %>%
    dplyr::select(scaling) %>%
    .$scaling %>%
    as.numeric()

  nparan_0   <- model_0 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(Parameters) %>%
    .$Parameters %>%
    as.numeric()

  nparan_1   <- model_1 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(Parameters) %>%
    .$Parameters %>%
    as.numeric()

  scaling_1 <- model_1 %>%
    purrr::pluck("summaries") %>%
    tibble::as_tibble() %>%
    dplyr::select(LLCorrectionFactor) %>%
    mutate(scaling = LLCorrectionFactor) %>%
    dplyr::select(scaling) %>%
    .$scaling %>%
    as.numeric()

  cd  <- (nparan_0 * scaling_0 - nparan_1*scaling_1)/(nparan_0 - nparan_1)

  trd <- (deviance_0 - deviance_1)/cd

  chi      <- pchisq(trd, nparan_1-nparan_0, lower.tail=FALSE)
  dif_par  <- nparan_1-nparan_0
  dif_dev  <- deviance_0-deviance_1

  chi_text <- decimal(chi, 2)
  dev_text <- decimal(dif_dev, 2)

  return(paste0('LRT (',dif_par,') = ',dev_text,' p = ',chi_text))
}
