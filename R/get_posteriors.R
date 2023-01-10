#' Get posteriors from VPVD stanfit object
#'
#' @param stanfit
#' @param group_tag
#' @param subject_tag
#'
#' @return
#' @export
#'
#' @examples
get_posteriors <- function(stanfit,
                           group_tag = 'by_group_drug$',
                           subject_tag = 'subject_effect$') {

    wFit <- stanfit
    #wFit <- readRDS(stanfit)

    params <- rstan::extract(wFit)

    outcomes <- list(
        group_params = params[stringr::str_detect(string = names(params),
                                                   pattern = group_tag)],
        subject_params = params[stringr::str_detect(string = names(params),
                                                     pattern = subject_tag)]
        )
    return(outcomes)
}
