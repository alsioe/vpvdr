#' Collect posteriors from VPVD stanfit object
#'
#' @param stanfit The stanfit object you would like to extract posteriors from
#' @param group_tag Regex code to identify the names of the group paramaters
#' @param subject_tag Regex code to identify the names of the subject parameters
#'
#' @return List of lists: One list for grouped parameters and one list for individuals
#' @export
#'
#' @examples
get_posteriors <- function(stanfit,
                           group_tag = 'by_group_drug$',
                           subject_tag = 'subject_effect$') {

    # Some error handling
    if (!exists('stanfit')) {
        stop('Stanfit (S4) object or name of .rds file required.')
        }

    # If the stanfit argument passes on an object, do nothing
    if (typeof(stanfit) == 'S4') wFit <- stanfit

    # If the stanfit argument refers to an .rds file, readRDS it
    if (is.character(stanfit)) wFit <- readRDS(stanfit)

    # Extract the parameters using rstan::extract
    params <- rstan::extract(wFit)

    # We will use stringr to select only the relevant GROUP parameters
    # Note that the $ at the end of the Regex signals that we only want the
    # entries where the tag ends the name of the parameter - this means
    # that the 'raw_unit_normal' parameters are not carried on

    outcomes <- list(
        group_params = params[stringr::str_detect(string = names(params),
                                                   pattern = group_tag)],
        subject_params = params[stringr::str_detect(string = names(params),
                                                     pattern = subject_tag)]
        )

    return(outcomes)
}
