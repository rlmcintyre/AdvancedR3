#'descriptive_statistics
#'
#' @param data is the data file you are starting from
#'
#' @return a data frame or tibble of descriptive statistics
descriptive_stats <- function(data) {
    data |>
        dplyr::group_by(metabolite) |>
        dplyr::summarise(dplyr::across(
            value,
            list(
                mean = mean,
                st_dev = sd
            )
        )) |>
        dplyr::mutate(dplyr::across(
            tidyselect::where(is.numeric),
            ~ round(.x, digits = 1)
        ))}
