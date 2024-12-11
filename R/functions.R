#'Descriptive_statistics
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


## Figure outputs

#' distribution plot
#'
#' @param data - the lipidomics dataset
#'
#' @return A gg2plot2 graph

plot_distributions <- function(data) {
    data |>
        ggplot2::ggplot(ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}

