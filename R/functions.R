#' Descriptive_statistics
#'
#' @param data is the data file you are starting from
#'
#' @return a data frame or tibble of descriptive statistics
descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}


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



#' Convert a column's character values to snake case format
#'
#' @param data the lipidomics dataset
#' @param columns the column you want to convert to snake case
#'
#' @return a data frame
#'
column_values_to_snake_case <-
  function(data, columns) {
    data |> dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
  }



#' Restructure data frame
#'
#' @param data lipidomics dataset
#'
#' @return a data frame

metabolites_to_wider <- function(data) {
  data |> tidyr::pivot_wider(
    names_from = metabolite,
    values_from = value,
    values_fn = mean,
    names_prefix = "metabolite_"
  )
}



#' A transformation recipe to pre-process the data
#'
#' @param data lipidomics dataset
#' @param metabolite_variable column of the metabolite variable
#'
#' @return data frame

create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) |>
    recipes::update_role({{ metabolite_variable }}, age, gender, new_role = "predictor") |>
    recipes::update_role(class, new_role = "outcome") |>
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}


#' Create a workflow object of the model and transformations
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object

create_model_workflow <- function(model_specs, recipe_specs){
    workflows::workflow() |>
        workflows::add_model(model_specs) |>
        workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model the model workflow object that has been fitted
#'
#' @return a data frame

tidy_model_output <- function (workflow_fitted_model)
    {workflow_fitted_model |>
    workflows::extract_fit_parsnip() |>
    broom::tidy(exponentiate = TRUE)
    }
