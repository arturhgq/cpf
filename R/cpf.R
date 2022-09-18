#' @title Add leading zeros in cpfs
#' @description `r lifecycle::badge("experimental")`
#'
#' This function adds leading zeros in cpfs
#' @param .cpf cpfs
#' @examples
#' add_digits("22344533")
#' @export
add_digits <- \(.cpf){
  dtools::extract_numbers(.cpf) -> cpf
  stringr::str_split(cpf, "") -> cpf_split
  purrr::map_dbl(cpf_split, length) -> cpf_length

  purrr::map2_dbl(
    .x = cpf,
    .y = cpf_length,
    ~
      if (!is.na(.x)) {
        return(.y)
      } else {
        return(NA_real_)
      }
  ) -> cpf_length

  11 - cpf_length -> cpf_length_remainder
  stringr::str_dup("0", cpf_length_remainder) -> n_zeros

  purrr::map2_chr(
    .x = n_zeros,
    .y = cpf,
    ~
      if (!is.na(.x)) {
        paste0(.x, .y)
      } else {
        NA
      }
  )
}


#' @title Make cpf strings consistent across different patterns
#' @description `r lifecycle::badge("stable")`
#'
#' This function makes cpf strings consistent across different patterns
#' @param .cpf cpf string
#' @examples
#'   fake_cpf <- c(
#'    "111,232-123.23",
#'    "11122233323",
#'    "234..222--454,,00"
#'  )
#'  fix_cpf(fake_cpf)
#' @export
fix_cpf <- \(.cpf) {
  dtools::extract_numbers(.cpf) |>
    add_digits() |>
    stringr::str_replace(
      pattern = "(.{3})(.{3})(.{3})(.{2})",
      replacement = "\\1.\\2.\\3-\\4")
}

#' @title Validate cpf
#' @description `r lifecycle::badge("experimental")`
#'
#' This function validates cpfs by calculating of the check digits
#'
#' @param .data data frame
#' @param .cpf cpf variable
#' @param .clear_steps delete the variables used to validate cpfs
#' @param .fix_cpf [fix_cpf()]
#' @examples
#'
#'data.frame(
#'  cpf = c(
#'    "22233344",
#'    "423.643.534-29",
#'    "458.613.523-23",
#'    13434563434,
#'    NA
#'    )
#'  ) -> data
#'  validate_cpf(data, cpf)
#' @export
validate_cpf <- \(.data, .cpf, .fix_cpf = TRUE, .clear_steps = TRUE) {
  .data |>
    dplyr::group_by(
      {{.cpf}} := forcats::fct_inorder({{.cpf}})
    ) |>
    dplyr::mutate(
      cpf_id = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      {{.cpf}} := add_digits(as.character({{.cpf}})),
      cpf_split = stringr::str_extract_all({{.cpf}}, "[:digit:]"),
      cpf_length = purrr::map_dbl(cpf_split, length),
      cpf_unique = purrr::map_dbl(cpf_split, dplyr::n_distinct),
      cpf_ = purrr::map_chr(
        cpf_split,
        ~ paste0(.x, collapse = "")
      ),
      # digit check 1
      cpf_9digits = stringr::str_extract(cpf_, "\\d{9}"),
      cpf_split = stringr::str_split(cpf_9digits, ""),
      cpf_split = purrr::map(cpf_split, as.numeric),
      cpf_prod = purrr::map(cpf_split, ~ .x * 10:2),
      cpf_sum = purrr::map_dbl(cpf_prod, sum),
      cpf_quotient = cpf_sum%/%11,
      cpf_remainder = cpf_sum %% 11,
      check_digit1 = dplyr::case_when(
        cpf_remainder < 2 ~ 0,
        cpf_remainder >= 2 ~ 11 - cpf_remainder,
      ),
      # digit check 2
      cpf_10digits = stringr::str_extract(cpf_, "\\d{10}"),
      cpf_split = stringr::str_split(cpf_10digits, ""),
      cpf_split = purrr::map(cpf_split, as.numeric),
      cpf_prod = purrr::map(cpf_split, ~ .x * 11:2),
      cpf_sum = purrr::map_dbl(cpf_prod, sum),
      cpf_quotient = cpf_sum%/%11,
      cpf_remainder = cpf_sum %% 11,
      check_digit2 = dplyr::case_when(
        cpf_remainder < 2 ~ 0,
        cpf_remainder >= 2~ 11 - cpf_remainder
      ),
      # comparison
      cpf_digits_var = substr(cpf_,10,11),
      cpf_digits_algorithm = glue::glue("{check_digit1}{check_digit2}", .na = ""),
      cpf_digits_algorithm = dplyr::na_if(cpf_digits_algorithm, ""),
      cpf_check = dplyr::case_when(
        as.character(cpf_digits_var) == cpf_digits_algorithm ~ "valid",
        as.character(cpf_digits_var) != cpf_digits_algorithm ~ "invalid",
        is.na({{.cpf}}) ~ "unknown",
      )
    ) |>
    dplyr::select(
      - dplyr::all_of(
        c("cpf_remainder",
          "cpf_quotient",
          "cpf_sum",
          "cpf_prod",
          "cpf_split")
      )
    ) |>
    dplyr::relocate(
      .after = cpf_,
      cpf_id,
      cpf_length,
      cpf_unique,
      cpf_9digits,
      cpf_10digits,
      check_digit1,
      check_digit2,
      cpf_digits_var,
      cpf_digits_algorithm,
      cpf_check
    ) -> .cpf_data

  if (.fix_cpf) {
    .cpf_data |>
      dplyr::mutate(
        {{.cpf}} := as.character({{.cpf}}),
        {{.cpf}} :=  fix_cpf({{.cpf}})
      ) -> .cpf_data
  }

  if (.clear_steps) {
    .cpf_data |>
      dplyr::select(
        - dplyr::all_of(
          c(
            "cpf_",
            "cpf_digits_algorithm",
            "cpf_digits_var",
            "cpf_9digits",
            "cpf_10digits",
            "cpf_unique",
            "cpf_length",
            "check_digit1",
            "check_digit2")
        )
      )
  } else {
    return(.cpf_data)
  }
}
