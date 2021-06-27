#' Obtain the specialties from the names of doctors using the web page "Conoce a tu médico"
#'
#' @param apellido_paterno first last name to be included in the form
#' @param apellido_materno second last name to be included in the form
#' @param nombres first name(s) to be included in the form
#' @param cmp número de colegiatura, to be included in the form
#'
#' @return A tibble with the specialties or an empty tibble if there is no specialties
conoce_a_tu_medico <- function(apellido_paterno = "", apellido_materno = "", nombres = "", cmp = "") {
  web_page <- "https://200.48.13.39/cmp/php/index.php"
  html <- xml2::read_html(web_page)
  form <- rvest::html_form(html)[[1]]
  form <- rvest::html_form_set(
    form,
    cmp = cmp,
    appaterno = apellido_paterno,
    apmaterno = apellido_materno,
    nombres = nombres
  )
  resp <- rvest::html_form_submit(form)

  detail <- httr::content(resp) %>%
    rvest::html_element("#simple-example-table center a") %>%
    rvest::html_attr("href")
  detail <- xml2::read_html(paste0("https://200.48.13.39/cmp/php/", detail))

  specialty <- detail %>% rvest::html_elements("#simple-example-table4")
  if (is.na(rvest::html_element(specialty , "td"))) {
    specialty <- NA
  } else {
    specialty <- rvest::html_table(specialty)
  }
  names(specialty) <- paste(apellido_paterno, apellido_materno, nombres)
  specialty
}
