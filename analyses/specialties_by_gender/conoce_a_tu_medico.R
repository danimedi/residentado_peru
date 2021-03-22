#' Obtain the specialties from the names of doctors using the web page "Conoce a tu médico"
#'
#' @param apellido_paterno first last name to be included in the form
#' @param apellido_materno second last name to be included in the form
#' @param nombres first name(s) to be included in the form
#' @param cmp número de colegiatura, to be included in the form
#'
#' @return A tibble with the specialties or an empty tibble if there is no specialties
#' @export
#'
#' @examples
#' 
library(rvest)
library(xml2)
library(httr)

conoce_a_tu_medico <- function(apellido_paterno = "", apellido_materno = "", nombres = "", cmp = "") {
  web_page <- "https://200.48.13.39/cmp/php/index.php"
  html <- read_html(web_page)
  form <- html_form(html)[[1]]
  form <- html_form_set(
    form, 
    cmp = cmp, 
    appaterno = apellido_paterno, 
    apmaterno = apellido_materno, 
    nombres = nombres
  )
  resp <- html_form_submit(form)
  
  detail <- content(resp) %>% html_element("#simple-example-table center a") %>% html_attr("href")
  detail <- read_html(paste0("https://200.48.13.39/cmp/php/", detail))
  
  specialty <- detail %>% html_elements("#simple-example-table4")
  if (is.na(html_element(specialty , "td"))) {
    specialty <- NA
  } else {
    specialty <- html_table(specialty)
  }
  names(specialty) <- paste(apellido_paterno, apellido_materno, nombres)
  specialty
}
