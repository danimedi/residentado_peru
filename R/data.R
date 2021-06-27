#' Data set with the data collected from the Peruvian residency exams from 2013 and 2020
#'
#' This data set was created from the web site of CONAREME, which provides PDF tables
#' of the applicants in different years, the data includes the information for the ordinary exams
#' (some years there were more than one exam, this extraordinary exams are not considered).
#'
#' Each row represents a person in a year, each column is a variable of that person in that year.
#' Notice that the *same person* in different years will have different rows.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{year}{year}
#'   \item{gender}{gender, 1 = female, 0 = male}
#'   \item{Universidad_postulantes}{university of the person at the moment of applying}
#'   \item{Universidad_ingresantes}{university of the person at the moment of entering}
#'   \item{tipo}{specialty and/or subspecialty}
#'   \item{especialidad_subespecialidad_postulantes}{specialty/subspecialty at the moment of applying}
#'   \item{especialidad_subespecialidad_ingresantes}{specialty/subspecialty at the moment of entering}
#'   \item{serum}{score given for SERUM}
#'   \item{bonificacion}{bonus points}
#'   \item{Modalidad}{modality of the application}
#'   \item{V9}{modality of the application}
#'   \item{V11}{modality of the application}
#'   \item{V13}{modality of the application}
#'   \item{Norma}{law during the application}
#'   \item{O.M.}{?}
#'   \item{puntaje}{total score}
#'   \item{nota_final}{total score}
#'   \item{promedio_final}{total score}
#' }
#' @source \url{https://www.conareme.org.pe/web/}
"peru_residentado"
