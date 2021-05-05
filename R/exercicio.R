#' Encontrar pior ano do Brasileirao para um time
#'
#' Usando a base Brasileirao, encontrar pior ano
#' (menor numero de vitorias) de um time na serie historica
#'
#' @param time Nome de um time como na coluna 'quem_ganhou' de
#' [partidas_brasileirao] ("Cruzeiro por padrao")
#'
#' @return Uma tibble com tres colunas
#' @export
#'
#' @examples
#' encontrar_pior_ano_time("Corinthians")
#'
encontrar_pior_ano_time <- function(time = "Cruzeiro") {
  #"https://git.io/JOqUN" %>%
  #  readr::read_csv2() %>%
  meupacote2::partidas_brasileirao %>%
    dplyr::group_by(temporada, quem_ganhou) %>%
    dplyr::filter(quem_ganhou != "Empate", quem_ganhou %in% time) %>%
    dplyr::count(quem_ganhou, sort = TRUE, name = "n_vitorias") %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_vitorias == min(n_vitorias)) %>%
    dplyr::rename("time" = quem_ganhou)
}
