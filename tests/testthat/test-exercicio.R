test_that("encontrar_pior_ano_time() works", {

  # Tibble
  expect_s3_class(encontrar_pior_ano_time(), "tbl")
  expect_equal(ncol(encontrar_pior_ano_time()), 3)
  expect_equal(nrow(encontrar_pior_ano_time()), 1)

  # Valores
  expect_equal(encontrar_pior_ano_time()$temporada, 2019)
  expect_equal(encontrar_pior_ano_time("São Paulo")$temporada, 2017)

  # Erros
  expect_warning(encontrar_pior_ano_time("SãoPaulo"), "no non-missing arguments")
  expect_warning(encontrar_pior_ano_time(1), "no non-missing arguments")
})

# Posso criar mais testes para outras funcoes
#test_that("outra_funcao() works", {
#  # .....
#})
