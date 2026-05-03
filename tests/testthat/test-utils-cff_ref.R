test_that("fallback_dates", {
  xinit <- list(year = "2025")
  p <- fallback_dates(list(year = "2025"))
  expect_identical(xinit, p)

  p2 <- fallback_dates(list("date-published" = "2025-09-25"))
  expect_snapshot(as_cff(p2))
})

test_that("Test institution", {
  # Weak test
  skip_on_cran()

  field_list <- list(
    title = "R: test",
    organization = "A text",
    authors = "An author",
    location = "Vienna, Austria",
    year = "2026",
    doi = "10.32614/R.manuals",
    url = "https://www.R-project.org/",
    bibtex_entry = "manual",
    type = "manual"
  )

  expect_identical(get_bibtex_inst(field_list)$institution, "A text")

  field_list <- list(
    title = "R: test",
    institution = "A text",
    authors = "An author",
    location = "Vienna, Austria",
    year = "2026",
    doi = "10.32614/R.manuals",
    url = "https://www.R-project.org/",
    bibtex_entry = "article",
    type = "article"
  )

  expect_identical(get_bibtex_inst(field_list)$institution, "A text")

  field_list <- list(
    title = "R: test",
    school = person("A person", comment = c(ROR = "033d3q980")),
    authors = "An author",
    location = "Vienna, Austria",
    year = "2026",
    doi = "10.32614/R.manuals",
    url = "https://www.R-project.org/",
    bibtex_entry = "phdthesis",
    type = "phdthesis"
  )

  expect_identical(get_bibtex_inst(field_list)$institution, "A person")

  field_list <- list(
    title = "R: test",
    institution = person("A person", comment = c(ROR = "033d3q980")),
    authors = "An author",
    location = "Vienna, Austria",
    year = "2026",
    doi = "10.32614/R.manuals",
    url = "https://www.R-project.org/",
    bibtex_entry = "article",
    type = "article"
  )

  expect_identical(get_bibtex_inst(field_list)$institution, "A person")
})
