library(testthat)
library(quicR)


# use_r("organize_tables")

for (i in c("test", "test2", "test3")) {

  file <- paste0("input files/", i, ".xlsx")

  test_that(
    "organize_tables returns list?",
    {expect_type(organize_tables(file), "list")}
  )

  test_that(
    "organize_tables returns list of tibbles?",
    {expect_true(
      class(organize_tables(file)[[1]])[1] == "tbl_df" &
        class(organize_tables(file)[[1]])[2] == "tbl" &
        class(organize_tables(file)[[1]])[3] == "data.frame"
    )}
  )
}

test_that(
  "organize_tables accepts 384 as plate arg?",
  {expect_no_error(organize_tables("input files/test384.xlsx", plate=384))}
)
