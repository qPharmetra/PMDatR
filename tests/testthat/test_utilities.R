# Unit tests for utilities

context("Utilities")

### Header section
library(testthat)
library(PMDatR)


test_that("is.empty.yaml return T for NULL and missing nodes",{
  #create test node
  node = list(A=NULL,B="",C=1, D=c("","",""))
  expect_equivalent(is.empty.yaml(node),F) # F for top level
  expect_equivalent(is.empty.yaml(node$A),T) # T for NULL
  expect_equivalent(is.empty.yaml(node$B),T) # T for ""
  expect_equivalent(is.empty.yaml(node$C),F) # F for numeric value
  expect_equivalent(is.empty.yaml(node$D),T) # T for all "" in vector

})

test_that("coalesce returns first non-null argument",{
  expect_equivalent(coalesce(NULL,2), 2)
  expect_equivalent(coalesce(NULL,NULL,3), 3)
  expect_equivalent(coalesce(1,NULL), 1)
  expect_equivalent(coalesce(NULL,NULL,NULL), NULL)
})


## test reinterpret_errors
test_that("Test reinterpret_errors",{
  expect_match(tryCatch(eval(expression(A=B)),
                        error=PMDatR:::reinterpret_errors),".+RT1.+")
  expect_match(tryCatch(eval(expression(asdf())),
                        error=PMDatR:::reinterpret_errors),".+RT2.+")
  expect_match(tryCatch(eval(expression(read.csv("asdf.csv"))),
                        error=PMDatR:::reinterpret_errors,
                        warning=PMDatR:::reinterpret_errors),".+Warning RW1.+")
  expect_match(tryCatch(eval(expression({a=list();a[[1]]})),
                        error=PMDatR:::reinterpret_errors),".+RT4.+")
  expect_match(tryCatch(eval(expression(if("A")y=1)),
                        error=PMDatR:::reinterpret_errors),".+RT5.+")
  expect_match(tryCatch(eval(expression(mean[1])),
                        error=PMDatR:::reinterpret_errors),".+RT6.+")
})
