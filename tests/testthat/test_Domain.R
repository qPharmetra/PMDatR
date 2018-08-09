library(testthat)
library(dplyr)
library(PMDatR)


context("Domain")

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# EX_SUPPEX.yaml contains semi-valid settings for loading a domain with premerge

settings = yaml::yaml.load_file(file.path(path.testroot, "testdata/data1/EX_SUPPEX.yaml"))
#replace filepath and InputMappings$PreMergeFile with valid filenames
exFile = file.path(path.testroot, "testdata/data1/csv/ex.csv")
dom = settings$Domains[[1]]
dom$filepath = exFile
dom$InputMappings$PreMergeFile = exFile

test_that("Check structure Domain",{
  #create test domain
  testdom = domain(dom)
  expect_equal(testdom$valid,T)
})

test_that("Domain no Name",{
  badSettings = dom
  badSettings$name=NULL
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D1.+")
})

test_that("Domain bad Name",{
  badSettings = dom
  badSettings$name="1bad"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D2.+")
})

test_that("Domain file not supplied",{
  badSettings = dom
  badSettings$filepath=NULL
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D3.+")
})

test_that("Domain file blank",{
  badSettings = dom
  badSettings$filepath=""
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D4.+")
})

test_that("Domain file does not exist",{
  badSettings = dom
  badSettings$filepath="adsf"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D4.+")
})


test_that("Bad Keys",{
  badSettings = dom
  badSettings$InputMappings$Keys="good, 1bad, , func(x)"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D5.+")
})

test_that("Bad Premerge Keys",{
  badSettings = dom
  badSettings$InputMappings$PreMergeKeys="good, 1bad, , func(x)"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D6.+")
})

test_that("Bad Premerge Columns with SUPPQUAL set",{
  badSettings = dom
  badSettings$InputMappings$PreMergeCols="ID=ID, EXSEQ=func(, 111"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D10.+")
  expect_match(testdom$errors[[2]],"Error D7.+")
  expect_match(testdom$errors[[3]],"Error D9.+")
})

test_that("Bad Premerge Columns without SUPPQUAL set",{
  badSettings = dom
  badSettings$InputMappings$PreMergeCols="ID=ID, EXSEQ=func(, 111"
  badSettings$InputMappings$PreMergeSupp=F
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D10.+")
  expect_match(testdom$errors[[2]],"Error D7.+")
})

test_that("SUPPQUAL flag is corrupted",{
  badSettings = dom
  badSettings$InputMappings$PreMergeSupp="airplane"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D8.+")
})

test_that("SUPPQUAL premerge has required columns",{
  badSettings = dom
  badSettings$InputMappings$PreMergeCols="USUBJID=USUBJID, EXSEQ=STUFF"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D9.+")
})

# NO, premergekeys need to be in dataset OR premergecols.  Symantic check
# test_that("PreMergeKeys are in PreMergeCOls",{
#   badSettings = dom
#   badSettings$InputMappings$PreMergeCols="USUBJID=USUBJID"
#   badSettings$InputMappings$PreMergeSupp=F
#   #create test domain
#   testdom = domain(badSettings)
#   expect_equal(testdom$valid,F)
#   expect_match(testdom$errors[[1]],"Error D10.+")
# })

test_that("Pre-merge file DNE",{
  badSettings = dom
  badSettings$InputMappings$PreMergeFile="no_file.xpt"
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D11.+")
})

test_that("Pre-merge file empty",{
  badSettings = dom
  badSettings$InputMappings$PreMergeFile=""
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,T)
})

test_that("Domain mapped columns: missing Name or Mapping",{
  badSettings = dom
  badSettings$InputMappings$Columns[[1]]$Name=NULL
  badSettings$InputMappings$Columns[[2]]$Mapping=NULL
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D12.+")
  expect_match(testdom$errors[[2]],"Error D12.+")
})

test_that("Domain mapped columns: illegal Name",{
  badSettings = dom
  badSettings$InputMappings$Columns[[1]]$Name="1badname"
  badSettings$InputMappings$Columns[[2]]$Name="" #also bad
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D13.+")
  expect_match(testdom$errors[[2]],"Error D13.+")
})


test_that("Domain mapped columns: illegal Mappings",{
  badSettings = dom
  badSettings$InputMappings$Columns[[1]]$Mapping="incomplete_function(.."
  badSettings$InputMappings$Columns[[2]]$Mapping=""
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D15.+")
  expect_match(testdom$errors[[2]],"Error D14.+")
})

test_that("Domain mapped columns: name not in source metadata",{
  # this is a reported runtime error if metadata don't exist
  badSettings = dom
  badSettings$SourceMetaData$Columns = lapply(badSettings$InputMappings$Columns,
                                          function(x){names(x)[1]="name";x}) # just need a list with sublist of names
  badSettings$SourceMetaData$Source="manual"
  badSettings$InputMappings$Columns[[1]]$Mapping="bad name"
  badSettings$InputMappings$Columns[[2]]$Mapping="not_in_source"
  badSettings$InputMappings$Columns[[2]]$Required=T # must be a required column in order to trigger semantic check
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,F)
  expect_match(testdom$errors[[1]],"Error D15.+")
  expect_match(testdom$errors[[2]],"Error D16.+")
})

test_that("Domain mapped columns: name not in source but no metadata",{
  # this is a reported runtime error if metadata don't exist
  badSettings = dom
  badSettings$InputMappings$PreMergeFile=""
  badSettings$InputMappings$Columns[[2]]$Mapping="not_in_source"
  badSettings$InputMappings$Columns[[2]]$Required=T # must be a required column in order to trigger semantic check
  #create test domain
  testdom = domain(badSettings)
  expect_equal(testdom$valid,T)
  testdom = load.domain(testdom)
  expect_equal(testdom$valid,F)

  expect_match(testdom$errors[[1]],"Error D16.+")
})

#### Testing of domain preprocessing

dm.settings = list(name="DM",
                   filepath=file.path(path.testroot,"testdata/data1/csv/dm.csv"),
                   InputMappings=list(Keys="ID",
                                      Filter="SUBJID==1",
                                      Columns=list(list(Name="ID", Mapping="USUBJID"),
                                                   list(Name="RFSTDTC", Mapping="iso_to_posix(RFSTDTC)"),
                                                   list(Name="SUBJID", Mapping="SUBJID"),
                                                   list(Name="AGE", Mapping="AGE"))
                                      ))

blank_preprocess=function(dom){dom}


test_that("Select, Transform, and filter of variables in source domain",{
  dm.dom = domain(dm.settings)
  dm.dom = load.domain(dm.dom)
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(dm.dom$Data$AGE, 48)
  expect_equal(dm.dom$Data$SUBJID, 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC), "2013-12-18 09:00:00")
})

test_that("load domain but skip mapping function",{
  dm.dom = domain(dm.settings)
  dm.dom = load.domain(dm.dom, .fun=NULL)
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),10)
  expect_equal(ncol(dm.dom$Data),28)
})

test_that("load domain but override mapping function",{
  dm.dom = domain(dm.settings)
  dm.dom = load.domain(dm.dom, .fun=function(dom){
    dom$Data = data_frame(A=1)
    dom
  })
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(ncol(dm.dom$Data),1)
  expect_equal(dm.dom$Data$A,1)
})

test_that("Select, Transform, and filter of variables in source domain with Hook",{
  this.settings = dm.settings
  this.settings$InputMappings$PreprocessHook="data$AGE='Hello'\ndata"
  dm.dom = domain(this.settings)
  dm.dom = load.domain(dm.dom)
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(dm.dom$Data$AGE, "Hello")
  expect_equal(dm.dom$Data$SUBJID, 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC), "2013-12-18 09:00:00")
})

test_that("Select, Transform, and filter of variables in source domain with Hook bypassed",{
  this.settings = dm.settings
  this.settings$InputMappings$PreprocessHook="data$AGE='Hello'\ndata"
  dm.dom = domain(this.settings)
  dm.dom = load.domain(dm.dom, .hook=NULL)
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(dm.dom$Data$AGE, 48)
  expect_equal(dm.dom$Data$SUBJID, 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC), "2013-12-18 09:00:00")
})

test_that("Select, Transform, and filter of variables in source domain with Hook overridden",{
  this.settings = dm.settings
  this.settings$InputMappings$PreprocessHook="data$AGE='Hello'"
  dm.dom = domain(this.settings)
  dm.dom = load.domain(dm.dom, .hook=function(data){
    data$AGE="Nope"
    data
    })
  ## did we get expected row?
  expect_equal(nrow(dm.dom$Data),1)
  expect_equal(dm.dom$Data$AGE, "Nope")
  expect_equal(dm.dom$Data$SUBJID, 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC), "2013-12-18 09:00:00")
})


tte.dom = domain(list(name="TTE",
                      filepath=file.path(path.testroot,"testdata/data1/csv/adtte.csv")))

test_that("pre-merge non SUPPQUAL",{
  ## add in premerge settings
  test.settings = dm.settings
  test.settings$InputMappings$PreMergeFile=file.path(path.testroot,"testdata/data1/csv/adtte.csv")
  test.settings$InputMappings$PreMergeKeys="SUBJID"
  test.settings$InputMappings$PreMergeCols="CENSOR=CNSR, TTE=AVAL"
  test.settings$InputMappings$PreMergeSupp=F
  dm.dom = domain(test.settings)
  ## extract pre-process function
  #eval(parse(text=dm.dom$fnPreProc))
  dm.dom = load.domain(dm.dom, .fun=NULL)
  ## did we get expected row?
  expect_equal(dm.dom$Data$AGE[1], 48)
  expect_equal(dm.dom$Data$SUBJID[1], 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC[1]), "2013-12-18T09:00")
  expect_equal(dm.dom$Data$CNSR[1], 0)
  expect_equal(dm.dom$Data$TTE[1], 14)
  expect_equal(dm.dom$Data$PARAMCD[1], "Time to Death (days)")
})

##
dm=read.csv(dm.settings$filepath)



test_that("pre-merge SUPPQUAL",{
  ## add in premerge settings
  this.settings=dm.settings
  this.settings$InputMappings$PreMergeFile=file.path(path.testroot,"testdata/data1/csv/suppdm.csv")
  this.settings$InputMappings$PreMergeKeys="SUBJID"
  this.settings$InputMappings$PreMergeCols="SUBJID=as.numeric(IDVARVAL), QNAM=QNAM, QVAL=QVAL"
  this.settings$InputMappings$PreMergeSupp=T
  this.settings$InputMappings$MappedDomain="DM"
  dm.dom = domain(this.settings)

  dm.dom = load.domain(dm.dom, .fun=blank_preprocess)
  ## did we get expected row?
  expect_equal(dm.dom$Data$AGE[1], 48)
  expect_equal(dm.dom$Data$SUBJID[1], 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC[1]), "2013-12-18T09:00")
  expect_equal(dm.dom$Data$DRINKING[1], "HEAVY")
  expect_equal(dm.dom$Data$SMOKING[1], "NONE")

})

test_that("pre-merge SUPPQUAL with duplicate QNAM",{
  ## add in premerge settings
  this.settings=dm.settings
  this.settings$InputMappings$PreMergeFile=file.path(path.testroot,"testdata/data1/csv/suppdm2.csv")
  this.settings$InputMappings$PreMergeKeys="SUBJID"
  this.settings$InputMappings$PreMergeCols="SUBJID=as.numeric(IDVARVAL), QNAM=QNAM, QVAL=QVAL"
  this.settings$InputMappings$PreMergeSupp=T
  this.settings$InputMappings$MappedDomain="DM"
  dm.dom = domain(this.settings)
  ## extract pre-process function
  #eval(parse(text=dm.dom$fnPreProc))
  dm.dom = load.domain(dm.dom, .fun=blank_preprocess)
  ## did we get expected row?
  expect_equal(dm.dom$Data$AGE[1], 48)
  expect_equal(dm.dom$Data$SUBJID[1], 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC[1]), "2013-12-18T09:00")
  expect_equal(dm.dom$Data$DRINKING[1], "HEAVY")
  expect_equal(dm.dom$Data$SMOKING[1], "NONE")
  expect_equal(dm.dom$Data$DRINKING_2[1], "HEAVY")
  expect_equal(dm.dom$Data$SMOKING_2[1], "NONE")

})

test_that("pre-merge SUPPQUAL with filter",{
  ## add in premerge settings
  this.settings=dm.settings
  this.settings$InputMappings$PreMergeFile=file.path(path.testroot,"testdata/data1/csv/suppdm.csv")
  this.settings$InputMappings$PreMergeKeys="SUBJID"
  this.settings$InputMappings$PreMergeCols="SUBJID=as.numeric(IDVARVAL), QNAM=QNAM, QVAL=QVAL"
  this.settings$InputMappings$PreMergeSupp=T
  this.settings$InputMappings$PreMergeFilter="RDOMAIN=='DM'"
  this.settings$InputMappings$MappedDomain="DM"
  dm.dom = domain(this.settings)
  expect_true(dm.dom$valid)
  ## extract pre-process function
  #eval(parse(text=dm.dom$fnPreProc))
  dm.dom = load.domain(dm.dom, .fun=blank_preprocess)
  ## did we get expected row?
  expect_equal(dm.dom$Data$AGE[1], 48)
  expect_equal(dm.dom$Data$SUBJID[1], 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC[1]), "2013-12-18T09:00")
  expect_equal(dm.dom$Data$DRINKING[1], "HEAVY")
  expect_equal(dm.dom$Data$SMOKING[1], "NONE")

})


test_that("pre-merge SUPPQUAL with filter, remove records",{
  ## add in premerge settings
  this.settings=dm.settings
  this.settings$InputMappings$PreMergeFile=file.path(path.testroot,"testdata/data1/csv/suppdm.csv")
  this.settings$InputMappings$PreMergeKeys="SUBJID"
  this.settings$InputMappings$PreMergeCols="SUBJID=as.numeric(IDVARVAL), QNAM=QNAM, QVAL=QVAL"
  this.settings$InputMappings$PreMergeSupp=T
  this.settings$InputMappings$PreMergeFilter="RDOMAIN=='EX'"
  this.settings$InputMappings$MappedDomain="DM"

  dm.dom = domain(this.settings)
  expect_true(dm.dom$valid)
  ## extract pre-process function
  #eval(parse(text=dm.dom$fnPreProc))
  dm.dom = load.domain(dm.dom, .fun=blank_preprocess)
  ## did we get expected row?
  expect_equal(dm.dom$Data$AGE[1], 48)
  expect_equal(dm.dom$Data$SUBJID[1], 1)
  expect_equal(as.character(dm.dom$Data$RFSTDTC[1]), "2013-12-18T09:00")
  # names should be names of domain only (no EX records in the suppdm file)
  expect_equal(names(dm.dom$Data), names(dm))

})
