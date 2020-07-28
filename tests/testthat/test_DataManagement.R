library(testthat)
library(PMDatR)
library(dplyr)

context("DataManagement")

path.testroot = "tests" # for testing in console
path.testroot = ".." # for build

# utility function to use here
get.meta.col.names = function(meta) unlist(lapply(meta,"[[","name"))

# example.yaml contains valid settings for different query types in data1 example
settings_file = file.path(path.testroot, "testdata/data1/example.yaml")

#settings = yaml::yaml.load_file(settings_file)

test_that("Check structure DM",{
  #create queryDV
  DM = DataManagement(settings_file)

})

# can't test RMD functionality from testthat
# test_that("RMD creates function text",{
#   outpath = file.path(path.testroot,"testthat/test1.rmd")
#   rmarkdown::draft(outpath, template = "Process_Settings", package = "PMDatR", edit=F)
#   rmarkdown::render(outpath, params = list(settings_file = file.path("../..",settings_file)))
#
# })


test_that("write_post_transform processes", {
  checktxt = paste0("function(.data){\n.data %>%\npost_transform_start('ELTM') ",
                    "%>%\ngroup_by(ID) %>% transform(ELTM=elapsed.time(TIME)) ",
                    "%>%\npost_transform_end('ELTM') %>%\npost_transform_start",
                    "('TAFD') %>%\ntime_after_first_dose(groups='ID', .name='TA",
                    "FD') %>%\npost_transform_end('TAFD') %>%\npost_transform_st",
                    "art('NDOSE') %>%\ngroup_by(ID) %>% transform(NDOSE = pmax(1",
                    ",cumsum(EVID==1))) %>%\npost_transform_end('NDOSE') %>%\npos",
                    "t_transform_start('TAD') %>%\ntime_after_first_dose(groups",
                    "=.(ID,NDOSE), .name='TAD') %>%\npost_transform_end('TAD') ",
                    "%>%\npost_transform_start('EVDT') %>%\ntransform(EVDT=for",
                    "mat_date(TIME)) %>%\npost_transform_end('EVDT') %>%\npost_",
                    "transform_start('EVTM') %>%\ntransform(EVTM=format_time(TI",
                    "ME)) %>%\npost_transform_end('EVTM') %>%\npost_transform_s",
                    "tart('FDDTTM') %>%\ngroup_by(ID) %>% time_of_first_dose('F",
                    "DDTTM') %>%\npost_transform_end('FDDTTM') %>%\npost_transf",
                    "orm_start('FDDT') %>%\ntransform(FDDT=format_date(FDDTTM,",
                    "'%Y%m%d')) %>%\npost_transform_end('FDDT') %>%\npost_transf",
                    "orm_start('FDTM') %>%\ntransform(FDTM=format_time(FDDTTM, '",
                    "%H%M')) %>%\npost_transform_end('FDTM') %>%\npost_transform",
                    "_start('SUBJID') %>%\ntransform(SUBJID=parse_usubjid(ID,5)) ",
                    "%>%\npost_transform_end('SUBJID') %>%\n#assign units\nconve",
                    "rt_units_from_list(.ul=list(ELTM='h',TAFD='h',TAD='h',AGE='",
                    "AGEU'))\n\n}")
  DMtest = DataManagement(settings_file)
  expect_equal(PMDatR:::write_post_transform(DMtest),checktxt)
})
