# test the merge functions

library(testthat)
library(dplyr)
library(PMDatR)
library(parsedate)
library(tidyr)

context("Test Merging functions")

# prepare sample data frames

IDs=1:3
times = 0:6*86400 + parse_iso_8601("2010-12-25T08:35:12")


ex.df = data.frame(ID = IDs, TIME=times[1], AMT=100, EVID=1)

pc.df = expand.grid(ID=IDs,TIME=times) %>% arrange(ID,TIME) %>%
  group_by(ID) %>% mutate(EVID=0, DV=1:n())

dm.df = data.frame(ID=IDs, WT=IDs, AGE=IDs, HT=IDs) %>% group_by(ID)

vs.df = expand.grid(ID=IDs,TIME=times-.5*86400) %>% arrange(ID,TIME) %>%
  group_by(ID) %>% mutate(EVID=2, BPSYS = ID, BPDIA = ID)



test_that("merge process works correctly",{
  # no problems expected

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  events.df = events.df %>% mutate_each(funs(locf),BPSYS,BPDIA)

  expect_equal(names(events.df),c('ID','TIME','AMT','EVID','DV','BPSYS','BPDIA','WT','AGE','HT')) #check column names
  expect_equal(events.df$ID,rep(1:3, each=15)) #check ID
  expect_equal(as.numeric(events.df$TIME),
               as.numeric(rep(sort(c(times,times[1],times-.5*86400)),3))) # check dates
  expect_equal(events.df$EVID, rep(c(2,1,0,2,0,2,0,2,0,2,0,2,0,2,0),3)) #check EVID, expecting 2,1,0,2,0,2..
  expect_equal(events.df$DV, rep(c(NA,NA,1,NA,2,NA,3,NA,4,NA,5,NA,6,NA,7),3)) #check DV, expecting NA,NA,1,NA,2,NA,3...
  expect_equal(events.df$AMT,rep(c(NA,100,rep(NA,13)), 3)) #check AMT
  # covariate values should be same as ID
  expect_equal(events.df$WT,events.df$ID)
  expect_equal(events.df$AGE,events.df$ID)
  expect_equal(events.df$HT,events.df$ID)
  expect_equal(events.df$BPSYS,events.df$ID)
  expect_equal(events.df$BPDIA,events.df$ID)

})


### use post.merge.refactoring
test_that("post.merge.refactoring works with defaults",{
  # no problems expected

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  tranfun = function(.data){.data %>% mutate_each(funs(locf),BPSYS,BPDIA)}

  events.df = post.merge.refactoring(events.df, fun.transform = tranfun, options=NULL)

  expect_equal(names(events.df),c('RECID','ID','TIME','AMT','EVID','DV','BPSYS','BPDIA',
                                  'WT','AGE','HT', 'ELTM', 'TAFD', 'NDOSE', 'TAD')) #check column names
  expect_equal(events.df$ID,rep(1:3, each=8)) #check ID
  expect_equal(as.numeric(events.df$TIME),
               as.numeric(rep(sort(c(times,times[1])),3))) # check dates
  expect_equal(events.df$EVID, rep(c(1,0,0,0,0,0,0,0),3)) #check EVID, expecting 2,1,0,2,0,2..
  expect_equal(events.df$DV, rep(c(NA,1:7),3)) #check DV, expecting NA,NA,1,NA,2,NA,3...
  expect_equal(events.df$AMT,rep(c(100,rep(0,7)), 3)) #check AMT
  # covariate values should be same as ID
  expect_equal(events.df$WT,events.df$ID)
  expect_equal(events.df$AGE,events.df$ID)
  expect_equal(events.df$HT,events.df$ID)
  expect_equal(events.df$BPSYS,events.df$ID)
  expect_equal(events.df$BPDIA,events.df$ID)
})

test_that("post.merge.refactoring fails with bad fun.transform",{
  # no problems expected

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  tranfun = function(.data){.data %>% mutate_each(funs(locf),NOPE,BPDIA)}

  events.df = post.merge.refactoring(events.df, fun.transform = tranfun, options=NULL)

})

test_that("post.merge.refactoring works with KeepEVID2",{
  # no problems expected
  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  tranfun = function(.data){.data %>% mutate_each(funs(locf),BPSYS,BPDIA)}

  events.df = post.merge.refactoring(events.df, fun.transform = tranfun,
                                     options=list(KeepEvid2=T))

  expect_equal(names(events.df),c('RECID','ID','TIME','AMT','EVID','DV','BPSYS','BPDIA',
                                  'WT','AGE','HT', 'ELTM', 'TAFD', 'NDOSE', 'TAD')) #check column names
  expect_equal(events.df$ID,rep(1:3, each=15)) #check ID
  expect_equal(as.numeric(events.df$TIME),
               as.numeric(rep(sort(c(times,times[1],times-.5*86400)),3))) # check dates
  expect_equal(events.df$EVID, rep(c(2,1,0,2,0,2,0,2,0,2,0,2,0,2,0),3)) #check EVID, expecting 2,1,0,2,0,2..
  expect_equal(events.df$DV, rep(c(NA,NA,1,NA,2,NA,3,NA,4,NA,5,NA,6,NA,7),3)) #check DV, expecting NA,NA,1,NA,2,NA,3...
  expect_equal(events.df$AMT,rep(c(0,100,rep(0,13)), 3)) #check AMT
  # covariate values should be same as ID
  expect_equal(events.df$WT,events.df$ID)
  expect_equal(events.df$AGE,events.df$ID)
  expect_equal(events.df$HT,events.df$ID)
  expect_equal(events.df$BPSYS,events.df$ID)
  expect_equal(events.df$BPDIA,events.df$ID)

})

test_that("post.merge.refactoring works with EVID Sort Order different",{
  # no problems expected
  vs2.df = expand.grid(ID=IDs,TIME=times) %>% arrange(ID,TIME) %>%
    group_by(ID) %>% mutate(EVID=2, BPSYS = ID, BPDIA = ID)

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs2.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  tranfun = function(.data){.data %>% mutate_each(funs(locf),BPSYS,BPDIA)}

  events.df = post.merge.refactoring(events.df, fun.transform = tranfun,
                                     options=list(EVIDorder=c(2,1,0,3,4),
                                                  KeepEvid2=T))
  #just check the evid order 2,1,0
  expect_equal(events.df$EVID[1:3],c(2,1,0))

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs2.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  events.df = post.merge.refactoring(events.df, fun.transform = tranfun,
                                     options=list(EVIDorder=c(0,1,2,3,4),
                                                  KeepEvid2=T))
  #just check the evid order 0,1,2
  expect_equal(events.df$EVID[1:3],c(0,1,2))

  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs2.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  events.df = post.merge.refactoring(events.df, fun.transform = tranfun,
                                     options=list(EVIDorder=c(2,1,0,3,4),
                                                  KeepEvid2=F))
  #just check the evid order 1,0,0 because we clip out the 2
  expect_equal(events.df$EVID[1:3],c(1,0,0))
})

test_that("post.merge.refactoring works with custom sort order",{
  # no problems expected
  events.df = PMDatR::append.events(ex.df, pc.df)
  events.df = PMDatR::append.CovT(events.df,vs.df)
  events.df = PMDatR::merge.Cov(events.df,list(dm.df))
  tranfun = function(.data){.data %>% mutate_each(funs(locf),BPSYS,BPDIA)}

  events.df = post.merge.refactoring(events.df, fun.transform = tranfun,
                                     options=list(SortOrder=c("TIME","ID")))

  expect_equal(names(events.df),c('RECID','ID','TIME','AMT','EVID','DV','BPSYS','BPDIA',
                                  'WT','AGE','HT', 'ELTM', 'TAFD', 'NDOSE', 'TAD')) #check column names
  expect_equal(events.df$ID,c(rep(1:3, each=2),rep(1:3,times=6 ))) #check ID
  expect_equal(as.numeric(events.df$TIME),
               as.numeric(sort(rep(c(times,times[1]),3)))) # check dates

})

test_that("right join removes subjects missing in covariates",{
  # take ID=1 out of dm.df and merge with pc.df.  see that ID=1 is removed
  dm2.df = dm.df %>% filter(ID!=1)
  attr(dm2.df,"join_type") = "right"
  events.df = merge.Cov(pc.df,list(dm2.df))

  expect_equal(unique(events.df$ID),2:3) #check ID
})

test_that("inner join retains only IDs matching in both pc and dm",{
  # change ID=1 to 4 in pc.  so pc has 2,3,4 and dm has 1,2,3.  only 2,3, match
  dm2.df=dm.df
  pc2.df = pc.df %>% group_by() %>% mutate(ID=ifelse(ID==1,4,ID))
  attr(dm2.df,"join_type") = "inner"
  events.df = merge.Cov(pc2.df,list(dm2.df))

  expect_equal(unique(events.df$ID),2:3) #check ID only keeps 2 and 3 that are in both sets
})

test_that("full join retains all IDs",{
  # change ID=1 to 4 in pc.  so pc has 2,3,4 and dm has 1,2,3.
  dm2.df=dm.df
  attr(dm2.df,"join_type") = "full"
  pc2.df = pc.df %>% group_by() %>% mutate(ID=ifelse(ID==1,4,ID))

  events.df = merge.Cov(pc2.df,list(dm2.df))

  expect_equal(sort(unique(events.df$ID)), 1:4) #check ID keeps 1,2,3,4
})

test_that("test that default is left join",{
  events.df = merge.Cov(pc.df,list(dm.df))
  #set up with left join selected
  dm2.df = dm.df
  attr(dm2.df,"join_type") = "left"
  events2.df = merge.Cov(pc.df,list(dm2.df))

  expect_equal(events.df, events2.df) #check ID
})


context("pre-merge")

dmsupp.df = data.frame(ID=IDs+1, THING=IDs) %>% group_by(ID)
write.csv(dm.df,"dm.csv",quote=F,row.names = F)
write.csv(dmsupp.df,"dmsupp.csv",quote=F,row.names=F)
dom1=list(name="DM",filepath="dm.csv",txtSettings=list(sep=",", header=T))
dom2=list(name="SUPPDM",filepath="dmsupp.csv",txtSettings=list(sep=",", header=T))

dom1=domain(dom1)
dom1=load.domain(dom1)
dom2=domain(dom2) #don't load dom2

test_that("pre-merge process works with left join correctly",{
  # no problems expected


  dm.pm.df = pre.merge(dom1,dom2,keys="ID",jointype = "left", THING=THING)

  # THING[1] will be NA, b/c subject 1 is not in dom2
  expect_true(is.na(dm.pm.df$THING[1]))
})

test_that("pre-merge process works with right join correctly",{
  # no problems expected

  dm.pm.df = pre.merge(dom1,dom2,keys="ID",jointype="right", THING=THING)

  # subject 1 is gone, b/c subject 1 is not in dom2
  expect_false(1 %in% dm.pm.df$ID)
})

test_that("pre-merge process works with inner join correctly",{
  # no problems expected

  dm.pm.df = pre.merge(dom1,dom2,keys="ID",jointype="inner", THING=THING)

  # subject 1 and 4 are gone, b/c subject 1 is not in dom2 and 4 not in dom1
  expect_false(all(c(1,4) %in% dm.pm.df$ID))
})

test_that("pre-merge process works with full join correctly",{
  # no problems expected

  dm.pm.df = pre.merge(dom1,dom2,keys="ID",jointype="full", THING=THING)

  # subjects 1:4 present.  THING is NA for 1, and WT is NA for 4
  expect_true(all(c(1,4) %in% dm.pm.df$ID))
  expect_true(is.na(dm.pm.df[dm.pm.df$ID==1,"THING"]))
  expect_true(is.na(dm.pm.df[dm.pm.df$ID==4,"WT"]))
})

test_that("pre-merge process works from load.domain",{
  dom1=domain(list(name="DM", filepath="dm.csv",FileSettings=list(sep=",", header=T), InputMappings=
               list(PreMergeFile="dmsupp.csv", PreMergeKeys="ID", PreMergeCols="THING=THING", PreMergeSupp=F)))
  dom1=load.domain(dom1)
})

#remove dm files
file.remove(c("dm.csv","dmsupp.csv"))

### test post_process_dosing
library(tibble)
library(dplyr)
### create example ex table
ex_dat <- tribble(
  ~ID,	~TIME,	~TRT,	~EVID, ~AMT,	~II,	~MEX,
  1,	"12/14/2016 09:00",	"A",	1, 100,	0,	F,
  1,	"12/21/2016 10:12",	"A",	1, 100,	24,	  F,
  1,	"01/01/2017   0:00", "A",  1, 0,	  24,	  T,
  1,	"01/11/2017 07:33",	  "A",	1, 100,	0,	  F
) %>% mutate(TIME = iso_to_posix(TIME))

test_that("post_process_dosing works with no missed doses",{
          test_df = ex_dat %>% filter(MEX==F)
          ref= tribble(
            ~ID,  ~TIME,  ~TRT,  ~EVID,   ~AMT,    ~II, ~MEX,  ~ADDL,

             1, "2016-12-14 09:00:00",     "A",     1,   100,     0,  F,   0,
             1, "2016-12-21 10:12:00",     "A",     1,   100,     0,  F,   0,
             1, "2016-12-22 07:33:00",     "A",     1,   100,    24,  F,  19,
             1, "2017-01-11 07:33:00",     "A",     1,   100,     0,  F,   0
          ) %>% mutate(TIME = iso_to_posix(TIME))
          expect_equal(post_process_dosing(test_df),ref)
          })

test_that("post_process_dosing works with  missed doses",{
  test_df = ex_dat
  ref = tribble(
    ~ID,  ~TIME,   ~TRT,  ~EVID,   ~AMT,    ~II,   ~MEX,  ~ADDL,
    1, "2016-12-14 09:00:00",     "A",     1,   100,     0, FALSE,     0,
    1, "2016-12-21 10:12:00",     "A",     1,   100,     0, FALSE,     0,
    1, "2016-12-22 07:33:00",     "A",     1,   100,    24, FALSE,     9,
    1, "2017-01-01 00:00:00",     "A",     1,     0,     0,  TRUE,     0,
    1, "2017-01-02 07:33:00",     "A",     1,   100,    24,  FALSE,     8,
    1, "2017-01-11 07:33:00",     "A",     1,   100,     0, FALSE,     0
  ) %>% mutate(TIME=iso_to_posix(TIME))

  expect_equal(post_process_dosing(test_df),ref)
})

test_that("post_process_dosing EVID=1 on missed DOSE is reset",{
  test_df = ex_dat
  test_df[4,"EVID"]=1
  ref = tribble(
    ~ID,  ~TIME,   ~TRT,  ~EVID,   ~AMT,    ~II,   ~MEX,  ~ADDL,
    1, "2016-12-14 09:00:00",     "A",     1,   100,     0, FALSE,     0,
    1, "2016-12-21 10:12:00",     "A",     1,   100,     0, FALSE,     0,
    1, "2016-12-22 07:33:00",     "A",     1,   100,    24, FALSE,     9,
    1, "2017-01-01 00:00:00",     "A",     1,     0,     0,  TRUE,     0,
    1, "2017-01-02 07:33:00",     "A",     1,   100,    24, FALSE,     8,
    1, "2017-01-11 07:33:00",     "A",     1,   100,     0, FALSE,     0
  ) %>% mutate(TIME=iso_to_posix(TIME))
  expect_equal(post_process_dosing(test_df),ref)
})


test_that("post_process_dosing DOSE<>0 on missed DOSE is reset",{
    test_df = ex_dat
    test_df[4,"AMT"]=100
    ref = tribble(
      ~ID,  ~TIME,   ~TRT,  ~EVID,   ~AMT,    ~II,   ~MEX,  ~ADDL,
      1, "2016-12-14 09:00:00",     "A",     1,   100,     0, FALSE,     0,
      1, "2016-12-21 10:12:00",     "A",     1,   100,     0, FALSE,     0,
      1, "2016-12-22 07:33:00",     "A",     1,   100,    24, FALSE,     9,
      1, "2017-01-01 00:00:00",     "A",     1,     0,     0,  TRUE,     0,
      1, "2017-01-02 07:33:00",     "A",     1,   100,    24, FALSE,     8,
      1, "2017-01-11 07:33:00",     "A",     1,   100,     0, FALSE,     0
    ) %>% mutate(TIME=iso_to_posix(TIME))

  expect_equal(post_process_dosing(test_df),ref)
})

remove(ID)
test_that("post_process_dosing works with no missed doses and split CMT",{
  #test_df = ex_dat %>% filter(MEX==F) %>% select(-MEX) %>% mutate(CMT="DOSE.1, DOSE.2")
  test_df = ex_dat %>% filter(MEX==F) %>% select(-MEX) %>%
    getIndividualDoses(ID=ID, TIME=TIME, TRT=TRT, EVID=EVID, AMT=AMT, II=II, CMT="DOSE.1, DOSE.2") %>%
    # remove MDV for this test
    select(-MDV)
  ref= tribble(
    ~ID,  ~TIME,  ~TRT,  ~EVID,   ~AMT,    ~II,  ~ADDL, ~CMT,

    1, "2016-12-14 09:00:00",     "A",     1,   100,     0, 0, "DOSE.1",
    1, "2016-12-14 09:00:00",     "A",     1,   100,     0, 0, "DOSE.2",
    1, "2016-12-21 10:12:00",     "A",     1,   100,     0, 0, "DOSE.1",
    1, "2016-12-21 10:12:00",     "A",     1,   100,     0, 0, "DOSE.2",
    1, "2016-12-22 07:33:00",     "A",     1,   100,    24, 19, "DOSE.1",
    1, "2016-12-22 07:33:00",     "A",     1,   100,    24, 19, "DOSE.2",
    1, "2017-01-11 07:33:00",     "A",     1,   100,     0, 0, "DOSE.1",
    1, "2017-01-11 07:33:00",     "A",     1,   100,     0, 0, "DOSE.2"
  ) %>% mutate(TIME = iso_to_posix(TIME))
  expect_equal(post_process_dosing(test_df),ref)
})

ex_dat2 = tribble(
                ~ID,	~TIME,	~AMT,	~EVID,	~CMT,	~II,	~MEX,	~SPLIT,
                "STUDY-01-1",	"2013-12-18T09:00",	100,	1,	"ANALYTE1",	0,	FALSE,	1,
                "STUDY-01-1",	"2013-12-22T08:30",	100,	1,	"ANALYTE1",	24,	FALSE,	1,
                "STUDY-01-1",	"2013-12-28T10:27",	100,	1,	"ANALYTE1",	0,	FALSE,	1,
                "STUDY-01-3",	"2013-12-18T09:10",	100,	1,	"ANALYTE1",	0,	FALSE,	1,
                "STUDY-01-3",	"2013-12-22T08:10",	100,	1,	"ANALYTE1",	12,	FALSE,	1,
                "STUDY-01-3",	"2013-12-24T00:00",	0,	  1,	"ANALYTE1",	12,	TRUE,	1,
                "STUDY-01-3",	"2013-12-28T06:55",	100,	1,	"ANALYTE1",	0,	FALSE,	1,
                "STUDY-01-4",	"2013-12-18T09:00",	100,	1,	"ANALYTE1",	0,	FALSE,	2,
                "STUDY-01-4",	"2013-12-22T08:30",	100,	1,	"ANALYTE1",	24,	FALSE,	2,
                "STUDY-01-4",	"2013-12-28T10:27",	100,	1,	"ANALYTE1",	0,	FALSE,	2
) %>% mutate(TIME = iso_to_posix(TIME))

test_that("post_process_dosing works with multiple subjects",{
  #test_df = ex_dat2
  test_df = ex_dat2  %>% select(-SPLIT) %>%
    getIndividualDoses(ID=ID, TIME=TIME, EVID=EVID, AMT=AMT, II=II, MEX=MEX,
                       CMT=ifelse(ID=="STUDY-01-4","ANALYTE1.1, ANALYTE1.2","ANALYTE1.1")) %>%
    #remove MDV just for these checks
    select(-MDV)
  ref= tribble(
    ~ID,	~TIME,	~AMT,	~EVID,	~CMT,	~II,	~MEX, ~ADDL,
    "STUDY-01-1",	"2013-12-18 09:00",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-1",	"2013-12-22 08:30",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-1",	"2013-12-23 10:27",	100,	1,	"ANALYTE1.1",	24,	FALSE,		4,
    "STUDY-01-1",	"2013-12-28 10:27",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,

    "STUDY-01-3",	"2013-12-18 09:10",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-3",	"2013-12-22 08:10",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-3",	"2013-12-22 18:55",	100,	1,	"ANALYTE1.1",	12,	FALSE,		2,
    "STUDY-01-3",	"2013-12-24 00:00",	0,	  1,	"ANALYTE1.1",	0,	TRUE,		  0,
    "STUDY-01-3",	"2013-12-24 18:55",	100,	1,	"ANALYTE1.1",	12,	FALSE,		6,
    "STUDY-01-3",	"2013-12-28 06:55",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,

    "STUDY-01-4",	"2013-12-18 09:00",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-4",	"2013-12-18 09:00",	100,	1,	"ANALYTE1.2",	0,	FALSE,		0,
    "STUDY-01-4",	"2013-12-22 08:30",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-4",	"2013-12-22 08:30",	100,	1,	"ANALYTE1.2",	0,	FALSE,		0,
    "STUDY-01-4",	"2013-12-23 10:27",	100,	1,	"ANALYTE1.1",	24,	FALSE,		4,
    "STUDY-01-4",	"2013-12-23 10:27",	100,	1,	"ANALYTE1.2",	24,	FALSE,		4,
    "STUDY-01-4",	"2013-12-28 10:27",	100,	1,	"ANALYTE1.1",	0,	FALSE,		0,
    "STUDY-01-4",	"2013-12-28 10:27",	100,	1,	"ANALYTE1.2",	0,	FALSE,		0
  ) %>% mutate(TIME = iso_to_posix(TIME)) %>%
    select(ID, TIME, AMT, EVID, II, MEX, CMT, ADDL)

  expect_equal(post_process_dosing(test_df) %>% arrange(ID,TIME),ref)
})

### post.merge.processesing

