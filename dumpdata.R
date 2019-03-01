process.Cov <-
function(DM){
  # demographics covariates
  cov1.df = getCov(DM$domains$DM$Data,
                   ID = USUBJID,
                   AGE=AGE,
                   SEX=SEX,
                   RACE=RACE,
                   ETHNIC=ETHNIC,
                   cov.keys = c("ID")
                   )

  # LB covariates
  cov2.df = getCov(DM$domains$LB$Data,
                   ID = USUBJID,
                   cov.filter = LBBLFL=="Y",
                   cov.col = LBTESTCD,
                   cov.val = LBSTRESN,
                   EPOCH=EPOCH,
                   cov.keys = c("ID", "EPOCH"),
                   CREAT=CREAT
                   )
  list(cov1.df, cov2.df)

  # use the default keys for the domain unless overridden here
}
