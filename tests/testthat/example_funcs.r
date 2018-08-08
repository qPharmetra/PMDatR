
### Header
library(PMDatRLillyExt)

### Load and Preprocess domains
preprocess.domains <- function(DMobj) {
    DMobj$Domains$DM = load.domain(DMobj$Domains$DM, 
        .fun = preprocess_DM)
    # put the data into a global variable named after
    # the domain
    
    DM <<- DMobj$Domains$DM$Data
    DMobj$Domains$EX = load.domain(DMobj$Domains$EX, 
        .fun = preprocess_EX)
    # put the data into a global variable named after
    # the domain
    
    EX <<- DMobj$Domains$EX$Data
    DMobj$Domains$LB = load.domain(DMobj$Domains$LB, 
        .fun = preprocess_LB)
    # put the data into a global variable named after
    # the domain
    
    LB <<- DMobj$Domains$LB$Data
    DMobj$Domains$PC = load.domain(DMobj$Domains$PC, 
        .fun = preprocess_PC)
    # put the data into a global variable named after
    # the domain
    
    PC <<- DMobj$Domains$PC$Data
    DMobj
}

## pre-processing functions apply filters,
## transformations, pre-merge specified by settings.
## Set data in dom$Data, and return dom

# pre-processing function for domain: DM

preprocess_DM <- function(dom) {
    dom$Data = getDomain(dom$Data, STUDYID = STUDYID, 
        DOMAIN = DOMAIN, USUBJID = USUBJID, SUBJID = SUBJID, 
        RFSTDTC = RFSTDTC, RFENDTC = RFENDTC, RFXSTDTC = RFXSTDTC, 
        RFXENDTC = RFXENDTC, RFICDTC = RFICDTC, RFPENDTC = RFPENDTC, 
        DTHDTC = DTHDTC, DTHFL = DTHFL, SITEID = SITEID, 
        INVID = INVID, INVNAM = INVNAM, BRTHDTC = BRTHDTC, 
        AGE = AGE, AGEU = AGEU, SEX = SEX, RACE = RACE, 
        ETHNIC = ETHNIC, ARMCD = ARMCD, ARM = ARM, 
        ACTARMCD = ACTARMCD, ACTARM = ACTARM, COUNTRY = COUNTRY)
    dom
}


# pre-processing function for domain: EX

preprocess_EX <- function(dom) {
    dom$Data = getDomain(dom$Data, EXDTC = iso_to_posix(EXSTDTC), 
        EXSTDTC = iso_to_posix(EXSTDTC), EXENDTC = iso_to_posix(EXENDTC), 
        STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID, 
        EXSEQ = EXSEQ, EXTRT = EXTRT, EXDOSE = EXDOSE, 
        EXDOSU = EXDOSU, EXDOSFRM = EXDOSFRM, EXDOSFRQ = EXDOSFRQ, 
        EXOCCUR = EXOCCUR, EXROUTE = EXROUTE, VISITNUM = VISITNUM, 
        VISIT = VISIT, EPOCH = EPOCH, EXSTDY = EXSTDY, 
        EXENDY = EXENDY, EXTPT = EXTPT, EXTPTNUM = EXTPTNUM, 
        II = replace_values(EXDOSFRQ, QD = 24, ONCE = 0, 
            BID = 12, WEEKLY = 168))
    dom
}


# pre-processing function for domain: LB

preprocess_LB <- function(dom) {
    dom$Data = getDomain(dom$Data, STUDYID = STUDYID, 
        DOMAIN = DOMAIN, USUBJID = USUBJID, LBSEQ = LBSEQ, 
        LBTESTCD = LBTESTCD, LBTEST = LBTEST, LBCAT = LBCAT, 
        LBORRES = LBORRES, LBORRESU = LBORRESU, LBORNRLO = LBORNRLO, 
        LBORNRHI = LBORNRHI, LBSTRESC = LBSTRESC, LBSTRESN = LBSTRESN, 
        LBSTRESU = LBSTRESU, LBSTNRLO = LBSTNRLO, LBSTNRHI = LBSTNRHI, 
        LBNRIND = LBNRIND, LBSTAT = LBSTAT, LBREASND = LBREASND, 
        LBNAM = LBNAM, LBSPEC = LBSPEC, LBSPCCND = LBSPCCND, 
        LBBLFL = LBBLFL, VISITNUM = VISITNUM, VISIT = VISIT, 
        EPOCH = EPOCH, LBDTC = iso_to_posix(LBDTC), 
        LBDY = LBDY)
    dom
}


# pre-processing function for domain: PC

preprocess_PC <- function(dom) {
    dom$Data = getDomain(dom$Data, STUDYID = STUDYID, 
        DOMAIN = DOMAIN, USUBJID = USUBJID, PCSEQ = PCSEQ, 
        PCGRPID = PCGRPID, PCTESTCD = PCTESTCD, PCTEST = PCTEST, 
        PCORRES = PCORRES, PCORRESU = PCORRESU, PCSTRESC = PCSTRESC, 
        PCSTRESN = PCSTRESN, PCSTRESU = PCSTRESU, PCSTAT = PCSTAT, 
        PCREASND = PCREASND, PCNAM = PCNAM, PCSPEC = PCSPEC, 
        VISITNUM = VISITNUM, VISIT = VISIT, EPOCH = EPOCH, 
        TIME = iso_to_posix(PCDTC), PCDY = PCDY, PCTPT = PCTPT, 
        PCTPTNUM = PCTPTNUM)
    dom
}


Process_Dose <- function() {
    Dose = getIndividualDoses(EX, ID = USUBJID, TIME = parsedate(EXDTC), 
        AMT = EXDOSE, EVID = 1, CMT = 1, Units = EXDOSU, 
        EPOCH = EPOCH, VISIT = VISIT, DAY = EXSTDY)
    bind_rows(Dose)
    
}

Process_DV <- function() {
    DV1 = getDV(PC, ID = USUBJID, TIME = TIME, DV = PCSTRESN, 
        EVID = 0, CMT = 2, MDV = PCSTAT == "NOT DONE", 
        Units = PCSTRESU, dv.filter = PCTESTCD == "ANALYTE", 
        VISIT = VISIT, EPOCH = EPOCH, DAY = PCDY)
    bind_rows(DV1)
    
}

Process_Cov <- function() {
    Demog = getCov(DM, ID = USUBJID, cov.filter = ARM != 
        "NOT ASSIGNED", cov.keys = c("ID"), AGE = AGE, 
        SEX = SEX, RACE = RACE, ETHNIC = ETHNIC, FOOD = ARM == 
            "FED", fun.summary = list(AGE = "first_", 
            SEX = "first_", RACE = "first_", ETHNIC = "first_", 
            FOOD = "first_")) %>% # assign units
    convert_units_from_list(.ul = list(AGE = "AGEU"))
    
    Labs = getCov(LB, ID = USUBJID, cov.val = LBSTRESN, 
        cov.col = LBTESTCD, Units = LBSTRESU, cov.keys = c("ID", 
            "EPOCH"), AST = AST, ALT = ALT, ALB = ALB, 
        BILI = BILI, HCT = HCT, fun.summary = list(AST = "first_", 
            ALT = "first_", ALB = "first_", BILI = "first_", 
            HCT = "first_"))
    list(Demog = Demog, Labs = Labs)
    
}

Process_CovT <- function() {
    CovT = getCovT(LB, ID = USUBJID, TIME = parsedate(LBDTC), 
        EVID = 2, covT.val = LBSTRESN, covT.col = LBTESTCD, 
        Units = LBSTRESU, RBC = RBC, CREAT = CREAT, 
        fun.summary = list(RBC = "first_", CREAT = "first_"))
    bind_rows(CovT)
    
}

# pre-merge hook function

pre.merge.hook = function() {
}


# post-merge hook function

post.merge.hook = function() {
}


## These functions will work with the merged
## dataset, passed in as .data.  Return the modified
## dataset.

post.transform = function(.data) {
    .data %>% post_transform_start("ELTM") %>% group_by(ID) %>% 
        transform(ELTM = elapsed.time(TIME)) %>% post_transform_end("ELTM") %>% 
        post_transform_start("TAFD") %>% time_after_first_dose(groups = "ID", 
        .name = "TAFD") %>% post_transform_end("TAFD") %>% 
        post_transform_start("NDOSE") %>% group_by(ID) %>% 
        transform(NDOSE = pmax(1, cumsum(EVID == 1))) %>% 
        post_transform_end("NDOSE") %>% post_transform_start("TAD") %>% 
        time_after_first_dose(groups = .(ID, NDOSE), 
            .name = "TAD") %>% post_transform_end("TAD") %>% 
        post_transform_start("EVDT") %>% transform(EVDT = format_date(TIME)) %>% 
        post_transform_end("EVDT") %>% post_transform_start("EVTM") %>% 
        transform(EVTM = format_time(TIME)) %>% post_transform_end("EVTM") %>% 
        post_transform_start("FDDTTM") %>% group_by(ID) %>% 
        time_of_first_dose("FDDTTM") %>% post_transform_end("FDDTTM") %>% 
        post_transform_start("FDDT") %>% transform(FDDT = format_date(FDDTTM, 
        "%Y%m%d")) %>% post_transform_end("FDDT") %>% 
        post_transform_start("FDTM") %>% transform(FDTM = format_time(FDDTTM, 
        "%H%M")) %>% post_transform_end("FDTM") %>% 
        post_transform_start("SUBJID") %>% transform(SUBJID = parse_usubjid(ID, 
        5)) %>% post_transform_end("SUBJID") %>% # assign units
    convert_units_from_list(.ul = list(ELTM = "h", 
        TAFD = "h", TAD = "h", AGE = "AGEU"))
    
}


post.filter = function(.data) {
    .data
}


apply.exclusions = function(.data) {
    .data
}
