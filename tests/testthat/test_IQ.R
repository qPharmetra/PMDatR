context("IQ")
# Run IQ
IQ.out = R.installation.qualification()
# test that IQ passed
IQ.passed = length(grep("PASSED",IQ.out$IQ.message))>0
expect_true(IQ.passed)
