keisoku <- function(code){
  require(proftools)
  Rprof(tmp <- tempfile())
  code
  Rprof()
  res <- flatProfile(readProfileData(tmp))
  unlink(tmp)
  return(res)
  }