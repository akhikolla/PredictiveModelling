~/fuzzer_packages/fuzzedpackages/pedometrics/inst/testfiles/rowMinCpp/libFuzzer_rowMinCpp/rowMinCpp_timely_output


pkg.execlist <- list() 
pkg.unexlist <- list()
pkgs <- Sys.glob(file.path("~/fuzzer_packages/fuzzedpackages","*"))
for(pkg.i in pkgs){
  exelist <- list()
  unexelist <- list()
  testfiles <- file.path(pkg.i,"inst/testfiles")
  fun.count <- Sys.glob(file.path(testfiles,"*"))
  if(length(fun.count) > 0){
    for(fun.i in fun.count){
      #lib.dir <- file.path(fun.i, paste0("AFL_",basename(fun.i)))
      lib.dir <- file.path(fun.i, paste0("libFuzzer_",basename(fun.i)))    
      if(file.exists(lib.dir)){
      lib.dir.exec <- file.path(lib.dir,paste0(basename(fun.i),"_timely_output"))
      lib.files <- Sys.glob(file.path(lib.dir.exec,"*"))
      if(length(lib.files) > 0){                
        exelist<- c(exelist,fun.i)
      }else{
        unexelist<-c(unexelist,fun.i)
      }
      }
    }
  }
  if(length(exelist) == length(fun.count))
    pkg.execlist[[basename(pkg.i)]] <-exelist
  if(length(unexelist) > 0)
    pkg.unexlist[[basename(pkg.i)]] <-unexelist
}


print("Executed package:\n")
print(head(pkg.execlist))
print("Unexecuted package:\n")
print(head(pkg.unexlist))

r <- Sys.glob(file.path("/scratch/ak2296/RcppDeepState/fuzzer_packages/fuzzedpackages","*"))
length(r)
# 441

nt <- function(r){
  for(i in r){
    install.packages(i)
  }
}

future::plan("multiprocess", workers=24)
future.apply::future_lapply(r, FUN=nt)
































