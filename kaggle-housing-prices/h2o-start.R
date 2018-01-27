require(h2o)        ## machine learning algorithmes
h2oDataCave <- h2o.init(
  ip = "localhost", 
  port = 54321,
  nthreads = -1, 
  max_mem_size = "6g", 
  min_mem_size = "2g",
  strict_version_check = FALSE ## watch out!
)
