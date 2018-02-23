require(h2o)        
h2oDataCave <- h2o.init(
  ip = "localhost", 
  port = 54321,
  nthreads = -1, 
  max_mem_size = "10g", 
  min_mem_size = "1g",
  strict_version_check = FALSE ## watch out!
)
#### System information
h2o.xgboost.available()
h2o.clusterStatus()
h2o.clusterInfo()
#### Shutdown
h2o.shutdown()
