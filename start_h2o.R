#start H2O locally on EC2 server.
library(h2o)

remoteH2O <- h2o.init(ip='localhost', startH2O=TRUE, port = 54321, nthreads = -1, max_mem_size = '6g') #  Connection successful!