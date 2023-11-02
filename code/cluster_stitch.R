cluster_stitch <- function(scenario, beta, x, y) {
  
  library(abind)
  
  WD <- '~/iliketurtles3/output/'
  
  # put together filepaths for first set of sims
  file1 <- paste(WD, scenario, 'C/beta', beta, '/', x, '_abundance_F.Rda', sep = '')
  file2 <- paste(WD, scenario, 'C/beta', beta, '/', x, '_abundance_M.Rda', sep = '')
  file3 <- paste(WD, scenario, 'C/beta', beta, '/', x, '_abundance_total.Rda', sep = '')
  file4 <- paste(WD, scenario, 'C/beta', beta, '/', x, '_mature_abundance.Rda', sep = '')
  file5 <- paste(WD, scenario, 'C/beta', beta, '/', x, '_N.Rda', sep = '')
  
  # load them as objects
  load(file1); abundanceF1       <- sims_abundance_F
  load(file2); abundanceM1       <- sims_abundance_M
  load(file3); abundance_total1  <- sims_abundance_total  
  load(file4); mature_abundance1 <- sims_mature_abundance
  load(file5); N1                <- sims_N
  
  # put together filepaths for first set of sims
  file6  <- paste(WD, scenario, 'C/beta', beta, '/', y, '_abundance_F.Rda', sep = '')
  file7  <- paste(WD, scenario, 'C/beta', beta, '/', y, '_abundance_M.Rda', sep = '')
  file8  <- paste(WD, scenario, 'C/beta', beta, '/', y, '_abundance_total.Rda', sep = '')
  file9  <- paste(WD, scenario, 'C/beta', beta, '/', y, '_mature_abundance.Rda', sep = '')
  file10 <- paste(WD, scenario, 'C/beta', beta, '/', y, '_N.Rda', sep = '')
  
  # load them as objects
  load(file6);  abundanceF2       <- sims_abundance_F
  load(file7);  abundanceM2       <- sims_abundance_M
  load(file8);  abundance_total2  <- sims_abundance_total  
  load(file9);  mature_abundance2 <- sims_mature_abundance
  load(file10); N2                <- sims_N
  
  # combine objects into new ones
  sims_abundance_F      <- abind(abundanceF1, abundanceF2, along = 2)
  sims_abundance_M      <- abind(abundanceM1, abundanceM2, along = 2)
  sims_abundance_total  <- abind(abundance_total1, abundance_total2, along = 2)
  sims_mature_abundance <- abind(mature_abundance1, mature_abundance2, along = 2)
  sims_N                <- abind(N1, N2, along = 4)
  
  # new number of sims
  z = x + y
  
  # create new filepaths
  file11  <- paste(WD, scenario, 'C/beta', beta, '/', z, '_abundance_F.Rda', sep = '')
  file12  <- paste(WD, scenario, 'C/beta', beta, '/', z, '_abundance_M.Rda', sep = '')
  file13  <- paste(WD, scenario, 'C/beta', beta, '/', z, '_abundance_total.Rda', sep = '')
  file14  <- paste(WD, scenario, 'C/beta', beta, '/', z, '_mature_abundance.Rda', sep = '')
  file15  <- paste(WD, scenario, 'C/beta', beta, '/', z, '_N.Rda', sep = '')   
  
  # save new objects to new filepaths
  save(sims_abundance_F, file = file11)
  save(sims_abundance_M, file = file12)
  save(sims_abundance_total, file = file13)
  save(sims_mature_abundance, file = file14)
  save(sims_N, file = file15)      
  
  # remove old files
  file.remove(file1)
  file.remove(file2)
  file.remove(file3)
  file.remove(file4)
  file.remove(file5)      
  file.remove(file6)
  file.remove(file7)
  file.remove(file8)
  file.remove(file9)
  file.remove(file10)    
  
}
