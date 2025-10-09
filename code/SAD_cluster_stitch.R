# stitch SAD runs together on the cluster

SAD_cluster_stitch <- function(burn_ins, num_sims, betas) {
  
  for (bi in 1:length(burn_ins)) {
    
    for (ns in 1:length(num_sims)) {
      
      # initialize results dataframe
      SADdf <- data.frame(Model = NULL, 
                          Beta = NULL,
                          Sex = NULL,
                          Age = NULL,
                          Abundance = NULL, 
                          Proportion = NULL
      )
      
      for (b in 1:length(betas)) {
        
        P_base <- paste('../output/SAD_n', num_sims[ns], '_b', 
                        burn_ins[bi], '_P_base_beta', betas[b], '.Rdata', sep = '')
        
        load(P_base) 
        
        SAD_P <- SAD
        
        GM_base <- paste('../output/SAD_n', num_sims[ns], '_b', 
                         burn_ins[bi], '_GM_base_beta', betas[b], '.Rdata', sep = '')
        
        load(GM_base) 
        
        SAD_GM <- SAD
        
        SADdf <- rbind(SADdf, SAD_P, SAD_GM)
        
        file.remove(P_base)
        file.remove(GM_base)
        
      }
      
      save(SADdf, 
           file = paste('../output/SAD_n', num_sims[ns], 
                        '_b', burn_ins[bi], '.Rdata', sep = ''))    
      
    }
    
  }
  
}
