# stitch SAD runs together on the cluster

SAD_cluster_stitch <- function(burn_ins, num_sims, betas, temp_stochasticities) {
  
  for (bi in 1:length(burn_ins)) {
    
    for (t in 1:length(temp_stochasticities)) {
      
      for (ns in 1:length(num_sims)) {
        
        # initialize results dataframe
        SAD <- data.frame(Model = NULL,
                            Beta = NULL,
                            Year = NULL,
                            Sex = NULL,
                            Age = NULL,
                            Proportion = NULL, 
                            PSR = NULL,
                            OSR = NULL
        )
        
        for (b in 1:length(betas)) {
          
          TS <- ifelse(temp_stochasticities[t] == TRUE, 'TS_', '')
          
          # P_base <- paste('../output/SAD_n', num_sims[ns], '_b', 
          #                 burn_ins[bi], '_P_base_beta', betas[b], '.Rdata', sep = '')
          P_base <- paste('../output/SAD_deterministic_', TS, 'b', 
                          burn_ins[bi], '_P_base_beta', betas[b], '_n', nsims, 
                          '.Rdata', sep = '')
          
          load(P_base) 
          
          SAD_P <- SADdf        
          
          GM_base <- paste('../output/SAD_deterministic_', TS, 'b', 
                           burn_ins[bi], '_GM_base_beta', betas[b], '.Rdata', 
                           sep = '')
          
          load(GM_base) 
          
          SAD_GM <- SADdf
          
          SAD <- rbind(SAD, SAD_P, SAD_GM)
          
          file.remove(P_base)
          file.remove(GM_base)
          
        }
        
        TS <- ifelse(temp_stochasticities[t] == TRUE, 'TS_', '')
        
        SADdf <- SAD
        
        save(SADdf,
             file = paste('../output/SAD_deterministic_', TS, 'b', 
                          burn_ins[bi], '.Rdata', sep = ''))
        
      }
      
    }
    
  }
  
}
