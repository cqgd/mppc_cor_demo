#This serves to check that RJMCMC residuals (not the Negative Binomial regression residuals) are indeed N(0,1)-distributed in results generated with a specific value of omega.

library(peaky)
library(data.table)
library(ggplot2)
library(cowplot)

window_stats = function(w,B){
  B[w['w_start']:w['w_end'], 
    .(w_mean = mean(rjmcmc_residual), w_var=var(rjmcmc_residual))]
}


slide_window = function(B, width=250, omega_power=NA){
  setorder(B, dist)
  
  ######################
  #If predicted_present is used instead of predicted, omega needs to be re-supplied:
  #Quickly patch predicted_present in case NaNs got propagated from loci never sampled with a non-zero beta.
  #Very similar to interpret_peaky().
  #This shouldn't be an issue for many iterations, but the demo data is processed with few.
  #####################
  # if(is.numeric(omega_power)){
  #   omega = 10^omega_power
  #   dist_exp = function(offset, strength, omega) {
  #     strength * exp(-(abs(offset * omega)))
  #   }
  #   distance_matrix = mapply(function(position, strength, genome, omega) {
  #     dist_exp(genome - position, strength = strength, omega = omega)
  #   }, position = B$dist, strength = 1, MoreArgs = list(genome = B$dist, omega = omega), SIMPLIFY = TRUE)
  #   B[is.nan(beta_mean_present),beta_mean_present:=0] 
  #   B[,predicted_present:=distance_matrix  %*%  beta_mean_present]
  # }
  #######################
  
  B[,rjmcmc_residual:=residual-predicted]
  
  windows = data.table(baitID=unique(B$baitID), w_start = seq(1,max(1,nrow(B)-(width-1)),by=width))
  windows[,w_end:=w_start+width]
  stats = rbindlist(apply(windows,1,window_stats,B=B))
  windows = cbind(windows, stats)
  return(windows)
}  

window_residuals_fs = function(baits_rjmcmc_dir, window_dir, width=250, omega_power=NA){
  L = paste0(window_dir, "/log_window_qc.txt")
  if (!dir.exists(window_dir)) {
    dir.create(window_dir, recursive = TRUE)
  }
  write("PEAKY Window QC\n", file = L, append = FALSE, sep = "")
  
  result_files = list.files(baits_rjmcmc_dir,pattern="^bait_rjmcmc_.*.rds$")
  result_paths = paste0(baits_rjmcmc_dir,"/",result_files)
  
  peaky:::note(L,T,"Found ",length(result_paths)," baits in ", baits_rjmcmc_dir,"\n")
  
  window_files = gsub("bait_rjmcmc_","window_",result_files)
  window_paths = paste0(window_dir,"/",window_files)
  
  #This could be an mapply if RAM isn't problematic, along the lines of:
  #window_stats_all = mapply(slide_window, result_paths, window_paths, MoreArgs = list(omega_power=omega_power))
  #Could also be parallelized if an index of inputs is created first.
  
  for(b in seq_along(result_paths)){
    peaky:::note(L,T,"Loading result [",b,"/",length(result_paths),"]: ",result_paths[b])
    B = readRDS(result_paths[b])
    
    peaky:::note(L,F,"Calculating mean and variance of RJMCMC residuals across windows containing ",width," interactions....")
    window_result = slide_window(B, width=width, omega_power=omega_power)
    
    peaky:::note(L,T,"Writing these statistics to ",window_paths[b],"...\n")
    saveRDS(window_result,window_paths[b])
  }
  
  peaky:::note(L,T,"Done.")
}

plot_window_stats = function(window_dir){
  window_files = list.files(window_dir,pattern="^window.*rds$")
  window_paths = paste0(window_dir,"/",window_files)
  window_stats_all = rbindlist(sapply(window_paths,readRDS,simplify=FALSE))
  window_stats_all = melt(window_stats_all,measure.vars = c("w_mean","w_var"))
  nice_names = c("w_mean"="Mean across window",
                "w_var"="Variance across window")
  window_stats_all[,variable:=nice_names[variable]]
  plot = ggplot(window_stats_all) + 
          geom_histogram(aes(x=value,fill=variable),bins=30) + 
          facet_wrap(~variable,scales="free_x") + 
          ggtitle("RJMCMC residual statistics for all baits") + 
          ylab("Number of windows") + xlab("Value") +
          theme(legend.position = "none")
  
  print(plot)
  plot_path = paste0(window_dir,"/RJMCMC_residual_stats.pdf")
  cowplot::ggsave(plot_path,plot,width=8,height=4)
  print(plot_path)
  return(plot_path)
}


baits_rjmcmc_dir = "combined/baits_rjmcmc_A" #with output of interpret_peaky_fs()
window_dir = "combined/windows_A"

window_residuals_fs(baits_rjmcmc_dir, window_dir)
plot_window_stats(window_dir)

# 09-11-2018 18:15:01
# Found 9 baits in combined/baits_rjmcmc_A
# 
# 09-11-2018 18:15:01
# Loading result [1/9]: combined/baits_rjmcmc_A/bait_rjmcmc_119435.rds
# Calculating mean and variance of RJMCMC residuals across windows containing 250 interactions....
# 09-11-2018 18:15:01
# Writing these statistics to combined/windows_A/window_119435.rds...
# 
# 09-11-2018 18:15:01
# Loading result [2/9]: combined/baits_rjmcmc_A/bait_rjmcmc_119522.rds
# Calculating mean and variance of RJMCMC residuals across windows containing 250 interactions....
# 09-11-2018 18:15:01
# Writing these statistics to combined/windows_A/window_119522.rds...
#
# Etc.

#The plot should show a mean of 0 and a variance of 1 for most windows
