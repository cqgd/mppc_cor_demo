library(data.table)

combine = function(rjmcmc_path_A, rjmcmc_path_B, rjmcmc_path_AB){
  rjmcmc_A = readRDS(rjmcmc_path_A)
  rjmcmc_B = readRDS(rjmcmc_path_B)
  
  rjmcmc_AB = new("R2BGLiMS_Results_Merged", 
                  mcmc.output = rbind(rjmcmc_A@mcmc.output, rjmcmc_B@mcmc.output),
                  n.iterations = rjmcmc_A@n.iterations + rjmcmc_A@n.iterations)
  
  saveRDS(rjmcmc_AB, rjmcmc_path_AB)
  
  merge_result = data.table(iterations_in_A=rjmcmc_A@n.iterations, models_in_A=nrow(rjmcmc_A@mcmc.output),
                                        iterations_in_B=rjmcmc_B@n.iterations, models_in_B=nrow(rjmcmc_B@mcmc.output),
                                        iterations_in_AB=rjmcmc_AB@n.iterations, models_in_AB=nrow(rjmcmc_AB@mcmc.output))
  
  return(merge_result)
}

merge_chains_by_dir = function(rjmcmc_dir_A, rjmcmc_dir_B, rjmcmc_dir_AB){
  setClass("R2BGLiMS_Results_Merged",   slots = list(mcmc.output = "data.frame", n.iterations="numeric"))
  
  files_A = grep("rjmcmc_[0-9]+.rds$",list.files(rjmcmc_dir_A),value=TRUE)
  files_B = grep("rjmcmc_[0-9]+.rds$",list.files(rjmcmc_dir_B),value=TRUE)
  
  if(!dir.exists(rjmcmc_dir_AB)){
    dir.create(rjmcmc_dir_AB)
    if(!dir.exists(rjmcmc_dir_AB)){
      stop(paste0("Output directory ",rjmcmc_dir_AB," cannot be found nor created."))
    }
  }
  
  L = paste0(rjmcmc_dir_AB, "/merge_chains_log.txt")
  write("MERGING CHAINS\n", file = L, append = FALSE, sep = "")
  
  peaky:::note(L,TRUE,"Merging chains for RJMCMC results in:\nA: ",
               rjmcmc_dir_A,"\nB: ",rjmcmc_dir_B)
  
  files_all = union(files_A,files_B)
  files_both = intersect(files_A,files_B)
  
  paths_A = paste0(rjmcmc_dir_A,"/",files_both)
  paths_B = paste0(rjmcmc_dir_B,"/",files_both)
  paths_AB = paste0(rjmcmc_dir_AB,"/",files_both)
  
  peaky:::note(L,FALSE,"Unique RJMCMC results across A and B: ",length(files_all),
               "\nRJMCMC results in both A and B: ", length(files_both))
  
  peaky:::note(L,TRUE,"Loading common RJMCMC results and pooling their models...")
  
  combine_table = rbindlist(mapply(combine, paths_A, paths_B, paths_AB, SIMPLIFY=FALSE))
  combine_table[,results_file:=files_both]
  
  file_table = data.table(results_file=files_all, in_A=files_all%in%files_A, in_B=files_all%in%files_B)
  
  file_combine_table = merge(file_table, combine_table, by="results_file", all.x = TRUE)
  
  table_path = paste0(rjmcmc_dir_AB,"/","merged_chains.csv")
  
  peaky:::note(L,TRUE,"Saving overview of merged chains:\n",table_path)
  fwrite(file_combine_table, table_path)

  peaky:::note(L,FALSE,"Done.")
  return(file_combine_table)
}

#Use full paths:
rjmcmc_dir_A = "combined/rjmcmc_A" 
rjmcmc_dir_B = "combined/rjmcmc_B"
rjmcmc_dir_AB = "combined/rjmcmc_AB"

merge_chains_by_dir(rjmcmc_dir_A, rjmcmc_dir_B, rjmcmc_dir_AB)

# 19-06-2018 01:14:56
# Merging chains for RJMCMC results in:
# A: combined/rjmcmc_A
# B: combined/rjmcmc_B
# Unique RJMCMC results across A and B: 10
# RJMCMC results in both A and B: 8
# 19-06-2018 01:14:56
# Loading common RJMCMC results and pooling their models...
# 19-06-2018 01:15:22
# Saving overview of merged chains:
#   combined/rjmcmc_AB/merged_chains.csv
# Done.
# results_file  in_A  in_B iterations_in_A models_in_A iterations_in_B models_in_B iterations_in_AB models_in_AB
# 1: rjmcmc_102201.rds FALSE  TRUE              NA          NA              NA          NA               NA           NA
# 2: rjmcmc_119435.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 3: rjmcmc_119522.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 4: rjmcmc_144385.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 5: rjmcmc_162008.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 6: rjmcmc_175487.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 7:  rjmcmc_53559.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 8:  rjmcmc_71638.rds  TRUE FALSE              NA          NA              NA          NA               NA           NA
# 9:  rjmcmc_95958.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
# 10:  rjmcmc_96180.rds  TRUE  TRUE           1e+06       10000           1e+06       10000            2e+06        20000
