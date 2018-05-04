MPPC_cor = function(result_A, result_B){
  cor(result_A$rjmcmc_pos, result_B$rjmcmc_pos)
}

MPPC_cor_by_dir = function(baits_rjmcmc_dir_A, baits_rjmcmc_dir_B, baits_dir, cor_threshold=.75){
  files_A = grep("bait_rjmcmc_[0-9]+.rds$",list.files(baits_rjmcmc_dir_A),value=TRUE)
  files_B = grep("bait_rjmcmc_[0-9]+.rds$",list.files(baits_rjmcmc_dir_B),value=TRUE)
  
  if (!dir.exists(baits_dir)) {
    stop("Baits directory not found.")
  }
  
  L = paste0(baits_dir, "/MPPCcor_log.txt")
  
  peaky:::note(L,TRUE,"Calculating MPPC cor for interpreted peaky results in:\nA: ",
               baits_rjmcmc_dir_A,"\nB: ",baits_rjmcmc_dir_B)
               
  files_all = union(files_A,files_B)
  files_both = intersect(files_A,files_B)
  
  paths_A = paste0(baits_rjmcmc_dir_A,"/",files_both)
  paths_B = paste0(baits_rjmcmc_dir_B,"/",files_both)
  peaky:::note(L,FALSE,"Unique bait results across A and B: ",length(files_all),
               "\nBait results in both A and B: ", length(files_both))
  
  peaky:::note(L,TRUE,"Loading common results and calculating MPPC...")
  
  results_A = lapply(paths_A,readRDS)
  results_B = lapply(paths_B,readRDS)
  
  cors = mapply(MPPC_cor, results_A, results_B)
  
  cor_table = data.table(results_file=files_both, MPPC_cor=cors)
  file_table = data.table(results_file=files_all, in_A=files_all%in%files_A, in_B=files_all%in%files_B,
                          bait_path=paste0(baits_dir,"/",gsub("rjmcmc_","",files_all)))
  
  file_cor_table = merge(file_table, cor_table, by="results_file", all.x = TRUE)
  file_cor_table[,redo:=!is.na(MPPC_cor) & MPPC_cor<cor_threshold]
  
  table_path = paste0(baits_dir,"/","MPPCcor.csv")
  baitlist_path = paste0(baits_dir,"/","baitlist_MPPCcor_sub_",cor_threshold,".txt")
  
  peaky:::note(L,TRUE,"Saving MPPC correlation overview:\n",table_path,
               "\nSaving baitlist for baits with MPPC correlation below ",cor_threshold,":\n",baitlist_path)
  fwrite(file_cor_table, table_path)
  write(file_cor_table[redo==TRUE,bait_path], baitlist_path)
  peaky:::note(L,FALSE,"Done.")
  
  return(file_cor_table)
}

#use full paths
baits_rjmcmc_dir_A = "combined/baits_rjmcmc_A/" 
baits_rjmcmc_dir_B = "combined/baits_rjmcmc_B/"
baits_dir = "combined/baits"
cor_threshold=.75

MPPC_cor_by_dir(baits_rjmcmc_dir_A, baits_rjmcmc_dir_B, baits_dir, 0.75)

# 04-05-2018 14:44:25
# Calculating MPPC cor for interpreted peaky results in:
#   A: combined/baits_rjmcmc_A/
#   B: combined/baits_rjmcmc_B/
#   Unique bait results across A and B: 10
# Bait results in both A and B: 8
# 04-05-2018 14:44:25
# Loading common results and calculating MPPC...
# 04-05-2018 14:44:25
# Saving MPPC correlation overview:
#   combined/baits/MPPCcor.csv
# Saving baitlist for baits with MPPC correlation below 0.75:
#   combined/baits/baitlist_MPPCcor_sub_0.75.txt
# Done.
# results_file  in_A  in_B                      bait_path    MPPC_cor  redo
# 1: bait_rjmcmc_102201.rds FALSE  TRUE combined/baits/bait_102201.rds          NA FALSE
# 2: bait_rjmcmc_119435.rds  TRUE  TRUE combined/baits/bait_119435.rds 0.100062417  TRUE
# 3: bait_rjmcmc_119522.rds  TRUE  TRUE combined/baits/bait_119522.rds 0.692424957  TRUE
# 4: bait_rjmcmc_144385.rds  TRUE  TRUE combined/baits/bait_144385.rds 0.877369736 FALSE
# 5: bait_rjmcmc_162008.rds  TRUE  TRUE combined/baits/bait_162008.rds 0.107567732  TRUE
# 6: bait_rjmcmc_175487.rds  TRUE  TRUE combined/baits/bait_175487.rds 0.630065561  TRUE
# 7:  bait_rjmcmc_53559.rds  TRUE  TRUE  combined/baits/bait_53559.rds 0.036390080  TRUE
# 8:  bait_rjmcmc_71638.rds  TRUE FALSE  combined/baits/bait_71638.rds          NA FALSE
# 9:  bait_rjmcmc_95958.rds  TRUE  TRUE  combined/baits/bait_95958.rds 0.039166755  TRUE
# 10:  bait_rjmcmc_96180.rds  TRUE  TRUE  combined/baits/bait_96180.rds 0.009392223  TRUE
