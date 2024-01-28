#Adapted from Vikram Agarwal by Gracie Gordon

cpath <- grep('conda', .libPaths(), value=TRUE, ignore.case=TRUE)
.libPaths(cpath)

library(dplyr)

args <- commandArgs(trailingOnly = TRUE)


#exp=args[1]
cond=args[1]
thresh=args[2]
outfile=args[3]
avg_outfile=args[4]
#out=args[3]

nrm_reps=c()
all_reps=c()
##MAKE MASTER TABLE
replicates=((length(args)-4)/2)
#no idea why the previous code doesn't show n=10 bc when thresh is set to 4, changing this seems to help.
thresh = as.numeric(thresh)


for (i in 5:(length(args)-replicates)){
   file=args[i]
   rep=args[i+replicates]
   tab=as.data.frame(read.table(file,header=TRUE))

   filter_tab=tab[tab$n_obs_bc >= thresh,]
   #unfortunately this results in no_BC appearing in the final results file, thus this line to filter out those that has no assigned cres
   filter_tab = filter_tab[filter_tab$name != "no_BC",]

   n_inserts=(dim(filter_tab)[1])

   cond_col=as.data.frame(rep(cond,n_inserts))
   rep_col=as.data.frame(rep(rep,n_inserts))
   colnames(cond_col)='condition'
   colnames(rep_col)='replicate'

   pref=as.data.frame(cbind(cond_col,rep_col))
   lab_tab=as.data.frame(cbind(pref,filter_tab))

   ##NORMALISZE PER REPLCATE TO COMBINE IN NEXT FUNCTION
   nrm_reps1=lab_tab
   nrm_reps1$ratio=(nrm_reps1$ratio)/(median(nrm_reps1$ratio))
   nrm_reps1$log2=round(log2(nrm_reps1$ratio),8)
   nrm_reps=rbind(nrm_reps,nrm_reps1)
   all_reps=rbind(all_reps,lab_tab)
}

write.table(all_reps,file=outfile,quote=FALSE,sep='\t',row.names=FALSE,col.names=TRUE )


##MAKE AVERAGED ACROSS REPLICATES

all_avg <- all_reps %>% group_by(condition) %>% summarize(
                    mean_ratio=mean(ratio),
                    mean_log2=log2(mean(ratio)),
                    mean_n_obs_bc=mean(n_obs_bc)
                  )


write.table(all_avg,file=avg_outfile,quote=FALSE,sep='\t',row.names=FALSE,col.names=TRUE )

print('done')
