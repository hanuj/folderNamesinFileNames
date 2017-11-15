#### R function to prefix names of subfolders within a parent folder to the names of files within the subfolders ####
#### can be used for specifc types of files such as ".pdf", ".xlsx" etc ####


folder_names_in_filenames<-function(parent_folder,filetype) {

  dirs<-dir(parent_folder,full.names=T,include.dirs=F)

  files<-list.files(parent_folder,pattern=filetype,recursive=T,full.names=F)

  new_fileNames<-as.vector(length(files))

      for (i in 1:length(files)) {
  
                                  locs<-regexpr(pattern="/",files[i],fixed=T)[1]

                                  b<-substr(files[i],1,locs-1)
  
                                  a<-gregexpr(pattern="/",files[i],fixed=T)
  
                                  a<-unlist(a)
  
                                  a<-max(a)
  
                                  c<-substr(files[i],a+1,1000)
  
                                  d<-paste0(b,"_",c)
  
                                  new_fileNames[i]<-gsub(c,d,files[i],fixed=T)
                                 }

  file.rename(paste0(parent_folder,files),
                paste0(parent_folder,new_fileNames))
}
