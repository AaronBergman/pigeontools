################### ?? ###################  

named2tib=function(nv){
  names=names(nv)
  vals=nv[1:length(nv)]
  tibble(names=names,values=vals)
} #??

############### Functions ###############

ln=function(x){log(x,2.71828)}

list_non_functions=function(g0=NA){ 
  if (is.na(g0)){g=globalenv()}
  function_objs=as.vector(lsf.str(g))
  ls(envir=g)[!ls(envir=g)%in%function_objs] 
}

pkg=function(pvec){for(p in pvec){suppressWarnings(suppressPackageStartupMessages(eval(call("library",p)))) }}

gt=function(x,envir= globalenv()){get(x,envir)}

cl=function(){eval(call("write_clip",c("rm(list=ls(all.names=T))\n")))}

dat=function(){
  paste0(str_to_lower(substr(month.name[month(Sys.Date())],1,3)),
         day(Sys.Date()
         ))}

daysec=function(){
  3600*hour(Sys.time())+60*minute(Sys.time())+second(Sys.time())
}

datsec=function(){
  paste0(dat(),daysec())
}

obj_classes=function(){
  nfa=sapply(as.list(list_non_functions()),get)%>%sapply(attributes)
  lnf=list_non_functions()[!str_detect(list_non_functions(),"nfa")]
  nfar=vector()
  for (i in 1:length(nfa)){
    nfar[i]=nfa[[i]]$class%>%paste0(collapse=' ')
    if(i==length(nfa)){
      nfClass=tibble(name=lnf,class=nfar)%>%as.data.table()
      rm(nfa,lnf,nfar)
    }
  }
  nfClass
}

lis=function(g=globalenv()){
  function_objs=as.vector(lsf.str(g))
  ls(envir=g)[!ls(envir=g)%in%function_objs]
}

name=function(thing){as.character(substitute(thing))}

do=function(str){eval(parse(text=str))}

tt=function(df){
  as_tibble(df)%>%t()%>%as_tibble()
}
dfNames=function(){list_non_functions()[list_non_functions()%>%as.list()%>%sapply(get)%>%sapply(is.data.frame)]}

rnk=function(x){rank(-x,na.last = "keep")}

read=function(str){
  helper=function(str){suppressMessages(eval(call("fread",paste0(str,'.csv',sep=''))))}
  as_tibble(helper(str))}

write=function(df=NULL){
  name_of_df=substitute(df)
  local_copy=get(as.character(name_of_df),envir = globalenv())
  if(is.data.frame(df)) {write_csv(local_copy,paste(name_of_df,'.csv',sep=''))
  } else{write_file(local_copy,paste(name_of_df,'.txt',sep=''))}}

v=function(x=.Last.value){view(get(as.character(substitute(x)),envir = globalenv()),title=" ")}

toSheets=function(x){
  eval(call('BROWSE',"https://docs.google.com/spreadsheets/u/0/create"))
  if (!is.na(x)){eval(call('write_clip',x))}
}

################### Scraping ################### 

slurp_html=function(url){
  fn=paste0(str_replace_all(url,"[w{3}\\.\\/]",''),".txt")
  HTML(suppressMessages(download.file(url,fn)))
  paste(read_lines(fn),collapse=" ")
} #depracated?

slurp=function(url){content(GET(url),"text")}

slurp0=function(url){
  temphtml=content(GET(url),"text")
  write_file(temphtml,'temphtml.html')
  BROWSE('file:///Users/aaronbergman/R/temphtml.html')
}

slurp1=function(url,name='NA'){
  if (name=="NA"){
    name=url
  }
  temphtml=content(GET(url),"text")
  write_file(temphtml,paste0(name,'.html'))
  BROWSE(paste0('file:///Users/aaronbergman/R/',name,'.html'))
}

render=function(html_str){
  write_file(html_str,'temphtml.html')
  BROWSE('file:///Users/aaronbergman/R/temphtml.html')
}
csv2html=function(path,clip=T,return=T){
  # write_file(as.vector(as.matrix(df)),'templines.txt')
  f=readLines(path)
  t=list()
  for(i in 1:length(f)){
    t[i]=paste0("<tr>",paste0("<td>",unlist(strsplit(f[i],split = ",")),"</td>",collapse= ""),"/<tr>")
  }
  t[1] = paste0("<table>",t[1])
  t[length(t)] = paste0(t[length(t)],"</table>")
  out=as.character(paste0(t,collapse='\n'))
  if(clip==T){write_clip(out)}
  if(return==T){out}
}


################### GGplot ###################  
center_title=function(plot){plot+theme(plot.title=element_text(hjust=0.5))}

ggpoint=function(g,my_aes){center_title(ggplot(g)+geom_point(my_aes)+theme_minimal())}

################### Regression ###################  

reg_df=function(lm,thin=T){sub=data.frame(summary(lm)$coefficients)%>%
  mutate(variable=rownames(.))%>%as_tibble%>%
  rename(estimate=Estimate,st_error=Std..Error,t_val=t.value,`p(>|t|)`=`Pr...t..`)%>%
  mutate(across(is.numeric,~signif(.x,3)))%>%
  mutate(data=as.character(summary(lm)$call)[3],
         model=name(summary(lm)$call)[1],
         r2=signif(summary(lm)$r.squared[1],3),
         regression=as.character(summary(lm)$call)[2],
         `p(>|t|)`=if_else(`p(>|t|)`<(10^-40),0,`p(>|t|)`),
         significance=case_when(`p(>|t|)`>=.05~"p>.05",`p(>|t|)`<.001~"p<.001",`p(>|t|)`>=.01&`p(>|t|)`<.05~'.01<p<.05',`p(>|t|)`>=.001&`p(>|t|)`<.01~".001<p<.01"),
         estimate=signif(estimate,3),t_val=round(t_val,3),r2=round(r2,3))%>%
  select(c(6,9,5,1,10,3,4,8))
if (thin==T){select(sub,-c(1,7))} 
else{sub}  }

OLSeq=function(lm){
  coefs_tib=named2tib(lm[[1]])
  terms=coefs_tib%>%mutate(str=paste0(signif(values,5),names))%>%
    mutate(str=str_replace_all(str,"\\(Intercept\\)",""))%>%
    .$str
  rhs=paste0(terms,collapse=" + ")
  outcomeVar=coefs_tib$call%>%as.character()%>%.[2]%>%str_split(' ~')%>%unlist%>%.[1]
  paste0(outcomeVar,' = ',rhs)
}

regDF=function(lm){
  sub=reg_df(lm)
  estimates=list(sub$estimate)#,collapse=" ")
  tvals=list(sub$t_val)#),collapse=" ")
  eq=OLSeq(lm)
  sub%>%select(-c(2:5))%>%.[1,]%>%
    mutate(
      output=eq,
      abstMin=min(abs(unlist(tvals))),
      abs_tMax=max(abs(unlist(tvals))),
      estimates=estimates,
      tvals=tvals)%>%
    mutate(output=str_replace(output,'NA',
                              str_extract(regression,'\\w+')))
}



######## subgroup #########  

hydrolyze=function(df){
  for (c in colnames(df)){
    assign(c,
           as.vector(as.matrix(df[,c])),
           envir = globalenv())
  }} #goodish

regstr=function(df,y_max_ind=1,usual=T){ #seems to work but not customizable! ys on left, xs on right
  x_ind=(y_max_ind+1):ncol(df)
  base2=c("")
  y_vec=colnames(df[,1:y_max_ind])
  x_n=df[,x_ind]
  for (y in y_vec){
    base=c("")
    for (i in 1:length(x_n)){
      v=combn(colnames(x_n),m=i)%>%t%>%as_tibble%>%unite('x',1:i,sep=" + ")%>%.$x
      base=c(base,v)  }  #if(i==length(x_n)){base=base[-1]}
    reg_set=paste0(y," ~ ",base)
    base2=c(base2,reg_set)}
  basic=base2[base2!="" & !str_ends(base2,"~ ")]
  if(usual==T){paste0("reg_df(lm(",basic,"))")}
  else {basic}
}

vanillaRegs=function(df,ycount=1){
  x_ind=(ycount+1):ncol(df)#-ycount)
  base2=c("")
  y_vec=colnames(df[,1:ycount])
  x_n=df[,x_ind]
  for (y in y_vec){
    
    base=c("")
    for (i in 1:length(x_n)){
      v=combn(colnames(x_n),m=i)%>%t%>%as_tibble%>%unite('x',1:i,sep=" + ")%>%.$x
      base=c(base,v)  }  #if(i==length(x_n)){base=base[-1]}
    
    reg_set=paste0(y," ~ ",base)
    base2=c(base2,reg_set)
  }
  basic=base2[base2!="" & !str_ends(base2,"~ ")]
  paste0("reg_df(lm(",basic,"))")
}

mashAvocado=function(regs_vec){
  base=do(regs_vec[1])
  for (r in regs_vec[2:length(regs_vec)]){
    base=bind_rows(base,do(r))
  }
  base
}

makeSimpleGuac=function(df,ycount=1,removeintercept=T){
  sub=mashAvocado(vanillaRegs(df,ycount=ycount))
  if(removeintercept==T){filter(sub,variable!="(Intercept)")}
  # not ready yet
  # makeSpicyGuac=function(df,ymax=1){base=makeSimpleGuac(df) if (ymax>=2){ for (i in 2:ymax){base=bind_rows(base,base=makeSimpleGuac(df,i)) } }  base}
  else{sub}
}

################### More generics ###################  

sumstats=function(x){
  sub=st(x,add.median=T,out="csv",digits=1,fixed.digits=F)%>%as_tibble()
  colnames(sub)=c('var','n','mean','stdev','min','pctl25','median','pctl75','max')
  sub}

anything=function(){write_clip('[\\s\\w\\-\\n\\t\\d[:punct:]]+')}

peak=function(path='.'){
  sub=list.files(path)%>%file.info()
  sub%>%as_tibble%>%mutate(name=rownames(sub),
                           kb=round(size/(10^3),1),
                           .before=1)%>%
    rename(edited=mtime,accessed=ctime)%>%
    select(c(name,edited,kb,isdir,accessed))%>%
    arrange(-as.numeric(edited))%>%
    mutate(ext=str_extract(name,"\\.[A-Za-z]{1,5}"),.before=1)%>%
    mutate(ext=str_remove(ext,'\\.'))
}

newRmd=function(start=''){
  filename=paste0(start,'.Rmd')# filename=paste0(start,'-',datsec(),'.Rmd')
  file.create(filename)
  file.edit(filename)
}

store=function(base='im'){
  # imageName=paste0(start,'-',datsec(),'.Rdata')
  imageName=paste0(base,'.Rdata')#'-',datsec(),'.Rdata')
  save.image(paste0(base,'.Rdata'))
  #assign('recentStore',imageName,envir = globalenv())
}



colclean=function(tib){
  tib%>%
    rename_all(~str_replace_all(., "[[:punct:] ]+", "_")) %>%
    rename_all(~str_replace_all(., "_{2,}", "_")) %>%
    rename_all(tolower)
}


pkg_all=function(package_list=c('codetools','forcats','foreign','plyr','ggplot2','ggtext','grid','gridtext','gtable','highr','isoband','jpeg','jsonlite','KernSmooth','knitr','labeling','lattice','magrittr','markdown','MASS','Matrix','selectr','stringi','purrr','rmarkdown','stringr','dplyr','dtplyr','haven','hms','vtable','clipr','lubridate','tidyverse','parsedate','data.table','httr','rvest','XML','xml2','htmltools','devtools','roxygen2','functionsthatshouldexist','quanteda','quanteda.textmodels','quanteda.textstats','quanteda.textplots','readtext','tuneR','cowplot','ggpubr','credentials','minqa','stringi','RCurl','googledrive','fixest','dreamerr','tokenizers','xtable','webshot2','processx','glmnet','Rcpp','xopen','clipr','reticulate','plm','lmtest')){
  pkg(package_list)
}
