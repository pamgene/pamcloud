library(tcltk)
library(RCurl)


PamCloud.getBinaryURL= function(url = "https://pamcloud.pamgene.com/jackrabbit/repository/default/PamCloud/BioNavigator/Resources/readme.txt",
                                credentials = login.dialog()){
  if(!is.null(credentials)){
    aFile = getBinaryURL(url, 
                         userpwd = paste(credentials$user,
                                              credentials$pw,
                                              sep = ":"),
                         ssl.verifypeer = FALSE)
  } else{
    aFile = getBinaryURL(url,
                         ssl.verifypeer = FALSE)
  }
return(aFile)
}

PamCloud.checkAccess = function(credentials = login.dialog()){
  aFile = PamCloud.getBinaryURL(credentials = credentials)
  return(substring(rawToChar(aFile),1,33) == "<PamCloud Bionavigator Resources>")
}

PamCloud.installPackage = function(packageUrl, tmpDir = "C:\\temp"){
  credentials = login.dialog()
  if(!PamCloud.checkAccess(credentials)){
      stop("PamCloud access denied or not available")
  }
  
  nPack = length(packageUrl)
  print(paste("Installing", nPack, "packages"))
  for (i in 1:nPack){
    aFile = PamCloud.getBinaryURL(packageUrl[i], credentials)
  
    if (!file.exists("C:\\temp")){
      tmpDir = getwd();
    }
    pckFile = file.path(tmpDir, basename(packageUrl[i]))     
    writeBin(aFile, pckFile)
    install.packages(pckFile, repos = NULL)
  }
}

login.dialog = function(){
  credentials = list(user = "PamCloud User Name", pw = "pass") 
  onok = function(){tkcget
                    tkdestroy(tt)
  }
  tt = tktoplevel()
  label.user = tklabel(tt, text = "User Name")
  userVal = tclVar(init = credentials$user)
  entry.user =  tkentry(tt, textvariable = userVal)
  label.pw = tklabel(tt, text = "Password")
  pwVal = tclVar(init = credentials$pw)
  entry.pw = tkentry(tt, textvariable = pwVal, show = "*")
  button.ok = tkbutton(tt, text = "OK", command =onok)
  tkpack(label.user)
  tkpack(entry.user)
  tkpack(label.pw)
  tkpack(entry.pw)
  tkpack(button.ok)
  tkfocus(tt)
  tkwait.window(tt)
  credentials$user = tclvalue(userVal)
  credentials$pw = tclvalue(pwVal)
  return(credentials)
}
 



