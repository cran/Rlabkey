ifcookie <- function()
{
if(exists("labkey.sessionCookieName")) {Cvalue=1; Cname=labkey.sessionCookieName; 
Ccont=labkey.sessionCookieContents} else {Cvalue=0; Cname=NULL; Ccont=NULL}

return(list(Cvalue=Cvalue,Cname=Cname,Ccont=Ccont))
}




