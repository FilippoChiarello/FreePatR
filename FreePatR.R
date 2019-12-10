library(rvest)

countpaper <- function(query,year=FALSE){
	
query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)	
query <- gsub(" AND ", dQuote(")+AND+("), query,fixed=TRUE)
query <- gsub(" OR ", dQuote("+OR+"), query,fixed=TRUE)
query <- gsub(" ","+", query)
query <- gsub("_","+", query)
query <- dQuote(query)
query <- gsub("^","(", query)
query <- gsub("$",")", query)
query <- gsub("“","%E2%80%9C", query)
query <- gsub("”","%E2%80%9C", query)


link <- paste("http://www.freepatentsonline.com/result.html?p=1&edit_alert=&srch=xprtsrch&query_txt=",query,"&uspat=on&usapp=on&eupat=on&date_range=all&stemming=on&sort=relevance&search=Search",sep="")


if(year!=FALSE){
link <- paste("http://www.freepatentsonline.com/result.html?p=1&edit_alert=&srch=xprtsrch&query_txt=APD%2F",year,"+",query,"&uspat=on&usapp=on&eupat=on&date_range=all&stemming=on&sort=relevance&search=Search",sep="")}


page <- read_html(link)

n_patents <- html_nodes(page,"#results .well-small td:nth-child(1)")

n_patents <- gsub(".*of ","",n_patents)

n_patents <- as.numeric(gsub(" .*</td>","",n_patents))

if(length(n_patents)==0){n_patents <- 0}


return (n_patents)}

