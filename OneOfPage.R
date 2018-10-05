URL = "https://forum.gamer.com.tw/C.php?page=5&bsn=17532&snA=19291&tnum=182&bPage=252" #-- encoding error page
URL = "https://forum.gamer.com.tw/C.php?page=1&bsn=17532&snA=134897&tnum=175&bPage=197" #-- Too many word
XC = read_html( curl(URL, handle = new_handle("useragent" = "Mozilla/5.0") ), encoding = "UTF-8") #-- Char 0xDD84 out of allowed range [9]
# XC<- iconv(temp, "ASCII", "utf8")

# read.html.con <- file(description = URL, encoding = "UTF-8", open = "rt")

XInPage = html_text(html_nodes(XC, xpath = "//p[@class='BH-pagebtnA']/a")) #-- 頁數
XInPage[length(XInPage)] #-- 頁數


XPostAuthor = html_text( html_nodes(XC, ".username") )
XPostUserID = html_text( html_nodes(XC, ".userid") )
#---以下一組的
XPostTitle = html_text( html_nodes(XC, ".c-post__header__title") ) #-- 只有一個
XPostTitle = rep( XPostTitle, times = length(XPostAuthor) )
#---以上一組的
XPostDate = html_attr( html_nodes(XC, xpath = "//div[@class='c-post__header__info']/a"), "data-mtime" )

XisTitle = c("1", rep("0", times = length(XPostAuthor)-1))
XURL = rep( URL, times = length(XPostAuthor) )

XCareer = html_attr( html_nodes(XC, xpath = "//div[@class='usericon usercareer']"), "title" )
XRace = html_attr( html_nodes(XC, xpath = "//div[@class='usericon userrace']"), "title" )

#---以下一組的
XUserLV = html_text( html_nodes(XC, ".userlevel"), trim = TRUE )
XUserLV = gsub("LV.\n", "", XUserLV)
#---以上一組的

XUserGP = html_attr( html_nodes(XC, ".usergp"), "title" )

#---以下一組的
XPostContent = html_text( html_nodes(XC, ".c-article__content") )
Xntag = gsub("<.,，*?>", "", XPostContent)  #-- 正規表示法移除tag
XPostContent = str_replace_all( Xntag, "\n", "" )  #-- 合併為一字串，並移除換行碼
XPostContent = str_replace_all( XPostContent, ",", "，" )  #-- 合併為一字串，並移除換行碼
#---以上一組的
Xnch = sapply(XPostContent, FUN=function(x)nchar(as.character(x)))
XPostGP = html_text( html_nodes(XC, xpath = "//span[@class='postgp']/span") )
XPostBP = html_text( html_nodes(XC, xpath = "//span[@class='postbp']/span") )

temp = data.frame( Title=XPostTitle, URL = XURL, Author=XPostAuthor, AuthorID=XPostUserID, PostDate = XPostDate, 
                   Career = XCareer, Race = XRace, UserLV = XUserLV, UserGP = XUserGP, PostContent = XPostContent, 
                   PostGP = XPostGP, PostBP = XPostBP, isTitle = XisTitle )

AllData <- temp

write.table(AllData, file = "TestData.csv", sep = ",", col.names = NA, append = FALSE, na = "NA")


YY = read.csv("TestData.csv", quote = "")
