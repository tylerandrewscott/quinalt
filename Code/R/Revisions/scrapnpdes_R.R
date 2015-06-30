
library(devtools)
devtools::install_github(repo = 'rvest',username = 'hadley')
c('rvest','dplyr','pipeR', 'knitr') -> packages 
lapply(packages, library, character.only = T) 

'http://www.deq.state.or.us/wq/sisdata/facilitycriteria.asp' -> url
permit_search <- html("http://www.deq.state.or.us/wq/sisdata/facilitycriteria.asp")

permit_search_session = rvest::html_session(url)
search_form <- html_form(permit_search_session)[[1]]
query <- set_values(search_form,PermitType = "NPDES",PermitStatus='Active')


results = submit_form(permit_search_session, query,submit = 'Submit')


# Attempt to crawl Columbia Lionshare for jobs
session <- html_session(url)
form <- html_form(session)[[1]]
form <- set_values(form, PermitType = "General Permits - NPDES and WPCF")
session2 <- submit_form(session, form)

form2 <- html_form(session2)[[1]]
form2 <- set_values(form2)

follow_link(session2,'ShowAllRecs()')



session3 <- submit_form(session2, form2)

session3 = follow_link(session2,'AllRecs()')


read_html(session2)

form2 <- set_values(form2, 'AllRecs': ')

rvest::s


html_form(session2)[[1]]


session = html_session('http://www.deq.state.or.us/wq/sisdata/facilitylist.asp')




#Below code commented out in Markdown

#pw <- .rs.askForPassword("Password?")
#form <- set_values(form, password = pw)
#rm(pw)
#session2 <- submit_form(session, form)
#session2 <- follow_link(session2, "Job")
#form2 <- html_form(session2)[[1]]
#form2 <- set_values(form2, PositionTypes = 7, Keyword = "Data")
#session3 <- submit_form(session2, form2)




'#team' -> css_page
url %>>%
  html

%>>%
  html_nodes(css_page)

%>>%
  html_table(header = F) %>>%
  data.frame() %>>%
  tbl_df() -> total_table


total_table %>>%
  filter(X.1 == 'Rk') %>>% as.character -> names
'Rk' %>>% grep(x = total_table$X.1) -> row_of_header #find where rank is
names %>>% tolower -> names(total_table)
names(total_table) %>>% (gsub('\\%|/','\\.',.)) -> names(total_table)
(row_of_header + 1) %>>% (total_table[.:nrow(total_table),]) -> total_table #skip that row and go to the end row and go to the end
total_table %>>% head

f1 <- set_values(f0, PermitType = "NPDES",PermitStatus='Active')

search_form
