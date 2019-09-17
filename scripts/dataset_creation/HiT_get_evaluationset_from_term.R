

#install.packages("zeallot")
library(rvest)
library(rjson)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)

##functions
}

check_result <- function(pos,id){
	if(length(pos)!=length(id)){
		quit(status="stop. The number of %s and %s do not correspond. The regex must be wrong.", pos, id)
	}
}

pos_from_one_span_node = function(node){
	id_and_pos <- node %>% html_text() 
	pospart <- unlist(strsplit(id_and_pos, " "))[2]
	posregex <- regexpr("(adj|adv|conj|int|n|prep|phr|v|vi|vt|v\\.\\s*impers|v\\.\\s*pass|v\\.\\s*refl)\\.", pospart)
	pos <- regmatches(pospart, posregex)
	return(pos)
    }

id_from_one_span_node = function(node){
	id_and_pos <- node %>% html_text()
	id <- unlist(strsplit(id_and_pos, " "))[1]
	if(grepl("fulldate", id)){
		id <- NULL
	}
	return(id)
    }

textfields_from_large_span_node = function(node){
	definition <- node %>% vapply(FUN.VALUE="character", function(n) html_nodes(n, "a") %>% html_text() %>% paste0(collapse="/"))
	definitionnr <- node %>% vapply(FUN.VALUE="character", function(n) html_nodes(n, "a") %>% html_attr("href") %>% str_extract("id=\\d+$")  %>% 	str_replace("id=", "") %>% paste0(collapse="/"))
	return(list(definition, definitionnr))
    }

 
###################

url <- "https://ht.ac.uk/category-selection/?qsearch=root"


nodes_to_data <- function(nodes, term){ #note: t_end will be NA if still in use in sense
	#term <- nodes1 %>% html_nodes(".categoryWord") %>% html_text()
	times <- nodes %>% html_nodes(".tData") %>% html_text() %>% lapply(fromJSON) %>% lapply(as.data.frame.list, stringsAsFactors=F) %>% bind_rows
	t <- times$times.starting_time
	t_end <- times$times.ending_time
	id <- nodes %>%  html_nodes("span.small") %>% lapply(., function(x) id_from_one_span_node(x)) #%>% .[!is.na(.)]
	pos <- nodes %>%  html_nodes("span.small") %>% lapply(., function(x) pos_from_one_span_node(x)) #%>%  llply(., unlist) %>% as.vector(.) %>% filter(!is.na)
	#pos_and_id <- cbind(pos, id)
	textfields <- nodes %>% textfields_from_large_span_node(.)
	c(definition, definitionnr)  %<-% textfields
	data <- cbind(term, pos, t, t_end, definition, definitionnr, id) %>% as.data.frame() #contains NAs
	return(data)
}

term_to_data <- function(term){ #includes the scrape
	url <- paste("https://ht.ac.uk/category-selection/?qsearch=", term, sep="")
	page <- url %>% as.character() %>% read_html(.)
	#I do these two types of node separately as I do not know how to say 'nodes of classcatOdd or catEven'
	nodes1 <- html_nodes(page, xpath="//*[@class='catOdd' and preceding-sibling::p[contains(text(), 'total of')] and following-sibling::h4[contains(text(), 'Category results:')]]")
	nodes2 <- html_nodes(page, xpath="//*[@class='catEven' and preceding-sibling::p[contains(text(), 'total of')] and following-sibling::h4[contains(text(), 'Category results:')]]")
	data1 <- nodes_to_data(nodes1, term)	
	data2 <- nodes_to_data(nodes2, term)	
	return(rbind(data1,data2))

}

fwrite(data, file = opt$outputfile, append=TRUE)




 
