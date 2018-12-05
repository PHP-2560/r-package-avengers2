## Purpose of 01_get_fec.R is to specify query and get data from fec 

# Initialize libraries 
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "stringr") # list of packages required
# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages) 
# Load packages
lapply(required_packages, library, character.only = TRUE)

## Construct FEC URL to get data from
get_fec <- function(state, candidates) {
  
  #keep api key here for now, then specify it in makefile
  myapi<- "jFTYk34OsWkFoEHLcUDa7G1Ax4GCyhJyAgCwB8oz"
  #candidates: "last name", "first name"
  candidate_query = candidates %>% toupper()
  #state
  state_query = c(state) %>% toupper()
  #time period
  year_query="2018"
  
  #1: FIND CANDIDATES
  
  #1.1: Set up query, get url and build initial content list
  baseurl <-"https://api.open.fec.gov/v1"
  endpoint_char <- "/candidates/search/"
  # query.cand<- list(api_key = myapi,state = "TX", candidate_status = "C", election_year = 2018, per_page = 100)
  query.cand<- list(api_key = myapi,candidate_status = "C", election_year = 2018, per_page = 100)
  FEC.cand<-GET(url = paste(baseurl,endpoint_char, sep = "/"), query = query.cand)
  FEC.cand<-rawToChar(FEC.cand$content) %>%
    fromJSON(simplifyDataFrame =  FALSE)
  
  #1.2: Retrieve candidate ids and names
  FEC.cand_clean<- FEC.cand$results 
  nullToNA <- function(x) {
    lapply(x, function(y) ifelse(is.null(y), NA, y))
  }
  FEC.cand_clean<- lapply(FEC.cand_clean, nullToNA)
  l <- lapply(FEC.cand_clean,
              function(x){
                data.frame(
                  candidate_id =  as.character(x$candidate_id), 
                  cand_name = as.character(x$name)
                )
              }
  )
  datapage <- do.call("rbind", l)
  datapage$page<-1
  #create an empty dataframe to store all pages
  candidate_list<-datapage[FALSE,]
  #add first page
  candidate_list<- rbind(candidate_list,datapage)
  #loop to extract the other pages
  candpages<- FEC.cand$pagination$pages
  for (p in 2:candpages) {
    query.cand<- list(api_key = myapi, candidate_status = "C", election_year = 2018, per_page = 100, page = p)
    FEC.cand<-GET(url = paste(baseurl,endpoint_char, sep = "/"), query = query.cand)
    FEC.cand<- rawToChar(FEC.cand$content)
    FEC.cand<-fromJSON(FEC.cand, simplifyDataFrame =  FALSE)
    candpages<- FEC.cand$pagination$pages
    FEC.cand_clean<- FEC.cand$results 
    FEC.cand_clean<- lapply(FEC.cand_clean, nullToNA)
    l <- lapply(FEC.cand_clean,
                function(x){
                  data.frame(
                    candidate_id = as.character(x$candidate_id), 
                    cand_name =as.character(x$name)
                  )
                }
    )
    datapage <- do.call("rbind", l)
    datapage$page<-p
    candidate_list<- rbind(candidate_list,datapage)
  }
  #format candidate list
  #candidate id, split name and last name
  candidate_list = data.frame(
    candidate_list$candidate_id, 
    str_split_fixed(candidate_list$cand_name, pattern = ", ", 2), 
    substr(candidate_list$candidate_id, start = 3, stop = 4),
    substr(candidate_list$candidate_id, start = 1, stop = 1))
  colnames(candidate_list) <-c("candidate_id", "surname", "name" , "state", "race")
  candidate_list$surname<-as.character(candidate_list$surname)
  candidate_list$state<-as.character(candidate_list$state)
  candidate_list$race<-as.character(candidate_list$race)
  
  #1.3: extract the desired name
  candidates<- data.frame(str_split_fixed(candidate_query, pattern = ", ", 2), state_query)
  colnames(candidates)<- c("surname", "first name", "state")
  #merge the candidate_id and clean candidate list for senate race only 
  candidates<- left_join(candidates, candidate_list, by = c("surname", "state")) %>%
    mutate(race = substr(candidate_id,1,1)) %>%
    filter(race=="S") %>% # specifies senate
    group_by(surname, `first name`) %>%
    filter(row_number()==1)
  
  #2: FIND COMMITTEES LINKED TO EACH CANDIDATE
  
  #This section of code works in a similar way to the previous one
  #For each candidate, we get the list of committees, specifying their id, which will then enable us to get the contributions
  for (i in 1:dim(candidates)[1]) {
    cid<-candidates$candidate_id[i]
    endpoint_comm <-paste0("candidate/",cid,"/committees/")
    query.comm<- list(api_key = myapi, candidate_id = cid, per_page=100)
    FEC.comm<-GET(url = paste(baseurl,endpoint_comm, sep = "/"), query = query.comm)
    FEC.comm<- rawToChar(FEC.comm$content)
    FEC.comm<-fromJSON(FEC.comm, simplifyDataFrame =  FALSE)
    FEC.comm_clean<- FEC.comm$results 
    FEC.comm_clean<- lapply(FEC.comm_clean, nullToNA)
    l <- lapply(FEC.comm_clean,
                function(x){
                  data.frame(
                    candidate_id = as.character(x$candidate_ids), 
                    committee_id = as.character(x$committee_id),
                    state = x$state,  
                    party = x$party, 
                    committee_name = x$name, 
                    comm_type = as.character(x$committee_type), 
                    last_date = substr(as.character(x$last_file_date), 1, 7)
                  )
                }
    )
    datapage <- do.call("rbind", l)
    datapage<-datapage %>%
      #Identify the "real committees": looking at those whose last file date is Nov 2018    
      filter(last_date=="2018-11")
    #create an empty dataframe to store all pages (only during 1st iteration)  
    if (i==1) {
      committees_list<-datapage[FALSE,]    
    }
    #append results
    committees_list<- rbind(committees_list,datapage)
  }
  committees_list
  
  #3: FIND ALL INDIVIDUAL DONATIONS GIVEN FOR EACH COMMITTE PREVIOUSLY SELECTED
  
  #This section of code works in a similar way to the previous ones
  #Get the contributions for each committee linked to a candidate 
  endpoint_contr <- "schedules/schedule_a/" #select endpoint to retrieve individual contributions
  FEC.contributions <- tbl_df(NULL) #create empty dataframe for individual contributions
  for (c in 1:length(committees_list$committee_id)) {
    cid <- committees_list$committee_id[c]
    #set initial last_index to NULL to integrate search of first page in a single loop with search or other pages.  
    lindex <- NULL 
    last.contr.dt<-NULL
    w<-0 #page counter 
    while (w>=0) {
      #we have a limit of 1000 calls per hour, which means 16.7 per minute, or one every 3.75 seconds    
      Sys.sleep(3.75)
      w<- w+1
      #hardcoded period under analisys
      myquery<- list(api_key = myapi, election_year="2018", max_date = "2018-11-01", min_date = "2018-10-01", per_page= 100, is_individual = TRUE, committee_id = cid, last_index = lindex, last_contribution_receipt_date = last.contr.dt)
      FEC.resp<-GET(url = paste(baseurl,endpoint_contr, sep = "/"), query = myquery)
      FEC.data<- rawToChar(FEC.resp$content)
      FEC.data<-fromJSON(FEC.data, simplifyDataFrame =  FALSE)
      FEC.data_clean<- FEC.data$results 
      FEC.data_clean<- lapply(FEC.data_clean, nullToNA) 
      old_index<-lindex
      lindex <- FEC.data$pagination$last_indexes$last_index
      last.contr.dt<-FEC.data$pagination$last_indexes$last_contribution_receipt_date
      #BREAK condition to interrupt the while loop when there are no pages anymore
      if (is.null(lindex)) {
        break
      }
      l <- lapply(FEC.data_clean,
                  function(x){
                    data.frame(
                      id= x$sub_id,
                      contrib_name = x$contributor_name,
                      contrib_state = x$contributor_state,
                      city = x$contributor_city, 
                      zipcode = x$contributor_zip, 
                      street = x$contributor_street_1, 
                      occupation = x$contributor_occupation, 
                      employer = x$contributor_employer, 
                      date = x$contribution_receipt_date, 
                      election_year = x$fec_election_year, 
                      election = x$election_type, 
                      election_desc = x$fec_election_type_desc, 
                      committee_id = x$committee_id, 
                      amount = x$contribution_receipt_amount
                    )
                  }
      )
      datapage <- do.call("rbind", l)
      datapage$page<-w
      #create an empty dataframe to store all pages for a given committee
      if (w==1) {
        commpage<-datapage[FALSE,]  
      }
      #add first page
      commpage<- rbind(commpage,datapage)
    }
    commpage <- left_join(commpage, committees_list) # add committee info to each contribution
    
    FEC.contributions<- rbind(FEC.contributions, commpage)
  }
  #Remove duplicates
  FEC.contributions<-unique(FEC.contributions)

  # save contributions file so we don't have to redownload
  write.csv(FEC.contributions, file = paste0("./data/", state,"_ind_cont.csv"))
}

get_fec(input_state, input_candidates)
