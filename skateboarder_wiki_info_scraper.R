library(rvest)
library(stringr)
library(data.table)
library(jsonlite)

infobox_xpath <- '//*[@id="mw-content-text"]/div[1]/table[1]'
url_prefix <- "https://en.wikipedia.org/?curid="

find_height <- function(info) {
  height_index <- str_locate(info, "(?i)height")[2]
  if(!is.na(height_index)) {
    height_row <- substr(info, height_index + 1, height_index + 20)
    ft_index <- str_locate(height_row, "(?i)ft")[1,1]
    in_index <- str_locate(height_row, "(?i)in")[1,1]
    if(!is.na(ft_index)) {
      height_ft <- as.numeric(str_extract(substr(height_row, ft_index - 3, ft_index - 1), "[0-9]"))
      if(is.na(in_index)) {
        height_in = 0
      } else {
        height_in <- as.numeric(str_extract(substr(height_row, in_index - 4, in_index - 1), "[0-9]+"))
      }
      total_height <- height_ft * 12 + height_in
    } else {
      total_height <- NA
    }
    m_index <- str_locate(height_row, "(?i)cm")[1,1]
    using_cm <- TRUE
    if(is.na(m_index)) {
      m_index <- str_locate(height_row, "(?i)m")[1,1]
      using_cm <- FALSE
    }
    if(!is.na(m_index)) {
      height_m <- as.numeric(str_extract(substr(height_row, m_index - 6, m_index - 1), "[0-9]+\\.*[0-9]+"))
    } else {
      height_m <- NA
    }
    return(data.table(
      "height_in" = total_height, 
      "height_m" = ifelse(using_cm, height_m / 100, height_m)
    ))
  } else {
    return(data.table(
      "height_in" = NA,
      "height_m" = NA
    ))
  }
}

find_weight <- function(info) {
  weight_index <- str_locate(info, "(?i)weight")[2]
  if(!is.na(weight_index)) {
    weight_row <- substr(info, weight_index + 1, weight_index + 20)
    lb_index <- str_locate(weight_row, "(?i)lb")[1,1]
    if(!is.na(lb_index)) {
      weight_lb <- as.numeric(str_extract(substr(weight_row, lb_index - 5, lb_index - 1), "[0-9]+\\.*[0-9]+"))
    } else {
      weight_lb <- NA
    }
    kg_index <- str_locate(weight_row, "(?i)kg")[1,1]
    if(!is.na(kg_index)) {
      weight_kg <- as.numeric(str_extract(substr(weight_row, kg_index - 5, kg_index - 1), "[0-9]+\\.*[0-9]+"))
    } else {
      weight_kg <- NA
    }
    data.table(
      "weight_lb" = weight_lb, 
      "weight_kg" = weight_kg
    )
  } else {
    data.table(
      "weight_lb" = NA, 
      "weight_kg" = NA
    )
  }
}

search_wiki <- function(srsearch) {
  srsearch <- gsub(" ", "%20", srsearch)
  full_search_string <- paste("http://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=",srsearch,"&format=json",sep="")
  response <- fromJSON(full_search_string)
  response
}

get_skater_data <- function(skater_name) {
  search_result <- search_wiki(str_c(skater_name, "skateboarder", sep = "%20"))$query$search
  if(str_detect(search_result$title[1], skater_name)) {
    page_id <- search_result$pageid[1]
    wiki_url <- str_c(url_prefix, page_id, sep = "")
    skater_info <- read_html(wiki_url) %>%
      html_nodes(xpath = infobox_xpath) %>%
      html_text()
    height <- find_height(skater_info)
    weight <- find_weight(skater_info)
    return(cbind(data.table("name" = skater_name), height, weight))
  } else {
    return(NULL)
  }
}

compile_data <- function(names) {
  all_data <- data.table(
    name = character(), 
    height_in = numeric(), 
    height_m = numeric(), 
    weight_lb = numeric(), 
    weight_kg = numeric()
  )
  for(i in seq_along(names)) {
    data <- get_skater_data(names[i])
    if(!is.null(data)) {
      all_data <- rbindlist(list(all_data, data))
    }
  }
  all_data
}
compile_data(names = c("Nyjah Huston", "Brandon Valjalo", "Yuto Horigome", "Keegan Palmer"))
