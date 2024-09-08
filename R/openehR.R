query <- function(url, query, config = list(), ...) {

  if (!endsWith(url, "/")) {
    url = paste0(url, "/")
  }

  # Remove any newlines
  stripped_query = stringr::str_replace_all(query, "\n", " ")

  # Remove any unnecessary whitespaces
  stripped_query = stringr::str_squish(stripped_query)

  url = paste0(url, "query/aql")

  response <- httr::POST(
    url = url,
    body = list(q = stripped_query),
    encode = "json",
    config = config,
    ...
  )

  if (httr::http_error(response)) {
    stop(
      paste0(
        "Could not execute query. HTTP ",
        response$status_code,
        " Response:\n",
        httr::content(response, as = "text")
      )
    )
  } else {
    # Get the content of the response as text
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    # Parse resultset JSON
    resultSet <- jsonlite::fromJSON(content, flatten = TRUE)
    
    # Extract rows and columns
    rows <- resultSet$rows
    columnNames <- resultSet$columns$name
    
    # Create data frame and assign column names
    dataframe <- as.data.frame(rows, stringsAsFactors = FALSE)
    
    # Assign column names if the result set contains any data
    if (ncol(dataframe) > 0 && length(columnNames) == ncol(dataframe)) {
      colnames(dataframe) <- columnNames
    }
    
    return(dataframe)
  }
}
