
#' @title Get Token
#' @description getToken() returns a token needed to run getArthroCollections() and getPools(). The function prompts users for a VectorSurv account credentials.
#' @keywords authentication
#' @return User token
#' @importFrom rstudioapi askForPassword
#' @importFrom httr GET POST add_headers content modify_url
#' @examples
#'  \dontrun{token = getToken()}
#' @export
getToken = function(){


  #Prompt credentials
  username = askForPassword("Gateway username")
  password = askForPassword("Gateway password")


  #HTTP
  headers = c(
    'Content-Type' = 'application/json'
  )

  body = paste('{
      "username":', '\"',username,'\",',
               '"password":' ,'\"',password,'\"',
               '}', sep="");


  response <- POST( url = "https:/api.vectorsurv.org/login", body = body, add_headers(headers))

  response_content <- content(response, 'parsed')

  if(is.null(response_content$token)){
    stop("Check username and password")
  }
    token <- response_content$token
  # ids = c()
  # for (i in 1:length(response_content$agencies)){
  #
  #   ids = rbind(ids,cbind(response_content$agencies[[i]]$id,response_content$agencies[[i]]$code))
  #
  # }


  return(token)
}
