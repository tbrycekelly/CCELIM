
#' Notify
#'
#' This function allows the automated notification of the completetion of the model.
#' @param name The name of the model run that is now completed.
#' @import twitteR
#' @export
#' @examples notify()
notify = function(name) {
    consumer_key <- 'nL2kyC5p2mbuvgU4VyXl2Ie62'
    consumer_secret <- 'CjupL1L7vTH0lw1FPQ7GKrbeXTk0wA4S2ZngAhXKZj1APiGqSe'
    access_token <- '3063984053-Ply0hfQ0CgPeLyrZJn5XqX8KBzzIh2MSjN8mduO'
    access_secret <- 'SopyvYaPqqrzMBNxSQ89AuXpYhWEcqWHElZcmXpFwYXIq'
    setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
    
    message = paste("@TBryceKelly. Your results of ",name, " are now in.")
    try(tweet(message))
}