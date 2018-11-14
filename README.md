# google_search_crawler
warning: hitting google using the same logic and ip might cause 503 error.

this script basically read a device list from a file --> search them in google --> get the result from gsmarena only --> crawl gsmarena data.

gsmarena does not have an official API thus data provided by free API sometimes different from the one in gsmarena.com
therefore, to get the data exactly the same as in its page we need to crawl them.
