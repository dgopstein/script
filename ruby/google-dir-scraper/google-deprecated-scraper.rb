#!/usr/bin/ruby

require 'httparty'
require 'json'

if ARGV.size < 1
  print "usage: google-dir-scraper URL"
end

query = ARGV[0]

URL = 'https://ajax.googleapis.com/ajax/services/search/web'

def search(query, page = 0, results = 8)
  params = {v: 1.0,
   rsz: results,
   start: page * results,
   q: query
   }
  response = HTTParty.get(URL, :query => params)
  JSON.parse(response)
end

def search_take(query, max_results = 5000)
  all_results = []
  finished = false
  page = 0

  while !finished
    response = search(query, page)
    results = begin
        response["responseData"]["results"]
      rescue
        puts "Error reading responseData"
        p response
        []
      end

    all_results += results

    if results.empty? || all_results.size >= max_results
      finished = true
    else
      page += 1
      Kernel.sleep 0.5
    end
  end

  all_results
end

search_take(query).map{|s| puts s['unescapedUrl']}
