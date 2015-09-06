#!/usr/bin/ruby

require 'google-search'

puts

def find_items(query, max_cnt = 5000)
  items = []

  finished = false
  est_cnt = 0
  offset = 0
  
  while !finished
    search = Google::Search::Web.new do |s|
      s.query = query
      s.size = :large
      #s.each_response { print '.'; $stdout.flush }
      s.offset = offset
    end
    est_cnt = search.response.estimated_count
    if est_cnt > 5000
      puts "More than 5000 results" 
      finished = true
    end
    search_items = search.to_a
    items += search_items
    if search_items.empty?
      finished = true 
    else
      offset = items.size
    end
    # items[-10] and items[-10..-1].each {|i| puts i.uri }
    puts "#{items.length}  / #{est_cnt}"
  end

  items
end

if ARGV.size < 1
  print "usage: google-dir-scraper URL"
end

url = ARGV[0]

items = find_items("site:#{url}")

#puts search.response.to_a.map(&:uri).join("\n")
items.each do |item|
  puts item.uri
end
