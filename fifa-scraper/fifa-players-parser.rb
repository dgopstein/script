#!/usr/bin/env ruby

require 'csv'

#filepat = 'players_html/players_%03d.html'
filepat = 'players_html/keepers_%02d.html'

CSV.open("keepers.csv", "w") do |csv|
  (1..39).each do |page_num|
    html = File.open(filepat % page_num).read
    names = html.scan(%r{class="name">\s*(.*?)<br />.*?<span>(.*?)</span>.*?position">(.*?)<.*?rating.*?>.*?>\s*(\d\d)}m)
    names.each{ |name|
      csv << name
    }
  end
end
