#!/usr/bin/env ruby

require 'csv'

filepat = 'players_html/clubs_%02d.html'

CSV.open("clubs.csv", "w") do |csv|
  (1..25).each do |page_num|
    html = File.open(filepat % page_num).read
    names = html.scan(%r{<td class="text-left"><a href=".*?.png">([^<]+)</a></td>}m)
    names.each{ |name|
      csv << name
    }
  end
end
