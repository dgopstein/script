#!/usr/bin/ruby

#require 'net/http'
require 'open-uri'
require 'rmagick'


Stat = Struct.new(:team_name, :year, :wins, :losses, :ties)

def scrape(url, regex)
  ua = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:15.0) Gecko/20120427 Firefox/15.0a1"
  html = open(url, 'User-Agent' => ua).read

  html.scan(regex)
end

def scrape2(url, regex)
  ua = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:15.0) Gecko/20120427 Firefox/15.0a1"
  html = open(url, 'User-Agent' => ua).read
  p url
  p html
  p url

  html.scan(regex)
end

def years
  scrape('http://www.hockeydb.com/ihdb/stats/leagues/141.html',
         %r|/ihdb/stats/leagues/seasons/nhl1927(\d+).html|).flatten
end

def rosters(year)
  scrape("http://www.hockeydb.com/ihdb/stats/leagues/seasons/nhl1927#{year}.html",
          %r|/ihdb/stats/leagues/seasons/teams/(\d+).html|).flatten
end

def team(roster)
  scrape("http://www.hockeydb.com/ihdb/stats/leagues/seasons/teams/#{roster}.html",
         %r|/stte/([^"]*).html|).flatten.first
end

def stats(team)
  stats = scrape("http://www.hockeydb.com/stte/#{team}.html",
         %r|<td><a href="[^"]*">([-0-9]+)</a></td><td class="l">([^<]+)</td><td class="l">NHL</td><td class="l">\w+</td><td>(\d+)</td><td>(\d+)</td><td>(\d+)</td><td>(\d+)</td>|)
end

def main
  p ys = years
  rosters = ys.last.map{|y| rosters(y)}.flatten
  teams = rosters[0..1].map{|r| team(r)}
  p teams
  stats = teams.map{|t| stats(t)}

  p stats

end

main
