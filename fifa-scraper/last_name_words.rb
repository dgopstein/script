#! /usr/bin/env ruby

require 'set'

dict_file = '/usr/share/dict/words'

dict = File.open(dict_file).each_line.reject{|word| word =~ /[A-Z]/}.map(&:downcase).map(&:strip)

last_name_file = 'all_last_names.txt'

last_names = File.open(last_name_file).each_line.map(&:downcase).map(&:strip)

last_name_words = last_names & dict

puts last_name_words.sort.join("\n")
