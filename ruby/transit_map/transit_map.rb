#!/usr/bin/env ruby

def sample(density_level)
  (0..density_level).map{|x| x.to_f / density_level}.repeated_permutation(2)
end


#class AssertionError < RuntimeError; end
#def assert &block
#  raise AssertionError unless yield
#end

p sample(3)
p (sample(2).sort == [[0.0, 0.0], [0.0, 0.5], [0.0, 1.0], [0.5, 0.0], [0.5, 0.5], [0.5, 1.0], [1.0, 0.0], [1.0, 0.5], [1.0, 1.0]].sort)
