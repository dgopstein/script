#!/usr/bin/env ruby

def sample(density_level)
  (0..density_level).map{|x| x.to_f / density_level}.repeated_permutation(2)
end

LatLng = Struct.new(:lat, :lng) do
  def to_a; [lat, lng]; end
end

class Brooklyn
  def self.Top = 40.7206157
  def self.Bottom = 40.6302637
  def self.Left = -74.0040491
  def self.Right = -73.8929842
end

def sample_brooklyn
  steps = 19 # 400 points
  side_len = [Brooklyn.Top - Brooklyn.Bottom, Brooklyn.Right - Brooklyn.Left].map(&:abs).max
  deg_step = side_len / steps

  sample(steps).map{|y, x| LatLng.new(y*deg_step + Brooklyn.Bottom, x*deg_step + Brooklyn.Left)}
end
#class AssertionError < RuntimeError; end
#def assert &block
#  raise AssertionError unless yield
#end

p (sample(2).sort == [[0.0, 0.0], [0.0, 0.5], [0.0, 1.0], [0.5, 0.0], [0.5, 0.5], [0.5, 1.0], [1.0, 0.0], [1.0, 0.5], [1.0, 1.0]].sort)
p sample(19).size == 400
