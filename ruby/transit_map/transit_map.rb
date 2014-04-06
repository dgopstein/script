#!/usr/bin/env ruby

require 'facets'

def sample(density_level)
  (0..density_level).map{|x| x.to_f / density_level}.repeated_permutation(2)
end

LatLng = Struct.new(:lat, :lng) do
  def to_a; [lat, lng]; end
  def to_s; "#{lat},#{lng}"; end
  def inspect; to_s; end
end

class Brooklyn
  def self.Top; 40.7206157; end
  def self.Bottom; 40.6302637; end
  def self.Left; -74.0040491; end
  def self.Right; -73.8929842; end
end

NYUPoly = LatLng.new(40.694074,-73.986932)
Destination = NYUPoly

def sample_brooklyn
  steps = 19 # 400 points
  y_len, x_len = [Brooklyn.Top - Brooklyn.Bottom, Brooklyn.Right - Brooklyn.Left]

  sample(steps).map{|y, x| LatLng.new(y*y_len + Brooklyn.Bottom, x*x_len + Brooklyn.Left)}
end

def point(ll)
  <<EOS
    <Placemark>
      <styleUrl>#pushpin</styleUrl>
      <Point>
        <coordinates>#{ll.lng},#{ll.lat},0</coordinates>
      </Point>
    </Placemark>
EOS
  
end

def kml
<<EOS
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Folder>
    <GroundOverlay>
      <Icon>
        <href>groceries.png</href>
      </Icon>
      <LatLonBox>
        <north>#{Brooklyn.Top}</north>
        <south>#{Brooklyn.Bottom}</south>
        <east>#{Brooklyn.Left}</east>
        <west>#{Brooklyn.Right}</west>
        <rotation>0</rotation>
      </LatLonBox>
    </GroundOverlay>
    #{sample_brooklyn.map{|x| point(x)}.join("\n")}
  </Folder>
</kml>
EOS
end

def get(url, params=nil)
  require 'net/http'
  require 'openssl'
  uri = URI.parse(url)
  uri.query = URI.encode_www_form( params ) if params
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_NONE # read into this
  http.get(uri.request_uri).body
end

KEY = File.read(ENV['HOME']+'/script/GOOGLE_PLACES_API_KEY').strip

class Array
  # sum is definited in facets
  #def sum 
  #  inject(:+)
  #end

  def mean
    sum / size.to_f
  end
end

def directions_from(origin)
  url =  "https://maps.google.com/maps/api/directions/json"
  params = {
    origin: origin,
    destination: Destination,
    sensor: false,
    key: KEY,
    departure_time: 1396743295,
    mode: 'transit'
  }

  require 'json'
  JSON.parse(get(url, params))
end

def mean_duration(google_json)
  google_json['routes'].flat_map{|r| r['legs']}.map{|l| l['duration']['value']}.mean
end

def main
  File.write('transit_map.kml', kml)

  res = sample_brooklyn.take(2).mash do |origin|
    google_json = directions_from(origin)

    mean_duration = mean_duration(google_json)

    [origin, mean_duration]
  end

  p res
end

main
