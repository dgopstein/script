#!/usr/bin/env ruby

def sample(density_level)
  (0..density_level).map{|x| x.to_f / density_level}.repeated_permutation(2)
end

LatLng = Struct.new(:lat, :lng) do
  def to_a; [lat, lng]; end
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

File.write('transit_map.kml', kml)
