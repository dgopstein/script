#!/usr/bin/env ruby

HOME = ENV['HOME']
#$LOAD_PATH << "#{HOME}/deep_enumerable/lib"

require 'facets'
require 'json'
#require 'deep_enumerable'

def sample(density_level)
  (0..density_level).map{|x| x.to_f / density_level}.repeated_permutation(2)
end

LatLng = Struct.new(:lat, :lng) do
  def to_a; [lat, lng]; end
  def to_s; "#{lat},#{lng}"; end
  def inspect; "<#{to_s}>"; end
  def self.from_s(s); self.new(*s.split(',')); end
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

def point(ll, text='2', color='5680FC')
<<EOS
    <Placemark>
      <Style>
        <IconStyle>
          <Icon>
            <href>http://thydzik.com/thydzikGoogleMap/markerlink.php?text=#{text}&amp;color=#{color}</href>
          </Icon>
        </IconStyle>
      </Style>
      <Point>
        <coordinates>#{ll.lng},#{ll.lat},0</coordinates>
      </Point>
    </Placemark>
EOS
  
end

def kml(points)
<<EOS
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Folder>
  <!--
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
  -->
    #{points.map{|ll, name, color| point(ll, name, color)}.join("\n")}
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

KEY = File.read("#{HOME}/script/GOOGLE_PLACES_API_KEY").strip

class Array
  # sum is definited in facets
  #def sum 
  #  inject(:+)
  #end

  def mean
    sum / size.to_f
  end
end

class Hash
  def map_keys(&block) # Add to deep_enumerable?
    Hash[self.map {|k, v| [block.call(k), v] }]
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

  JSON.parse(get(url, params))
end

def mean_duration(google_json)
  google_json['routes'].flat_map{|r| r['legs']}.map{|l| l['duration']['value']}.mean
end

def duration_sample_brooklyn
  res = sample_brooklyn.mash do |origin|
    google_json = directions_from(origin)

    mean_duration = mean_duration(google_json)

    [origin, mean_duration]
  end
end

def cached_duration_samples
  filename = 'brooklyn_duration_samples.json'
  begin
    JSON.parse(File.read(filename))
      .map_keys{|ll| LatLng.from_s(ll)}
      .tap do |json|
        puts "Using cached file: #{filename} - #{json.length} points imported"
    end
  rescue Exception => e
    puts "Re-querying Google: #{e.message}"
    duration_sample_brooklyn.tap do |hash|
      File.write(filename, hash.to_json)
    end
  end
end

def write_kml(points)
  puts "Writing KML"
  File.write('transit_map.kml', kml(points))
end

# argb_unpack("E6EFA8".to_i(16)) == [0, 230, 239, 168]
def argb_unpack(color)
  3.downto(0).map do |shift|
    mask = (0xFF << shift*8)
    channel = color & mask
    value = channel >> shift * 8
    value
  end
end

# argb_pack(0, 230, 239, 168).to_s(16).upcase == "E6EFA8"
def argb_pack(*channels)
  channels.inject(0) { |color, channel| (color << 8) | channel }
end

def interpolate_color(color_range, value_range, value)
  is_string = color_range.any?{|c| c.is_a? String}
  c_min, c_max = color_range.map{|c| c.to_i(16) if is_string}
  v_min, v_max = value_range
  v_pct = (value - v_min) / (v_max - v_min).to_f

  an,rn,gn,bn = argb_unpack(c_min)
  ax,rx,gx,bx = argb_unpack(c_max)

  a = (an + ((ax - an) * v_pct)).to_i
  r = (rn + ((rx - rn) * v_pct)).to_i
  g = (gn + ((gx - gn) * v_pct)).to_i
  b = (bn + ((bx - bn) * v_pct)).to_i

  color = argb_pack(a, r, g, b)

  if is_string then color.to_s(16) else color end
end

def colorize(vals)
  color_min = '00EE00'
  color_max = 'EE0000'
  val_min = vals.min
  val_max = vals.max

  vals.map{|val| interpolate_color([color_min, color_max], [val_min, val_max], val)}
end

def main
  duration_samples = cached_duration_samples
  colors = colorize(duration_samples.values)
  formatted_durations =
    duration_samples.zip(colors)
                    .map(&:flatten)
                    .map{|ll, dist, color| [ll, (dist / 60).to_i, color]}
  write_kml(formatted_durations)
end

main
