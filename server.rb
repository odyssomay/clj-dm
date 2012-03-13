#!/usr/bin/env ruby
require 'webrick'
require 'optparse'
 
include WEBrick    # let's import the namespace so
                   # I don't have to keep typing
                   # WEBrick:: in this documentation.
options = {:Port=>8080, :DocumentRoot=> './'}
 
optparser=OptionParser.new do |opts|
    opts.banner = "Usage: #{File.basename(__FILE__)} [options]"
    opts.on("-p", "--port port_num", "Specify Port Number") do |v|
        options[:Port]=v
    end
    opts.on("-d", "--docroot path", "Specify Document Root Folder") do |v|
        options[:DocumentRoot]=v
    end
end
 
def start_webrick(config = {})
  # always listen on port 8080
  server = HTTPServer.new(config)
  yield server if block_given?
  ['INT', 'TERM'].each {|signal|
    trap(signal) {server.shutdown}
  }
  server.start
 
end
 
begin
  optparser.parse!(ARGV)
  start_webrick(options)
rescue Exception=>e
  puts optparser.to_s
end
