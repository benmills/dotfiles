#!/usr/bin/env ruby 
filename = "/Users/benmills/Library/Logs/Growl.log"
f = File.open(filename, "r+")
old_last = "nil"
last = ""

while true
  f.each_line do |line|
    if line.include?("growlnotify:")
      last =  line.split("growlnotify:")[1]
    end
  end

  if old_last != last
    old_last = last
    puts last
  end
end
