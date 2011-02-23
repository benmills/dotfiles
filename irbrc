# load libraries
require 'rubygems'
require 'wirble'
require 'utility_belt'


# start wirble (with color)
Wirble.init
Wirble.colorize


if defined?(RUBY_DESCRIPTION)
 puts "# #{RUBY_DESCRIPTION}"
else
 puts "# ruby #{RUBY_VERSION}"
end

def cls
  system 'clear'
end

# Activate auto-completion.
require 'irb/completion'


# Setup permanent history.
HISTFILE = "~/.irb_history"
MAXHISTSIZE = 100
begin
  histfile = File::expand_path(HISTFILE)
  if File::exists?(histfile)
    lines = IO::readlines(histfile).collect { |line| line.chomp }
    puts "Read #{lines.nitems} saved history commands from '#{histfile}'." if $VERBOSE
    Readline::HISTORY.push(*lines)
  else
    puts "History file '#{histfile}' was empty or non-existant." if $VERBOSE
  end
  Kernel::at_exit do
    lines = Readline::HISTORY.to_a.reverse.uniq.reverse
    lines = lines[-MAXHISTSIZE, MAXHISTSIZE] if lines.nitems > MAXHISTSIZE
    puts "Saving #{lines.length} history lines to '#{histfile}'." if $VERBOSE
    File::open(histfile, File::WRONLY|File::CREAT|File::TRUNC) { |io| io.puts lines.join("\n") }
  end
end

# Add helper method to Object
module LocalMethods
  def local_methods
    self.methods.sort - Object.methods
  end
end

Object.class_eval { include LocalMethods }
