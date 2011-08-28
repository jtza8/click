#!/usr/bin/ruby

require "fileutils"
require "pathname"

ARGV.each do |path_name|
  dir_name, file_name = path_name.match(/^(\w+)\-(.*)/)[1, 2]
  dest = Pathname.new(dir_name) + file_name
  FileUtils.mkdir_p dir_name
  FileUtils.mv(path_name, dest)
end
