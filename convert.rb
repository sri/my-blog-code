#! /usr/bin/env ruby1.8

# convert.rb -- converts files to PDF and
#    converts .tar.gz, .tar.bz2, etc. files to .zip.
# [I use this as a CGI up on my server.]
#
# sri, first ruby program -- 2006-10-22T21:31:56
# Released under MIT license
#
# $Id: convert.rb,v 1.3 2006/11/27 00:58:08 sri Exp $
#
# TODO:
# - handle if popen can't find program
# - convert metapost
# - if a program needs user opts,
#   (and it is somewhere in the middle),
#   stop before this cmd, query user and use
#   that...
  
require 'cgi'
require 'tmpdir'
require 'ftools'
require 'fileutils'

# converts the url passed in by the user
# to a zip file (after unpacking it...);
# used, to convert ".tar.gz" (like) formats to zip format
$convert_to_zip = nil

# some helpers:
def ignore_errors &block
  begin
    block.call
  rescue Exception => e
    e
  end
end

def with_tmpdir &block
  result = nil
  original = Dir.pwd
  tmp = tmpname
  begin
    Dir.mkdir tmp
    Dir.chdir tmp
    result = block.call original
  ensure
    Dir.chdir original
  end
  FileUtils.rm_rf tmp
  result
end

def tmpname
  File.join(Dir.tmpdir, "#{Process::pid}-#{rand 10_000_000}")
end

# files, newest created first to ... oldest
def newest(dirname=nil)
  dirname = '.' if dirname.nil?
  e = Dir.entries(dirname)
  e.delete('.')
  e.delete('..')
  e.sort {|x,y| File.ctime(y) <=> File.ctime(x)}[0]
end

def describe_regexp x
  x = x.to_s
end

# ---------------------------------------------------------------------

$converters = []

def defconverter(name, regexp, &block)
  $converters << [name, regexp, block]
end

def try_converters(what, recursive_call=nil)
  if recursive_call.nil?
    sh "wget -q '#{what}'"
    what = newest
  end
  $converters.each { |x|
    if x[1] =~ what
      continue = x[2].call(what)

      # if converting some archive to
      # zip, need to run converters only once
      if $convert_to_zip
        #
        # let's not include the original
        # in this zip archive -- the file
        # might be something like `x.gz' --
        # and gunzipping it will produce x,
        # the original doesn't remain...
        #
        ignore_errors { File.delete what }
        #
        # Firefox 1.5 has trouble downloading
        # <xyz>.tar.gz.zip files...
        #
        what = what.gsub('.', '-')
        sh "zip -r #{what+'.zip'} ."
        return what+'.zip'
      end
        
      # most recently created file was
      # by the above operation
      name = newest
      # name might have a converter...
      if continue
        return try_converters(name, true)
      else
        return name
      end
    end
  }
  # if recursive_call is true, it means
  # we found a converter -- we were just
  # checking if there was another converter
  # for the newly produced name...
  recursive_call ? what : nil
end
    
def sh cmd
  print "running: <b>#{cmd}</b><br>"
  IO.popen(cmd) {|f| print f.read }
  print "<br>"
  $stdout.flush
end
  
# ---------------------------------------------------------------------
# our converters:
# converters operate like so:
# 3 args: first, a symbol naming the converter;
# second, a regexp that if matched the third arg
# is executed: which is a block that takes one
# argument -- a local filename
# the converter should return true or nil:
# true to continue and try other converters;
# nil to return -- as this is the final one.

defconverter(:ps, /\.e?ps$/) { |x|
  sh "ps2pdf #{x}"
  nil
}

defconverter(:cweb, /\.w$/) { |x|
  sh "cweave #{x} - #{x}.tex"
  sh "tex #{x}.tex"
  sh "dvips #{x}.dvi"
  sh "ps2pdf #{x}.ps"
  nil
}

defconverter(:tex, /\.tex$/) { |x|
  File.cp x, "#{x}.tex"
  sh "tex #{x}.tex"
  sh "dvips #{x}.dvi"
  sh "ps2pdf #{x}.ps"
  nil
}

defconverter(:dvi, /\.dvi$/) { |x|
  File.cp x, "#{x}.dvi"
  sh "dvips #{x}.dvi"
  sh "ps2pdf #{x}.ps"
  nil
}

# ruby translation of Noah Friedman's untar
# http://www.splode.com/~friedman/software/scripts/src/untar

defconverter(:tar_gzipped,
             /\.(t[ag]z|tar\.gz|tar\.[Zz]|xtar\.gz|xtar\.[Zz]|nif)$/) { |x|
  sh "gzip -dc #{x} | tar -xpvf -"
  true
}

defconverter(:tar_bzip2ped,
             /\.(tbz|tbz2|tar\.bz2|xtar\.bz2|tar\.bz)$/) { |x|
  sh "bzip2 -dc #{x} | tar -xpvf -"
  true
}

defconverter(:tar, /\.(tar|xtar)$/) { |x|
  sh "tar -xpvf #{x}"
  true
}

defconverter(:zip, /\.(zip|jar)$/) { |x|
  sh "unzip -o #{x}"
  true
}

defconverter(:rpm, /\.rpm$/) { |x|
  sh "rpm2cpio #{x} | cpio -dimv --no-absolute-filenames"
  true
}

defconverter(:lzh, /\.lzh$/) { |x|
  # the first x is an opt -- not a misspell!
  sh "lha x #{x}"
  true
}

defconverter(:rar, /\.rar$/) { |x|
  # the first x is an opt -- not a misspell!
  sh "unrar x #{x}"
  true
}

# i don't get this:
#defconverter(:ar, /\.a$/) { |x|
#  libdir = File.basename(x, '.a')
#  if x[0] != ?/ then x = "../${x}" end
#  sh "mkdir -p #{libdir}"
#  sh "cd #{libdir} && ar xv #{x}"
#  true
#}

# order of the defined converters matter,
# so we want these at the end...
defconverter(:gunzip, /\.(gz|Z)$/) { |x|
  sh "gunzip #{x}"
  true
}

defconverter(:bzip2, /\.(bz2|bzip2)$/) { |x|
  sh "bunzip2 #{x}"
  true
}

# Metapost to PDF
defconverter(:metapost, /\.mp$/) { |x|
  sh "mptopdf #{x}"
  false
}

# ---------------------------------------------------------------------

def main
  cgi = CGI.new 'html4'
  url = cgi.params['url'].to_s.strip
  $convert_to_zip = ! cgi.params['zipitup'].to_s.empty?
  
  if url.empty?
    #$converters.sort! {|x, y| x[0].to_s <=> y[0].to_s }
    cgi.out {
      cgi.html("PRETTY" => " ") {
        cgi.head { cgi.title { 'convert.rb' } } +
        cgi.body {
          cgi.h3 { 'convert.rb &mdash; convert file to PDF or ZIP' } +
          cgi.form('get', $0) {
            cgi.b { 'url:' } +
            cgi.text_field('url', '', 150) + '&nbsp;' +
            cgi.submit + cgi.br +
            cgi.checkbox('zipitup') + ' zip it up? ' +
            cgi.small { '[with this turned on, you can convert
                          unix archive formats (like .tar.gz, .tar.bz2,
                          ...) to .zip format]' }
          } + cgi.br +
          
          cgi.b { 'available converters:' } + 
          cgi.small { "(the order in which they are tried)" } + cgi.br +
          
          $converters.map {|x| "#{x[0]} -- #{x[1].source}" + cgi.br }.to_s +
          
          cgi.br + cgi.b { 'some recently converted files:' } + cgi.br +
          Dir.entries('data').reject {|x| x=='.'||x=='..'}.map {|x|
            cgi.a("data/#{x}") { x } + cgi.br
          }.to_s + 
          
          cgi.br +
          cgi.div('align'=>'center') { cgi.a('convert.rb-txt') { 'src' } }
        }
      }
    }    
  else
    # i can't use cgi.out { ... } because i would like
    # to run output of each cmd and flush output to browser
    # for user to see what's in progress...
    # (i'm not even sure that's valid http protocol...)
    
    print "content-type: text/html\r\n\r\n"
    
    with_tmpdir {|original|
      print "<pre>"
      name =  try_converters cgi.params['url'].to_s
      print "</pre>"
      
      if name.nil?
        print "Couldn't find converter" + cgi.br + cgi.a($0) { 'home' }
      else
        File.cp(name, "#{original}/data/#{name}")
        print cgi.br + cgi.hr + cgi.a("data/#{name}") { name }
      end
    }
  end
end


if $0 == __FILE__
  main
end
