#! /usr/bin/env ruby1.8

require 'pp'

require 'rubygems'
require_gem 'syntax'

# visualize.rb -- Help visualize the source
#   structure of Ruby file/package.


module Visualize


  # ==================================================================
  #
  # A construct is of type Class, Module or Def
  #
  
  class Construct
    attr_reader :type, :name, :path, :children
    attr_accessor :moved, :closed, :old_id

    def initialize(type, name, path)
      @type = type
      @name = name
      @path = path
      @children = []
      @moved = false
      @old_id = -1
    end

    def self.fromtree tree
      return if tree.empty?
      name, type, path, *childtree = tree
      c = Construct.new(name, type, path)
      kids = childtree.collect { |x| Construct.fromtree x }
      c.children.concat(kids) unless kids.nil?
      c
    end
    
    def to_a
      [name, type, path] + children.collect { |x| x.to_a }
    end

    def clone
      c = Construct.new(type, name, path)
      c.moved = self.moved
      c.old_id = (old_id==-1 ? object_id : old_id)
      children.each { |x| c.children << x.clone }
      c
    end

    def same(other)
      type == other.type && name == other.name
    end
    
    def mark_as_moved
      self.moved = true
      children.each { |c| c.mark_as_moved }
    end

    def merge(other)
      merge1(other) if (!moved && same(other) && type != :def)
    end

    def merge1(other)
      other.children.each do |o|
        next if o.moved
        child = children.select { |x| x.same o }
        if child.empty?
          children << o.clone
          o.mark_as_moved
        else
          child.each { |c| c.merge o }
        end
      end
    end

    def pp(indentation_level=0, stream=$stdout)
      return if moved
      return if (!children.empty? && children.all? { |x| x.moved })

      out = (' ' * indentation_level) + type.to_s + ' ' + name.to_s
      stream.puts out
      children.each { |c| c.pp(2+indentation_level, stream) }
    end
  end

  

  # ==================================================================
  #
  # SourceFile
  #
  
  class SourceFile
    INTERESTING = ['def',
                   'module',
                   'class']
    BORING      = ['begin',
                   'if',
                   'case',
                   'do',
                   'for',
                   'unless',
                   'until',
                   'while']

    attr_reader :path, :constructs, :requires

    def initialize(path)
      @path = path
      @constructs = []
      src = File.read(@path)
      @alltokens = collect_tokens(src)
      @requires = collect_requires src
      gather
    end

    # We do it this way because "require" isn't
    # a keyword -- it is an identifier,
    # and going the tokenizer route isn't that much
    # cleaner that this way...
    def collect_requires src
      src.scan(/^[\t ]*require[ \t]*['"](.*?)['"]/).collect { |x| x[0] }
    end

    def collect_tokens src
      t = []
      result = []
      tokenizer = Syntax.load 'ruby'
      tokenizer.tokenize(src) { |token| t << [token] }
      mark_statement_modifiers t
      t.each do |token|
        token = token[0]
        what = token.to_s.strip
        next if what.empty?
        #puts what + ' ' + token.group.to_s
        result << [what, token.group]
      end
      result
    end


    class StatementModifier < String
      attr_reader :group

      def initialize(me, group)
        super me+'-statement-modifier'
        @group = group
      end
    end


    # In the gather method, we assume
    # keywords IF, UNLESS, WHILE & UNTIL
    # have matching end to go with them.
    # So here mark the statement modifier,
    # as StatementModifier.
    def mark_statement_modifiers tokens
      # If it starts after a newline or
      # a semicolor, it is a statement.
      # [Currently doesn't bother about
      #  line continations.]
      tokens.each_with_index do |t, i|
        next if i == 0
        next if t[0].group != :keyword
        next if not ['unless', 'until',
                     'while', 'if'].member?(t[0])

        prev = tokens[i-1][0]

        if prev.group == :normal && prev.member?("\n")
          next # it is a statement, do nothing
        end

        # skip whitespace
        j = i-1
        while (j > 0 &&
               tokens[j][0].group == :normal &&
               tokens[j][0].split(//).all? { |c| c == ' ' })
          j -= 1
        end

        prev = tokens[j][0]

        # it is a statement, do nothing
        next if (prev.group == :punct && prev == ";")

        # it is a statement modifier
        t[0] = StatementModifier.new(t[0], t[0].group)
      end
    end



    def getname all
      all.pop[0]
    end

    
    def gather
      stack = @constructs
      all = @alltokens.reverse

      while !all.empty?
        what, group = all.pop
        next unless group == :keyword

        if INTERESTING.member?(what)
          stack << Construct.new(what.to_sym, getname(all), path)
        elsif BORING.member?(what)
          stack << what
        elsif what == 'end'
          if BORING.member?(stack[-1])
            stack.pop
          else
            # construct.closed means that it
            # it a toplevel construct
            if stack.length == 1
              stack[0].closed = true
            else
              # Add this construct in
              # into enclosing one or create new.
                                
              last = stack.pop
              toplevel = found = false
              idx = stack.length+1

              stack.reverse_each do |c|
                idx -= 1
                if BORING.member?(c)
                  # keep going
                elsif c.closed
                  toplevel = true
                  break
                else
                  c.children << last
                  found = true
                  break
                end
              end

              if toplevel
                last.closed = true
                stack.insert(idx, last)
              elsif not found
                last.closed = true
                stack.insert(0, last)
              end

            end
          end
        end
      end
    end

    def pp
      constructs.each { |c| c.pp }
    end

    def merge(other)
      other.constructs.each { |oc|  constructs.each { |c| c.merge(oc) } }
    end

  end



  
  # ==================================================================
  #
  # SourcePackage
  #
  
  #
  # Basic algorithm is as follows:
  # Create a mapping: Required_File => Files_that_Require_It
  # Merge all the Files_that_Require_It for a given
  # key, with each other.
  #
  class SourcePackage
    
    def initialize(dir)
      path = File.join(dir, '**', '**', '*.rb')
      files = Dir.glob(path)

      @source_files = files.map { |x| SourceFile.new x  }

      map = Hash.new { |h, k| h[k] = [] }
      @source_files.each { |s| s.requires.each { |r| map[r] << s } }
      
      map.each_key do |k|
        find_matching_source_files(k).each { |f| map[k] << f }
      end
      
      map.each do |r, s|
        t = s.shift
        s.each { |o| t.merge(o) }
      end
      
      propogate_moved
    end

    #
    # Moved Propogation:
    # 1) If a construct moved, then all its children have moved, and
    # 2) If a construct moved, and all its sibilings have moved,
    #    then it can be considered as being moved
    # We don't care about (1) -- because if a construct has moved
    # we don't print it or its children.
    # (2) is handled here.
    #
    def propogate_moved
      @source_files.each { |x| x.constructs.each { |y| propogate_moved1 y } }
    end

    def propogate_moved1 c
      if c.moved
        c.mark_as_moved
      elsif c.children.empty?
        # do nothing
      elsif c.children.all? { |x| x.moved }
        c.moved = true
      else
        c.children.each { |x| propogate_moved1 x }
        c.moved = true if c.children.all? { |x| x.moved }
      end
    end

    def find_matching_source_files f_required
      f_required += '.rb' if /\.rb$/ !~ f_required
      re = Regexp.new(Regexp.quote(f_required) + '$')
      @source_files.select { |x| re =~ x.path }
    end

    def pp
      @source_files.each { |s| s.pp }
    end
  end
end



#
# Command-line stuff:
#

def opt? name
  if ARGV.member?(name)
    ARGV.delete(name)
    true
  else
    false 
  end
end

def print_tokens path
  tokenizer = Syntax.load 'ruby'
  tokenizer.tokenize(File.read(path)) do |t|
    puts "#{t.inspect} #{t.group}"
  end
end


def main
  include Visualize
  lib = opt?('-lib')
  rec = opt?('-r')
  
  if lib || rec
    if lib
      require 'rbconfig'
      dir = Config::CONFIG['rubylibdir']
    else
      dir = (ARGV[0] || '.')
    end
      
    Dir.glob(File.join(dir, '**', '**', '*.rb')) do |name|
      puts '=' * 78, "[#{name}]"
      begin
        SourceFile.new(name).pp
      rescue => err
        puts err.backtrace.join("\n")
        exit
        puts '*** error with file'
      end
    end

  elsif opt?('-pt')
    print_tokens(ARGV[0] || $0)

  else
    (ARGV.empty? ? [$0] : ARGV).each do |x|
      if File.directory?(x)
        SourcePackage.new(x).pp
      else
        SourceFile.new(x).pp
      end
    end
  end
end


if $0 == __FILE__
  main
end
