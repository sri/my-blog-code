require 'fileutils'

DOWNLOADS = %w{
  http://auxilii.com/msgparser/download/msgparser-1.6.zip
  http://www.freeutils.net/source/jtnef/jtnef-1_6_0.zip
  http://apache.ziply.com/poi/release/bin/poi-bin-3.6-20091214.tar.gz
  http://www.trieuvan.com/apache/ant/binaries/apache-ant-1.8.0-bin.zip }

JCLASS = 'MyMsgParser'
JAVA_INPUT = 'My.msg'
JAVA = <<END
  import java.util.*;
  import com.auxilii.msgparser.*;
  import com.auxilii.msgparser.attachment.*;
  public class #{JCLASS} {
    public static void main(String[] args) throws Exception{
      MsgParser msgp = new MsgParser();
      Message msg = msgp.parseMsg("#{JAVA_INPUT}");
      // UNUSED
      String fromEmail = msg.getFromEmail();
      String fromName = msg.getFromName();
      String subject = msg.getSubject();
      String body = msg.getBodyText();
      String longString = msg.toLongString();
      // We modify source of msgparser to make
      // properties public
      Iterator it = msg.properties.keySet().iterator();
      while (it.hasNext()) {
  	    String key = (String) it.next();
  	    Object val = msg.properties.get(key);
        System.out.println(key + ": " + val);
      }
    }
  }
END

class MsgExtractor
  def initialize(msgs_dir)
    @msgs_dir = msgs_dir
    @cp = '.:msgparser-1.6/dist/msgparser-1.6.jar:' + 
          'poi-3.6/poi-3.6-20091214.jar:lib/tnef.jar'
    download
    javac
  end
  
  def download
    DOWNLOADS.each do |u|
      b = File.basename(u)
      if !File.exists?(b)
        puts "downloading #{u}"
        `curl --silent -O #{u}` unless File.exists?(b)
      end
      next if File.exists?("#{b}.zipped")
      puts "unpacking #{b}"
      case b
      when /zip$/; `unzip -o #{b}`
      when /gz$/;  `gzip -dc #{b} | tar -xpvf -`
      end
      FileUtils.touch("#{b}.zipped")    end
  end
  
  def javac
    modify_msgparser
    @cp.split(':').each do |f|
      abort("#{f} doesn't exist") unless File.exists?(f) 
    end
    open("#{JCLASS}.java", "w") { |f| f.write(JAVA) }
    `javac -classpath #{@cp} #{JCLASS}.java`
  end
  
  def modify_msgparser
    jp = 'msgparser-1.6/src/main/java/com/auxilii/msgparser/Message.java'
    old1 = 'protected Map<String,Object> properties ' + 
           '= new HashMap<String,Object>();'
    new1 = old1.dup
    new1['protected'] = 'public'
    modified = File.read(jp)
    return if modified[new1]
    modified[old1] = new1
    puts "modifying #{jp}..."
    open(jp, 'w') { |f| f.write(modified) }
    FileUtils.cd('msgparser-1.6') do
      puts "rebuilding msgparser"
      `../apache-ant-1.8.0/bin/ant jar`
      FileUtils.mv 'dist/msgparser.jar', 'dist/msgparser-1.6.jar'
    end
  end
  
  def run(&extract_by)
    result = []
    Dir[File.join(@msgs_dir, '*.msg')].each do |f|
      b = File.basename(f)[0..-5]
      puts "extracting #{b}"
      FileUtils.cp(f, JAVA_INPUT)
      out = `java -classpath #{@cp} #{JCLASS}`
      data = extract_by.call(out)
      next if data.nil?
      break if $INTERRUPTED
      result << [b, data]
    end
    result.sort
  end
end

$INTERRUPTED = false

def main
  unless ARGV[0]
    abort("Usage: ruby msgs_extractor.rb msgs_dir")
  end
  tmp = ARGV[1] || "msgs_extractor"
  Dir.mkdir tmp rescue nil
  msgs_dir = File.expand_path ARGV[0]
  Dir.chdir tmp
  extractor = MsgExtractor.new(msgs_dir)
  trap("INT") { $INTERRUPTED = true; puts; }
  result = extractor.run { |o| o.scan /\d{7,20}/ } # extract phone numbers
  result.each { |n, ph| puts("%20s: %s" % [n, ph.join(", ")]) }
end

if $0 == __FILE__
  main
end
