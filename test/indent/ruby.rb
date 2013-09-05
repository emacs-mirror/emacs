# Percent literals.
b = %Q{This is a "string"}
c = %w!foo
 bar
 baz!
d = %(hello (nested) world)

# Don't propertize percent literals inside strings.
"(%s, %s)" % [123, 456]

# Or inside comments.
x = # "tot %q/to"; =
  y = 2 / 3

# Regexp after whitelisted method.
"abc".sub /b/, 'd'

# Don't mis-match "sub" at the end of words.
a = asub / aslb + bsub / bslb;

# Highlight the regexp after "if".
x = toto / foo if /do bar/ =~ "dobar"

# Multiline regexp.
/bars
 tees # toots
 nfoos/

def test1(arg)
  puts "hello"
end

def test2 (arg)
  a = "apple"

  if a == 2
    puts "hello"
  else
    puts "there"
  end

  if a == 2 then
    puts "hello"
  elsif a == 3
    puts "hello3"
  elsif a == 3 then
    puts "hello3"
  else
    puts "there"
  end

  case a
  when "a"
    6
  # Support for this syntax was removed in Ruby 1.9, so we
  # probably don't need to handle it either.
  # when "b" :
  #   7
  # when "c" : 2
  when "d"  then 4
  else 5
  end
end

# Some Cucumber code:
Given /toto/ do
  print "hello"
end

# Bug#15208
if something == :==
  do_something
end
