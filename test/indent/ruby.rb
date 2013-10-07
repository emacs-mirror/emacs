if something_wrong?             # ruby-move-to-block-skips-heredoc
  ActiveSupport::Deprecation.warn(<<-eowarn)
  boo hoo
  end
  eowarn
end

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

bar(class: XXX) do              # ruby-indent-keyword-label
  foo
end
bar

foo = [1,                       # ruby-deep-indent
       2]

foo = {                         # ruby-deep-indent-disabled
  a: b
}

foo = [                         # ruby-deep-indent-disabled
  1
]

foo(                            # ruby-deep-indent-disabled
  a
)

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

# Example from http://www.ruby-doc.org/docs/ProgrammingRuby/html/language.html
d = 4 + 5 +      # no '\' needed
    6 + 7

# Example from http://www.ruby-doc.org/docs/ProgrammingRuby/html/language.html
e = 8 + 9   \
    + 10         # '\' needed

begin
  foo
ensure
  bar
end

# Bug#15369
MSG = 'Separate every 3 digits in the integer portion of a number' +
      'with underscores(_).'

class C
  def foo
    self.end
    D.new.class
  end
end

a = foo(j, k) -
    bar_tee

while a < b do # "do" is optional
  foo
end

desc "foo foo" \
     "bar bar"

foo.
  bar

# FIXME: is this really valid Ruby?  Isn't the newline after "foo" treated as
# an implicit semi-colon?
foo
  .bar
