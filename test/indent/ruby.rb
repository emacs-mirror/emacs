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

# Some Cucumber code:
Given /toto/ do
  print "hello"
end
