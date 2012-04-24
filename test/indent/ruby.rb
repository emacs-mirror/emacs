# Don't mis-match "sub" at the end of words.
a = asub / aslb + bsub / bslb;

b = %Q{This is a "string"}
c = %w(foo
 bar
 baz)
d = %!hello!

# A "do" after a slash means that slash is not a division, but it doesn't imply
# it's a regexp-ender, since it can be a regexp-starter instead!
x = toto / foo; if /do bar/ then
                  toto = 1
                end

# Some Cucumber code:
Given /toto/ do
  print "hello"
end
