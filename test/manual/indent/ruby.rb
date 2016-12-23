if something_wrong?             # ruby-move-to-block-skips-heredoc
  ActiveSupport::Deprecation.warn(<<-eowarn)
  boo hoo
  end
  eowarn
  foo

  foo(<<~squiggly)
  end
  squiggly
end

def foo
  %^bar^
end

# Percent literals.
b = %Q{This is a "string"}
c = %w!foo
 bar
 baz!
d = %(hello (nested) world)

# Don't propertize percent literals inside strings.
"(%s, %s)" % [123, 456]

"abc/#{ddf}ghi"
"abc\#{ddf}ghi"

# Or inside comments.
x = # "tot %q/to"; =
  y = 2 / 3

# Regexp after whitelisted method.
"abc".sub /b/, 'd'

# Don't mis-match "sub" at the end of words.
a = asub / aslb + bsub / bslb;

# Highlight the regexp after "if".
x = toto / foo if /do bar/ =~ "dobar"

# Regexp options are highlighted.

/foo/xi != %r{bar}mo.tee

foo { /"tee/
  bar { |qux| /'fee"/ }         # bug#20026
}

bar(class: XXX) do              # ruby-indent-keyword-label
  foo
end
bar

foo = [1,                       # ruby-deep-indent
       2]

foo = {                         # ruby-deep-indent-disabled
  a: b
}

foo = { a: b,
        a1: b1
      }

foo({                           # bug#16118
      a: b,
      c: d
    })

bar = foo(
  a, [
    1,
  ],
  :qux => [
    3
  ])

foo(
  [
    {
      a: b
    },
  ],
  {
    c: d
  }
)

foo([{
       a: 2
     },
     {
       b: 3
     },
     4
    ])

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

  b = case a
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

  return false unless method == :+
  x = y + z # Bug#16609

  a = 1 ? 2 :(
    2 + 3
  )
end

# Bug#17097
if x == :!=
  something
end

qux :+,
    bar,
    :[]=,
    bar,
    :a

b = $:
c = ??

# Example from http://www.ruby-doc.org/docs/ProgrammingRuby/html/language.html
d = 4 + 5 +      # no '\' needed
    6 + 7

# Example from http://www.ruby-doc.org/docs/ProgrammingRuby/html/language.html
e = 8 + 9   \
    + 10         # '\' needed

foo = obj.bar { |m| tee(m) } +
      obj.qux { |m| hum(m) }

begin
  foo
ensure
  bar
end

# Bug#15369
MSG = 'Separate every 3 digits in the integer portion of a number' \
      'with underscores(_).'

class C
  def foo
    self.end
    D.new.class
  end

  def begin
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

# https://github.com/rails/rails/blob/17f5d8e062909f1fcae25351834d8e89967b645e/activesupport/lib/active_support/time_with_zone.rb#L206
foo # comment intended to confuse the tokenizer
  .bar

z = {
  foo: {
    a: "aaa",
    b: "bbb"
  }
}

foo if
  bar

fail "stuff" \
  unless all_fine?

if foo?
  bar
end

method arg1,                   # bug#15594
       method2 arg2,
               arg3

method? arg1,
        arg2

method! arg1,
        arg2

method !arg1,
       arg2

method [],
       arg2

method :foo,
       :bar

method (a + b),
       c, :d => :e,
       f: g

desc "abc",
     defg

it "is a method call with block" do |asd|
  foo
end

it("is too!") {
  bar
    .qux
}

and_this_one(has) { |block, parameters|
  tee
}

if foo &&
   bar
end

foo +
  bar

foo and
  bar

foo > bar &&
  tee < qux

zux do
  foo == bar &&
    tee == qux

  a = 3 and
    b = 4
end

foo + bar ==
  tee + qux

1 .. 2 &&
     3

3 < 4 +
    5

10 << 4 ^
  20

100 + 2 >>
  3

2 ** 10 /
  2

foo ^
  bar

foo_bar_tee(1, 2, 3)
  .qux&.bar
  .tee.bar
  &.tee

foo do
  bar
    .tee
end

def bar
  foo
    .baz
end

abc(foo
      .bar,
    tee
      .qux)

# http://stackoverflow.com/questions/17786563/emacs-ruby-mode-if-expressions-indentation
tee = if foo
        bar
      else
        tee
      end

a = b {
  c
}

aa = bb do
  cc
end

foo :bar do
  qux
end

foo do |*args|
  tee
end

bar do |&block|
  tee
end

foo = [1, 2, 3].map do |i|
  i + 1
end

bar.foo do
  bar
end

bar.foo(tee) do
  bar
end

bar.foo(tee) {
  bar
}

bar 1 do
  foo 2 do
    tee
  end
end

foo |
  bar

def qux
  foo ||= begin
            bar
            tee
          rescue
            oomph
          end
end

private def foo
  bar
end

%^abc^
ddd

qux = foo.fee ?
        bar :
        tee

zoo.keep.bar!(
  {x: y,
   z: t})

zoo
  .lose(
    q, p)

a.records().map(&:b).zip(
  foo)

foo1 =
  subject.update(
    1
  )

foo2 =
  subject.
    update(
      2
    )

# FIXME: This is not consistent with the example below it, but this
# offset only happens if the colon is at eol, which wouldn't be often.
# Tokenizing `bar:' as `:bar =>' would be better, but it's hard to
# distinguish from a variable reference inside a ternary operator.
foo(bar:
      tee)

foo(:bar =>
    tee)

regions = foo(
  OpenStruct.new(id: 0, name: "foo") => [
    10
  ]
)

{'a' => {
   'b' => 'c',
   'd' => %w(e f)
 }
}

# Bug#17050

return render json: {
                errors: { base: [message] },
                copying: copying
              },
              status: 400

top test(
      some,
      top,
      test)

foo bar, {
      tee: qux
    }
