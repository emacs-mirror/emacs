variable = foo(
  [
    qwe
  ], [
    rty
  ], {
    a: 3
  }
)

tee = [
  qwe
]

qux = [1,
       2]

att = {a: 1,
       b: 2}

a = 1 ? 2 :(
  2 + 3
)

unless bismark
  sink += 12
else
  dog = 99
end

foo1 =
  subject.update(
    1
  )

foo2 =
  subject.
    update(
      # Might make sense to indent this to 'subject' instead; but this
      # style seems more popular.
      2
    )

foo > bar &&
  tee < qux

1 .. 2 &&
     3

a = foo(j, k) -
    bar_tee

qux = foo.fee ?
        bar :
        tee

with_paren = (a + b *
                  c * d +
              12)

without_paren = a + b *
                    c * d +
                12

{'a' => {
   'b' => 'c',
   'd' => %w(e f)
 }
}

[1, 2, {
   'b' => 'c',
   'd' => %w(e f)
 }
]

foo(a, {
      a: b,
      c: d
    })

foo(foo, bar:
    tee)

foo(foo, :bar =>
    tee)

foo = %w[
  asd
]

# Local Variables:
# mode: ruby-ts
# ruby-after-operator-indent: t
# ruby-block-indent: t
# ruby-method-call-indent: t
# ruby-method-params-indent: t
# End:
