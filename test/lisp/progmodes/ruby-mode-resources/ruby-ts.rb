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

# Local Variables:
# mode: ruby-ts
# End:
