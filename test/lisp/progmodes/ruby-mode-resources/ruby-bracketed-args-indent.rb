foo
  .update({
    key => value,
    other_key:
  }, {
    key => value,
    other_key:
  })

update([
  1,
  2
], [
  3,
  4
])

update([{
  key: "value"
}, {
  key: "value"
}])

update(arg1, {
  foo: "bar"
}, [
  1,
  2
], arg2)

def foo
  foo.update(
    {
      key => value,
      other_key: foo
    }
  )
end

some_method(arg, include: [
  :value1,
  :value2
])

some_method(arg, options: {
  key: "value"
})

some_method(arg, :include => [
  :value1,
  :value2
])

some_method(arg, :options => {
  :key => "value"
})

# Local Variables:
# ruby-bracketed-args-indent: nil
# End:
