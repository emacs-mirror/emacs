foo
  .asdasd
  .proc do |**args|
    p(**args)
  end

foo
  .asdasd
  .proc { |**args|
    p(**args)
  }

bar.foo do
  bar
end

bar.foo(tee) do
  bar
end

bar.foo(tee) {
  bar
}

x.foo do
  foo
end.bar do
  bar
end

# Local Variables:
# ruby-block-indent: nil
# End:
