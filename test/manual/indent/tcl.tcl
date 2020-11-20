# Some sample code that tries to exercise the font-lock
# of various forms of writing strings.

puts "hello}"; # Top-level strings can contain unescaped closing braces!

puts a"b;                  # Non-delimited strings can contain quotes!
puts a""b;                 # Even several of them!

proc foo1 {} {
    puts "hello";   # Normal case!
    puts "hello\};  # This will signal an error when `foo1` is called!
}

proc foo2 {} {
    puts "hello; # This will also signal an error when `foo2` is called!
}

proc foo3 {} {
    puts a"b;                   # This will not signal an error!
    puts a""b";                 # And that won't either!
    puts "a""b";                # But this will!
}

# FIXME: The [..] interpolation within "..." strings is not properly
# handled by the current `syntax-propertize-function`!
set a "Testing: [split "192.168.1.1/24" "/"] address";
