module ModuleExample
    class ClassExample
        def instance_method
            puts "in instane_method"
        end
        def ClassExample.class_method
            puts "in class_method"
        end
        def instance_method_exclamation!
            puts "in instance_method_exclamation!"
        end
        def instance_method_question?
            puts "in instance_method_question?"
        end
        def instance_method_equals=
            puts "in instance_method_equals="
        end
        def `(command)
            return "just testing a backquote override"
        end
        def +(y)
            @x + y
        end
        def [](y)
            @ary[y]
        end
        def []=(y, val)
            @ary[y] = val
        end
        def <<(y)
            @x << y
        end
        def ==(y)
            @ary.length == y.ary.length
        end
        def <=(y)
            '@ary.length < y.ary.length'
        end
        def <=>(y)
            nil
        end
        def ===(y)
            self == y
        end
    end
    def module_instance_method
        puts "in module_instance_method"
    end
    def ModuleExample.module_class_method
        puts "in module_class_method"
    end
end

ModuleExample::ClassExample.class_method

# Local Variables:
# ruby-indent-level: 4
# End:
