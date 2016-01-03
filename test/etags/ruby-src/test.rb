module ModuleExample
    class ClassExample
        def class_method
            puts "in class_method"
        end
        def ClassExample.singleton_class_method
            puts "in singleton_class_method"
        end
        def class_method_exclamation!
            puts "in class_method_exclamation!"
        end
        def class_method_question?
            puts "in class_method_question?"
        end
        def class_method_equals=
            puts "in class_method_equals="
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
    def module_method
        puts "in module_method"
    end
    def ModuleExample.singleton_module_method
        puts "in singleton_module_method"
    end
end

ModuleExample::ClassExample.singleton_class_method
