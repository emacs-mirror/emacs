Rectangle = {}
function Rectangle.getPos ()
end

Circle = {}
function Circle.getPos ()
end

Cube = {}
function Cube.data.getFoo ()
end

Square = {}
function Square.something:Bar ()
end

-- Comment line
  -- Indented comment line

test = {}

   function test.me_22a(one, two)
	print"me_22a"
   end
   local function test.me22b (one)
	print"me_22b"
   end


 test.i_123 = function  (x)
	print"i_123"
end


test.me_12a(1,2)
test.i_123(1)
