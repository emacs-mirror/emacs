--[[
This is a
comment block.
]]
local function fun ()
    print("fun")
end
local f = (function ()
    print(1)
end)
for i = 1, 10 do
  print(i)
end
repeat
    print("repeat")
until false
while true do
    print("while")
end
do
    print(1)
end
local t = {
    a=1,
    b=2,
}
if true then
    print(1)
elseif false then
    print(0)
else
    print(0)
end
function f1 (has,
             lots,
             of,
             parameters)
    print("ok")
end
print(1,
      2,
      3,
      4)
