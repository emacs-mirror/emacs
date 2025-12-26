#!/usr/bin/env lua
-- ^ font-lock-comment-face
-- Comment
-- <- font-lock-comment-delimiter-face
-- ^ font-lock-comment-face
--[[
-- ^ font-lock-comment-face
Multi-line comment
-- ^ font-lock-comment-face
]]
-- <- font-lock-comment-face
local line_comment = "comment" -- comment
--                                ^ font-lock-comment-face
---@alias MyNumber integer
-- <- font-lock-comment-delimiter-face
------Calculate new number
-- ^ font-lock-comment-delimiter-face
function calc() end

-- Definition
local function f1() end
--             ^ font-lock-function-name-face
local f2 = function() end
--    ^ font-lock-function-name-face
local tb = { f1 = function() end }
--           ^ font-lock-function-name-face
function tb.f2() end
--          ^ font-lock-function-name-face
function tb:f3() end
--          ^ font-lock-function-name-face
tbl.f4 = function() end
--  ^ font-lock-function-name-face
function x.y:z() end
--           ^ font-lock-function-name-face

-- Keyword
if true then
-- <- font-lock-keyword-face
--      ^ font-lock-keyword-face
elseif true then
-- <- font-lock-keyword-face
else end
-- <- font-lock-keyword-face
--   ^ font-lock-keyword-face
local p = {}
-- ^ font-lock-keyword-face
for k,v in pairs({}) do end
-- <- font-lock-keyword-face
--      ^ font-lock-keyword-face
repeat if true then break end until false
-- <- font-lock-keyword-face
--                  ^ font-lock-keyword-face
--                            ^ font-lock-keyword-face
while true do end
-- <- font-lock-keyword-face
--         ^ font-lock-keyword-face
function fn() return true end
-- <- font-lock-keyword-face
--            ^ font-lock-keyword-face
goto label1
-- ^ font-lock-keyword-face
::label1::
if true and not false or nil then
--      ^ font-lock-keyword-face
--          ^ font-lock-keyword-face
--                    ^ font-lock-keyword-face
end

-- String
local _
_ = "x"
--   ^ font-lock-string-face
_ = 'x'
--   ^ font-lock-string-face
_ = "x\ty"
--   ^ font-lock-string-face
--      ^ font-lock-string-face
_ = "x\"y"
--   ^ font-lock-string-face
--      ^ font-lock-string-face
_ = 'x\'y'
--   ^ font-lock-string-face
--      ^ font-lock-string-face
_ = "x\z
    y"
--  ^ font-lock-string-face
_ = "x\0900y"
--        ^ font-lock-string-face
_ = "x\09y"
--       ^ font-lock-string-face
_ = "x\0y"
--      ^ font-lock-string-face
_ = "x\u{1f602}y"
--             ^ font-lock-string-face
_ = [[x]]
--    ^ font-lock-string-face
_ = [=[x]=]
--     ^ font-lock-string-face

-- Assignment
local n = 0
--    ^ font-lock-variable-name-face
o, p, q = 1, 2, 3
-- <- font-lock-variable-name-face
-- ^ font-lock-variable-name-face
--    ^ font-lock-variable-name-face
tbl[k] = "A"
--  ^ font-lock-variable-name-face
tbl.x = 1
--  ^ font-lock-variable-name-face
for i=0,9 do end
--  ^ font-lock-variable-name-face

-- Constant
local x <const> = 1
--      ^ font-lock-constant-face
local f <close> = io.open('/file')
--      ^ font-lock-constant-face
local a, b, c = true, false, nil
--              ^ font-lock-constant-face
--                    ^ font-lock-constant-face
--                           ^ font-lock-constant-face
::label2::
-- ^ font-lock-constant-face
goto label2
--   ^ font-lock-constant-face

-- Number
n = 123
--  ^ font-lock-number-face
print(99)
--    ^ font-lock-number-face
print(tbl[1])
--        ^ font-lock-number-face

-- Bracket
local t = {}
--        ^ font-lock-bracket-face
--         ^ font-lock-bracket-face
print(t[1])
--   ^ font-lock-bracket-face
--     ^ font-lock-bracket-face
--       ^ font-lock-bracket-face
--        ^ font-lock-bracket-face

-- Builtin
assert()
-- <- font-lock-builtin-face
bit32()
-- <- font-lock-builtin-face
collectgarbage()
-- <- font-lock-builtin-face
coroutine()
-- <- font-lock-builtin-face
debug()
-- <- font-lock-builtin-face
dofile()
-- <- font-lock-builtin-face
error()
-- <- font-lock-builtin-face
getmetatable()
-- <- font-lock-builtin-face
io()
-- <- font-lock-builtin-face
ipairs()
-- <- font-lock-builtin-face
load()
-- <- font-lock-builtin-face
loadfile()
-- <- font-lock-builtin-face
math()
-- <- font-lock-builtin-face
next()
-- <- font-lock-builtin-face
os()
-- <- font-lock-builtin-face
package()
-- <- font-lock-builtin-face
pairs()
-- <- font-lock-builtin-face
pcall()
-- <- font-lock-builtin-face
print()
-- <- font-lock-builtin-face
rawequal()
-- <- font-lock-builtin-face
rawget()
-- <- font-lock-builtin-face
rawlen()
-- <- font-lock-builtin-face
rawset()
-- <- font-lock-builtin-face
require()
-- <- font-lock-builtin-face
select()
-- <- font-lock-builtin-face
setmetatable()
-- <- font-lock-builtin-face
string()
-- <- font-lock-builtin-face
table()
-- <- font-lock-builtin-face
tonumber()
-- <- font-lock-builtin-face
tostring()
-- <- font-lock-builtin-face
type()
-- <- font-lock-builtin-face
utf8()
-- <- font-lock-builtin-face
warn()
-- <- font-lock-builtin-face
xpcall()
-- <- font-lock-builtin-face
print(_G)
--    ^ font-lock-builtin-face
print(_VERSION)
--    ^ font-lock-builtin-face
f.close()
-- ^ font-lock-builtin-face
f.flush()
-- ^ font-lock-builtin-face
f.lines()
-- ^ font-lock-builtin-face
f.read()
-- ^ font-lock-builtin-face
f.seek()
-- ^ font-lock-builtin-face
f.setvbuf()
-- ^ font-lock-builtin-face
f.write()
-- ^ font-lock-builtin-face

-- Delimiter
t = { 1, 2 };
--     ^ font-lock-delimiter-face
--          ^ font-lock-delimiter-face

-- Escape
_ = "x\ty"
--    ^ font-lock-escape-face
--     ^ font-lock-escape-face
_ = "x\"y"
--    ^ font-lock-escape-face
--     ^ font-lock-escape-face
_ = 'x\'y'
--    ^ font-lock-escape-face
--     ^ font-lock-escape-face
_ = "x\z
    y"
-- <- font-lock-escape-face
_ = "x\x5Ay"
--     ^ font-lock-escape-face
--      ^ font-lock-escape-face
_ = "x\0900y"
--       ^ font-lock-escape-face
_ = "x\09y"
--      ^ font-lock-escape-face
_ = "x\0y"
--     ^ font-lock-escape-face
_ = "x\u{1f602}y"
--     ^ font-lock-escape-face
--         ^ font-lock-escape-face

-- Function
func_one()
--  ^ font-lock-function-call-face
tbl.func_two()
--  ^ font-lock-function-call-face
tbl:func_three()
--  ^ font-lock-function-call-face
tbl.f = f4()
--      ^ font-lock-function-call-face

-- Operator
local a, b = 1, 2
--         ^ font-lock-operator-face
print(a & b)
--      ^ font-lock-operator-face
print(a | b)
--      ^ font-lock-operator-face
print(a ~ b)
--      ^ font-lock-operator-face
print(a << 1)
--      ^ font-lock-operator-face
--       ^ font-lock-operator-face
print(a >> 1)
--      ^ font-lock-operator-face
--       ^ font-lock-operator-face
print(a+b-a*b/a%b^a//b)
--     ^ font-lock-operator-face
--       ^ font-lock-operator-face
--         ^ font-lock-operator-face
--           ^ font-lock-operator-face
--             ^ font-lock-operator-face
--               ^ font-lock-operator-face
--                 ^ font-lock-operator-face
print(#t)
--    ^ font-lock-operator-face
print("h".."at")
--       ^ font-lock-operator-face
print(a==b)
--     ^ font-lock-operator-face
print(a~=b)
--     ^ font-lock-operator-face
print(a<=b)
--     ^ font-lock-operator-face
print(a>=b)
--     ^ font-lock-operator-face
print(a<b)
--     ^ font-lock-operator-face
print(a>b)
--     ^ font-lock-operator-face
function ff(...) end
--          ^ font-lock-operator-face

-- Property
t = { a=1 }
--    ^ font-lock-property-name-face
print(t.a)
--      ^ font-lock-property-use-face

-- Punctuation
tbl.f2()
-- ^ font-lock-punctuation-face
tbl:f3()
-- ^ font-lock-punctuation-face

-- Variable
function fn(x, y) end
--          ^ font-lock-variable-name-face
--             ^ font-lock-variable-name-face
fn(a, b)
-- ^ font-lock-variable-use-face
--    ^ font-lock-variable-use-face
print(a + b)
--    ^ font-lock-variable-use-face
--        ^ font-lock-variable-use-face
print(t[a])
--      ^ font-lock-variable-use-face
tbl.f1(p)
--     ^ font-lock-variable-use-face
tbl:f2(q)
--     ^ font-lock-variable-use-face
