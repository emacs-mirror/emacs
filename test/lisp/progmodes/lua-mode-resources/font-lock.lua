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
for i=0,9 do end
--  ^ font-lock-variable-name-face

-- Constant
::label2::
-- ^ font-lock-constant-face
goto label2
--   ^ font-lock-constant-face

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

-- Variable
function fn(x, y) end
--          ^ font-lock-variable-name-face
--             ^ font-lock-variable-name-face
