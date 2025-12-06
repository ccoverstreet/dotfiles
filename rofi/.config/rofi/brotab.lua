#!/usr/bin/lua


file = io.open("example.txt", "a")
file:write("ASD\n")

local bp = require("lua-shepi")
local rofi_markup = print("markup-row\ntrue\n")


local function GetTabs(sin, sout, serr)
    local delim = print("delim")
    local markup_s = '%s\n%s\n%s'
    local pipe_in = sin:read("a")
	file:write(pipe_in)
    for line in pipe_in:gmatch("([^\n]*)\n") do
		file:write(line)
        local id, title, url = line:match("([^\t]+)\t([^\t]+)\t([^\t]+)")
        local result = string.format(markup_s, title, url:match("https?://[www%.]*(.*)"), id)
        local prune_s = result:gsub("&", "&amp;")
        sout:write(prune_s)
    end
end

args = {...}
local pipe = bp.bt("list") | bp.tac("-s", "\n") | bp.fun(GetTabs)
if not args[1] then

	--file:write(bp.bt("list"))
	--file:write("after pipe\n")
    --io.write(pipe())
else
    os.execute(string.format("bt activate %s", args[1]:match("<i>(.-)</i>")))
end

