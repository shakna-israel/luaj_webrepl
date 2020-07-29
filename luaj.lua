#!/usr/bin/env lua5.3

local load = loadstring or load
local unpack = unpack or table.unpack

local load_source = nil
local make_env = nil

local luaj_version = {0,7,0,'beta'}

local version = function()
	local idx = _VERSION:find(" ") + 1
	return _VERSION:sub(idx, #_VERSION)
end

local luaj_next = function(t, key)
	local m = getmetatable(t)
	local n = m and m.__next or next
	return n(t,key)
end
-- Have pairs be aware of __next
local luaj_pairs = function(t) return luaj_next, t, nil end

-- Have ipairs be aware of __index
local _ipairs = function(t, var)
  var = var + 1
  local value = t[var]
  if value == nil then return end
  return var, value
end
local luaj_ipairs = function(t)
	if t[0] ~= nil then
		return _ipairs, t, -1
	else
		return _ipairs, t, 0
	end
end

-- If this isn't part of the host Lua,
-- add it in a way that is somewhat accurate.
if math.type == nil then
	math.type = function(x)
		if type(x) ~= 'number' then
			return nil
		end

		local i, f = math.modf(x)
		if 0 == f then
			return 'integer'
		else
			return 'float'
		end
	end
end

local is_sequence
local luaj_type = function(o)
	local meta = getmetatable(o)
	if meta ~= nil then
		if meta.__type then
			if type(meta.__type) == 'function' then
				return meta.__type(o)
			else
				return meta.__type
			end
		end
	end

	if math.type ~= nil then
		if math.type(o) ~= nil then
			return math.type(o)
		end
	end

	if io ~= nil then
		if io.type(o) ~= nil then
			return io.type(o)
		end
	end
	
	if type(o) == 'table' and is_sequence(o) then
			return 'sequence'
	else
		return type(o)
	end
end

local table_reverse = function(tbl)
    local size = #tbl
    local newTable = {}
 
    for i,v in ipairs(tbl) do
        newTable[(size-i) + 1] = v
    end
 
    return newTable
end

local string_explode = function(source)
	local ret = {}
	for i=1, #source do
		ret[i] = string.sub(source, i, i)
	end

	return ret
end

local utf8_explode = function(x)
	local ret = {}

	for uchar in string.gmatch(x, utf8.charpattern) do
		ret[#ret+1] = uchar
	end

	return ret
end

local matrix_index = function(self, column, row)
	local c = column - 1
	local r = row - 1

	return self._mtx[c + r * self._col + 1]
end

local iter = function(tbl, options)
	-- Takes an options table instead of sorter:
	-- options.sorter or '<'
	-- options.reverse or false

	local meta = getmetatable(tbl)

	-- Allow iterating over strings as well...
	if type(tbl) == 'string' then
		tbl = string_explode(tbl)
	-- Allow iterating over UTF8 strings as well...
	elseif meta ~= nil and meta.__type == 'utf8' then
		tbl = tbl:explode()
	-- Allow iterating over a matrix as well...
	elseif meta ~= nil and meta.__type == 'matrix' then
		local tmp = {}
		for r=1, tbl._row do
			for c=1, tbl._col do
				tmp[#tmp + 1] = {column = c, row = r, value = matrix_index(tbl, c, r)}
			end
		end
		tbl = tmp
	end

	if options == nil then
		options = {}
	end

	local sorter
	if not options.sorter then
		sorter = function(a, b) return a < b end
	else
		sorter = options.sorter
	end

	local keys = {}
	for k, _ in luaj_pairs(tbl) do
		keys[#keys + 1] = k
	end
	table.sort(keys, sorter)

	if options.reverse then

		local index = #keys + 1
		local count = 0

		return function()
			index = index - 1
			if index >= count then
				return keys[index], tbl[keys[index]]
			end
		end
	end

	local index = 0
	local count = #keys

	return function()
		index = index + 1
		if index <= count then
			return keys[index], tbl[keys[index]]
		end
	end
end

local value_iterator = function(tbl, options)
	if options == nil then
		options = {}
	end

	if options.reverse then
		local index = #tbl
		local count = 0

		return function()
			index = index - 1
			if index >= count then
				return index + 1, tbl[index + 1]
			end
		end

	else
		return luaj_ipairs(tbl)
	end
end

is_sequence = function(tbl)
	if #tbl == 0 then
		return false
	end

	local i = 1
	for idx, _ in iter(tbl) do
		if math.type(idx) ~= 'integer' then
			return false
		end
		if idx ~= i then
			return false
		end
		i = i + 1
	end
	return true
end

local datapath = function()
	local path_sep = package.config:sub(1,1)
	if path_sep == '/' then
		-- XDG

		local exists = function(file)
		   local ok, err, code = os.rename(file, file)
		   if not ok then
		      if code == 13 then
		         -- Permission denied, but it exists
		         return true
		      end
		   end

		   return ok, err
		end
		
		local dpath = os.getenv("XDG_DATA_HOME")
		if dpath == nil or #dpath < 1 or not exists(dpath .. path_sep) then
			-- $HOME/.local/share
			dpath = os.getenv("HOME") .. path_sep .. ".local" ..
			    path_sep .. "share" .. path_sep
			if not exists(dpath) then
				-- $HOME/share
				dpath = os.getenv("HOME") .. path_sep .. "share" .. path_sep
				if not exists(dpath) then
					return false
				end
			end
			return dpath
		end

		-- Check dpath ends with path_sep
		if dpath:sub(#dpath, #dpath) ~= path_sep then
			dpath = dpath .. path_sep
		end
		return depath
	else
		-- %APPDATA%
		local dpath = os.getenv('APPDATA')
		return dpath .. path_sep
	end
end

local dirname = function(name)
	local path_sep = package.config:sub(1,1)
	local index = name:match(string.format('^.*()%s', path_sep))
	-- No path seperator found:
	if index == nil or index > #name then
		return name
	end

	-- Grab the piece of the name we want
	local prename = name:sub(1, index)

	return prename
end

local memoize
do
  local cache = {}
  memoize = function(functor)

  -- TODO: If an argument is a table, try and compare by value as well...

    cache[functor] = {}
    return function(...)
      local cached = false
      local args = {...}

      local data
      -- Check if these arguments are cached.
      for k, v in pairs(cache[functor]) do
        if not cached then
          if #k == #args then
            local same = true
            for ix, arg in ipairs(args) do
              if k[ix] ~= arg then
                same = false
              end
            end
            if same then
              cached = true
              data = v
            end
          end
        end
      end

      if cached then
        return unpack(data)
      else
        local store = {functor(...)}
        cache[functor][args] = store
        return unpack(store)
      end
    end
  end
end



local try = function(functor, arguments, except)
	if arguments == nil then
		arguments = {}
	end

	local data = {pcall(functor, unpack(arguments))}

	if data[1] then
		-- Success, return all values.
		local r = {}
		for i=2, #data do
			r[#r+1] = data[i]
		end
		return unpack(r)
	else
		-- Do exception/s
		-- except can be nil, table<key = callable>, or callable.

		-- Swallow all errors blindly.
		if except == nil then
			return nil
		end

		local meta = getmetatable(except) or {}
		-- Table
		if meta.__call == nil and type(except) == 'table' then
			local error_code
			if type(data[2]) == 'string' then
				-- Extract the code, ignoring the program line/position
				local first = string.find(data[2], ":")
				local second = string.find(data[2], ":", first+1)
				error_code = data[2]:sub(second + 2)
			else
				error_code = data[2]
			end

			-- Unaccounted for error, re-raise at the try-caller level.
			if except[error_code] == nil then
				error(data[2], 2)
			end

			local r = {}
			for i=2, #data do
				r[#r+1] = data[i]
			end
			return except[error_code](unpack(r))
		else
			-- Callable
			local r = {}
			for i=2, #data do
				r[#r+1] = data[i]
			end
			return except(unpack(r))
		end
	end
end

local split = function(s1, pattern)
	if pattern == nil or #pattern == 0 then
		return {s1}
	end

	local t = {}
	local fpat = "(.-)" .. pattern
   	local last_end = 1
   	local s, e, cap = string.find(s1, fpat, 1)
   	while s do
		if s ~= 1 or cap ~= "" then
			table.insert(t, cap)
		end
		last_end = e+1
		s, e, cap = string.find(s1, fpat, last_end)
	end
	if last_end <= #s1 then
		cap = string.sub(s1, last_end)
		table.insert(t, cap)
	end
   	return t
end

local get_locals = function(func)
  -- http://lua-users.org/wiki/DavidManura
  -- http://lua-users.org/wiki/StringInterpolation

  if func == nil then func = 1 end

  local n = 1
  local locals = {}
  func = (type(func) == "number") and func + 1 or func
  while true do
    local lname, lvalue = debug.getlocal(func, n)
    if lname == nil then break end  -- end of list
    if lvalue == nil then lvalue = mynil end  -- replace
    locals[lname] = lvalue
    n = n + 1
  end
  return locals
end

local iformat = function(source, data)
	local value_interp = function(s, tab)
	  -- http://lua-users.org/wiki/RiciLake
	  -- http://lua-users.org/wiki/StringInterpolation
	  return (s:gsub('($%b{})', function(w)
	  	local x = tab[w:sub(3, -2)] or w

	  	-- TODO: handle objects with "no literal form"
	  	if type(x) == 'table' or type(x) == 'userdata' then
	  		return tostring(x)
	  	end

	  	return x
	  end))
	end

	local inner_interp = function(s, tab)
	  -- http://lua-users.org/wiki/RiciLake
	  -- http://lua-users.org/wiki/StringInterpolation
	  return (s:gsub('%%%((%a%w*)%)([-0-9%.]*[cdeEfgGiouxXsq])',
	            function(k, fmt) return tab[k] and ("%"..fmt):format(tab[k]) or
	                '%('..k..')'..fmt end))
	end

	local outer_interp = function(s, tab)
		-- Allow Python style...
		if type(tab) ~= "table" then
			tab = {tab}
		end

		local named_keys = {}
		local named_keys2 = {}

		-- Get locals
		for key, value in pairs(get_locals(2)) do
			if key ~= "(*temporary)" then
				named_keys[key] = value
				named_keys2[key] = value
			end
		end

		-- Split apart the named and numbered keys
		local unnamed_keys = {}
		for key, value in pairs(tab) do
			if not tonumber(key) then
				named_keys[key] = value
				named_keys2[key] = value
			else
				unnamed_keys[key] = value
			end
		end

		-- First pass, "${key}" style.
		local pass = value_interp(s, named_keys)
		-- Second pass, "$(key)s" style.
		pass = inner_interp(pass, named_keys2)

		if #unnamed_keys > 0 then
			-- Last pass, "%d" style
			return string.format(pass, unpack(unnamed_keys))
		else
			return pass or ""
		end
	end

	return outer_interp(source, data)
end

local open_cli = function(root)
	root['cli'] = {}

	local cli = root['cli']

	cli.argparse = {}
	cli.argparse.parser = function(name)
		local r = {}

		local meta = getmetatable(r) or {}
		meta.__index = cli.argparse
		setmetatable(r, meta)

		r.parser_name = name
		r.args = {}
		r.args.positional = {}
		r.args.positional_help = {}
		r.args.bare_flags = {}
		r.args.bare_flags_help = {}
		r.args.arg_flags = {}
		r.args.arg_flags_help = {}

		r.args.mapping = {}

		cli.argparse.toggle(r, "help", "Show help information.")

		return r
	end

	cli.argparse.positional = function(self, name, help)
		self.args.positional[#self.args.positional + 1] = name
		self.args.positional_help[#self.args.positional] = help or ''
	end

	cli.argparse.toggle = function(self, name, help)
		self.args.bare_flags[#self.args.bare_flags + 1] = name

		self.args.bare_flags_help[name] = help or ''

		if self.args.mapping[string.sub(name, 1, 1)] == nil then
			self.args.mapping[string.sub(name, 1, 1)] = {'bare_flags', name}
		end
	end

	cli.argparse.flag = function(self, name, help)
		self.args.arg_flags[#self.args.arg_flags + 1] = name

		self.args.arg_flags_help[name] = help or ''

		if self.args.mapping[string.sub(name, 1, 1)] == nil then
			self.args.mapping[string.sub(name, 1, 1)] = {'arg_flags', name}
		end
	end

	cli.argparse.help = function(self)
		local r = {}
		r[#r + 1] = string.format("USAGE: %s %s [options]", self.parser_name, table.concat(self.args.positional, ", "))

		if #self.args.positional > 0 then
			r[#r+1] = "\nPositional Arguments:"
		end

		for idx, value in ipairs(self.args.positional) do
			r[#r+1] = string.format("%s\n\t%s", value, self.args.positional_help[idx])
		end

		if #self.args.bare_flags > 0 then
			r[#r+1] = "\nToggle Options:"
		end

		for idx, val in ipairs(self.args.bare_flags) do

			local h = self.args.bare_flags_help[val] or ''

			if self.args.mapping[string.sub(val, 1, 1)] and self.args.mapping[string.sub(val, 1, 1)][1] == 'bare_flags' then
				r[#r+1] = string.format("-%s\n\t%s", string.sub(val, 1, 1), h)
			end

			r[#r+1] = string.format("-%s\n\t%s", val, h)
		end

		if #self.args.arg_flags > 0 then
			r[#r+1] = "\nArgument Options:"
		end

		for idx, val in ipairs(self.args.arg_flags) do

			local h = self.args.arg_flags_help[val] or ''

			if self.args.mapping[string.sub(val, 1, 1)] and self.args.mapping[string.sub(val, 1, 1)][1] == 'arg_flags' then
				r[#r+1] = string.format("-%s\n\t%s", string.sub(val, 1, 1), h)
			end

			r[#r+1] = string.format("--%s\n\t%s", val, h)
		end

		return table.concat(r, "\n")
	end

	cli.argparse.parse = function(self, args)
		-- Copy arguments so we have a table we can safely modify
		local targs = {}
		for idx, val in ipairs(args) do
			targs[idx] = val
		end
		local args = targs

		local r = {}

		-- Parse positional
		for idx, val in ipairs(self.args.positional) do
			if args[idx] == nil then
				return false, {'positional', val}
			end
			r[val] = args[idx]
		end

		-- Parse short flags by expanding them
		local replacements = {}
		for key, value in pairs(self.args.mapping) do
			for idx, cell in ipairs(args) do
				if cell == string.format("-%s", key) then

					local rewrite
					if value[1] == 'bare_flags' then
						rewrite = string.format("-%s", value[2])
					else
						rewrite = string.format("--%s", value[2])
					end

					replacements[#replacements+1] = {idx, rewrite}
				end
			end
		end

		-- Expand replacements
		for _, cell in ipairs(replacements) do
			args[cell[1]] = cell[2]
		end

		-- Parse bare flags
		for _, key in ipairs(self.args.bare_flags) do
			for idx, cell in ipairs(args) do
				if cell == string.format("-%s", key) then
					r[key] = true
				end
			end
		end

		-- Parse arg flags
		for _, key in ipairs(self.args.arg_flags) do
			for idx, cell in ipairs(args) do
				if cell == string.format("--%s", key) then
					r[key] = args[idx + 1]
					if r[key] == nil then
						return false, {'arg_flags', key}
					end
				end
			end
		end

		return true, r
	end

	cli.screen = {}

	cli.screen.reset = function()
		io.stdout:write("\x1b[0m")
		io.stdout:flush()
	end

	cli.screen.foreground = function(options)
		-- Usage styles:
		-- {256 = 312}
		-- {color = 'red'}
		-- {r = 26, g = 21, b = 19}

		if options[256] ~= nil then
			io.stdout:write(string.format("%s%d%s", "\x1b[38;5;", options[256], "m"))
		elseif options['color'] then
			local color = options['color']
			if color == 'black' then
				io.stdout:write(string.format("%s", "\x1b[30m"))
			elseif color == 'red' then
				io.stdout:write(string.format("%s", "\x1b[31m"))
			elseif color == 'green' then
				io.stdout:write(string.format("%s", "\x1b[32m"))
			elseif color == 'yellow' then
				io.stdout:write(string.format("%s", "\x1b[33m"))
			elseif color == 'blue' then
				io.stdout:write(string.format("%s", "\x1b[34m"))
			elseif color == 'magenta' then
				io.stdout:write(string.format("%s", "\x1b[35m"))
			elseif color == 'cyan' then
				io.stdout:write(string.format("%s", "\x1b[36m"))
			elseif color == 'white' then
				io.stdout:write(string.format("%s", "\x1b[37m"))
			elseif color == 'bright-black' then
				io.stdout:write(string.format("%s", "\x1b[30;1m"))
			elseif color == 'bright-red' then
				io.stdout:write(string.format("%s", "\x1b[31;1m"))
			elseif color == 'bright-green' then
				io.stdout:write(string.format("%s", "\x1b[32;1m"))
			elseif color == 'bright-yellow' then
				io.stdout:write(string.format("%s", "\x1b[33;1m"))
			elseif color == 'bright-blue' then
				io.stdout:write(string.format("%s", "\x1b[34;1m"))
			elseif color == 'bright-magenta' then
				io.stdout:write(string.format("%s", "\x1b[35;1m"))
			elseif color == 'bright-cyan' then
				io.stdout:write(string.format("%s", "\x1b[36;1m"))
			elseif color == 'bright-white' then
				io.stdout:write(string.format("%s", "\x1b[37;1m"))
			else
			end
		else
			io.stdout:write(string.format("\x1b[38;2;%d;%d;%d", options['r'] or 0, options['g'] or 0, options['b'] or 0))
		end
		io.stdout:flush()
	end

	cli.screen.background = function(options)
		-- Usage styles:
		-- {256 = 312}
		-- {color = 'red'}
		-- {r = 26, g = 21, b = 19}

		if options[256] ~= nil then
			io.stdout:write(string.format("%s%d%s", "\x1b[48;5;", options[256], "m"))
		elseif options['color'] then
			local color = options['color']
			if color == 'black' then
				io.stdout:write(string.format("%s", "\x1b[40m"))
			elseif color == 'red' then
				io.stdout:write(string.format("%s", "\x1b[41m"))
			elseif color == 'green' then
				io.stdout:write(string.format("%s", "\x1b[42m"))
			elseif color == 'yellow' then
				io.stdout:write(string.format("%s", "\x1b[43m"))
			elseif color == 'blue' then
				io.stdout:write(string.format("%s", "\x1b[44m"))
			elseif color == 'magenta' then
				io.stdout:write(string.format("%s", "\x1b[45m"))
			elseif color == 'cyan' then
				io.stdout:write(string.format("%s", "\x1b[46m"))
			elseif color == 'white' then
				io.stdout:write(string.format("%s", "\x1b[47m"))
			elseif color == 'bright-black' then
				io.stdout:write(string.format("%s", "\x1b[40;1m"))
			elseif color == 'bright-red' then
				io.stdout:write(string.format("%s", "\x1b[41;1m"))
			elseif color == 'bright-green' then
				io.stdout:write(string.format("%s", "\x1b[42;1m"))
			elseif color == 'bright-yellow' then
				io.stdout:write(string.format("%s", "\x1b[43;1m"))
			elseif color == 'bright-blue' then
				io.stdout:write(string.format("%s", "\x1b[44;1m"))
			elseif color == 'bright-magenta' then
				io.stdout:write(string.format("%s", "\x1b[45;1m"))
			elseif color == 'bright-cyan' then
				io.stdout:write(string.format("%s", "\x1b[46;1m"))
			elseif color == 'bright-white' then
				io.stdout:write(string.format("%s", "\x1b[47;1m"))
			else
			end
		else
			io.stdout:write(string.format("\x1b[48;2;%d;%d;%d", options['r'] or 0, options['g'] or 0, options['b'] or 0))
		end

		io.stdout:flush()
	end

	cli.effect = function(kind)
		if kind == 'bold' then
			io.stdout:write("\x1b[1m")
		elseif kind == 'underline' then
			io.stdout:write("\x1b[4m")
		elseif kind == 'reverse' then
			io.stdout:write("\x1b[7m")
		elseif kind == 'blink' then
			io.stdout:write("\x1b[5m")
		elseif kind == 'invisible' then
			io.stdout:write("\x1b[8m")
		elseif kind == 'reset' then
			cli.screen.reset()
		else
		end
		io.stdout:flush()
	end

	cli.clear = {}
	cli.clear.screen = function(option)
		if option == 'before' then
			io.stdout:write("\x1b[1J")
		elseif option == 'after' then
			io.stdout:write("\x1b[0J")
		else
			io.stdout:write("\x1b[2J\x1b[1;1H")
		end
		io.stdout:flush()
	end
	cli.clear.line = function(option)
		if option == 'before' then
			io.stdout:write("\x1b[1K")
		elseif option == 'after' then
			io.stdout:write("\x1b[0K")
		else
			io.stdout:write("\x1b[2K")
		end
		io.stdout:flush()
	end

	cli.cursor = {}
	cli.cursor.up = function(num)
		io.stdout:write(string.format("\x1b%dA", num))
		io.stdout:flush()
	end
	cli.cursor.down = function(num)
		io.stdout:write(string.format("\x1b%dB", num))
		io.stdout:flush()
	end
	cli.cursor.left = function(num)
		io.stdout:write(string.format("\x1b%dC", num))
		io.stdout:flush()
	end
	cli.cursor.right = function(num)
		io.stdout:write(string.format("\x1b%dD", num))
		io.stdout:flush()
	end
	cli.cursor.save = function()
		io.stdout:write("\x1bs")
		io.stdout:flush()
	end
	cli.cursor.restore = function()
		io.stdout:write("\x1bu")
		io.stdout:flush()
	end

	local meta = getmetatable(cli) or {}
	meta.__type = "library"
	setmetatable(cli, meta)

	return root
end

local open_contract = function(root)
	root['contract'] = {}

	local contract = root['contract']

	-- Contract enforcer
	contract.assert = function(bool, msg)
		if not bool then
			-- Raise the error where the contract'd function is called.
			error(msg, 3)
		end
	end

	-- Constructor for a generic Nil or particular Type
	contract.NilOrT = function(T)
		return function(obj)
			if obj == nil then
				return true
			elseif contract.match_type(obj, T) then
				return true
			else
				return false
			end
		end
	end

	-- Constructor for a generic Match Any of Ts...
	contract.AnyT = function(...)
		local args = {...}

		return function(obj)
			for idx, T in ipairs(args) do
				if contract.match_type(obj, T) then
					return true
				end
			end
			return false
		end
	end

	-- Constructor for a generic Match None of Ts...
	contract.NoneT = function(...)
		local args = {...}

		return function(obj)
			return not contract.AnyT(unpack(args))(obj)
		end
	end

	-- Constructor to match multiple types on the one object
	contract.AllT = function(...)
		local args = {...}

		return function(obj)
			for idx, T in ipairs(args) do
				if not contract.match_type(obj, T) then
					return false
				end
			end
			return true
		end
	end

	-- Constructor for a ranged number type.
	contract.Range = function(min, max)
		return function(obj)
			if not contract.match_type(obj, "integer") and not contract.match_type(obj, "float") then
				return false
			end

			if obj < min then
				return false
			end

			if obj > max then
				return false
			end

			return true
		end
	end

	-- Constructor to check a given length.
	contract.LengthRange = function(min, max)
		return function(obj)
			if #obj < min then
				return false
			end

			if #obj > max then
				return false
			end

			return true
		end
	end

	-- Constructor to check a given length.
	contract.Length = function(len)
		return function(obj)
			if #obj ~= len then
				return false
			end

			return true
		end
	end

	-- Our type matcher...
	contract.match_type = function(a, b)
		-- Match a standard type...
		if luaj_type(b) == "string" then
			return luaj_type(a) == b
		else
			-- Match one of our, or the user's, type definitions.
			return b(a)
		end
	end

	-- Our contract constructor...
	-- Returns a function wrapped in contracts.
	contract.contract = function(functor, return_type, arg_types)

		if arg_types == nil then
			arg_types = {}
		end

		if return_type == nil then
			return_type = "nil"
		end

		return function(...)

			local args = {...}
			local ass = contract.assert(#args == #arg_types,
				string.format("Received <%d> arguments, but expected <%d> arguments.", #args, #arg_types))
			if ass ~= nil then
				return ass
			end

			for idx, arg_type in ipairs(arg_types) do
				local ass = contract.assert(contract.match_type(args[idx], arg_type),
					string.format("For argument <%d>, expected an argument type of: <%s>, but received <%s>",
						idx, arg_type, luaj_type(args[idx])))
				if ass ~= nil then
					return ass
				end
			end

			-- Package for single return
			if type(return_type) ~= 'table' then
				return_type = {return_type}
			end

			local r = {functor(...)}
			local ass = contract.assert(not (#r > #return_type),
				string.format("Received <%d> return values, but expected <%d> return values.", #r, #return_type))
			if ass ~= nil then
				return ass
			end

			for idx, arg_type in ipairs(return_type) do
				local ass = contract.assert(contract.match_type(r[idx], arg_type),
					string.format("For return value <%d>, expected a value type of: <%s>, but received <%s>",
						idx, arg_type, luaj_type(r[idx])))
				if ass ~= nil then
					return ass
				end
			end

			return unpack(r)
		end
	end

	local meta = getmetatable(contract) or {}
	meta.__type = "library"
	setmetatable(contract, meta)

	return root
end

local open_csv = function(root)
	root['csv'] = {}

	local r = root['csv']

	local parse_csv_line = function(line, sep, destination)
		-- Convert escapes inside strings to numbers
		line = string.gsub(line, '".-"', function(inside_string)
			return (string.gsub(inside_string, "\\(.)", function(x)
				return string.format("\\%03d", string.byte(x))
			end))
		end)

		local instring = false
		local token = {}
		for i=1, #line do
			local c = string.sub(line, i, i)
			if instring then
				if c == '"' then
					instring = false
				else
					token[#token + 1] = c
				end
			else
				if c == '"' then
					instring = true
				elseif c == sep then
					local x = table.concat(token)

					x = (string.gsub(x, "\\(%d%d%d)", function(d)
						return string.char(tonumber(d))
					end))

					destination[#destination+1] = x
					token = {}
				else
					token[#token + 1] = c
				end
			end
		end

		if #token > 0 then
			local x = table.concat(token)

			x = (string.gsub(x, "\\(%d%d%d)", function(d)
				return string.char(tonumber(d))
			end))
			destination[#destination+1] = x
		end
	end

	r.load_file = function(filename, options)
		options = options or {}

		if options.header == nil then
			options.header = true
		end

		options.seperator = options.seperator or ','

		local f = io.open(filename, "r")
		-- Did we open it properly?
		if not f then
			return nil
		end

		local header = {}
		local tbl = {}

		local idx = 1
		for line in f:lines() do
			if idx == 1 and options.header == true then
				parse_csv_line(line, options.seperator, header)
			else
				tbl[#tbl + 1] = {}
				parse_csv_line(line, options.seperator, tbl[#tbl])
			end

			idx = idx + 1
		end

		f:close()

		if type(options.header) == 'table' then
			header = options.header
		end

		local ret = {}

		for row_num, row in ipairs(tbl) do
			ret[#ret + 1] = {}
			for idx, v in ipairs(row) do
				if tonumber(v) ~= nil then
					ret[row_num][header[idx] or idx] = tonumber(v)
				else
					ret[row_num][header[idx] or idx] = v
				end
			end
		end

		return ret
	end

	local split_lines = function(source)
		local lines = {}
		local line = {}

		for i=1, #source do
			local c = string.sub(source, i, i)
			if c == '\n' then
				lines[#lines + 1] = table.concat(line)
				line = {}
			else
				line[#line + 1] = c
			end
		end

		if #line > 0 then
			lines[#lines + 1] = table.concat(line)
		end

		return lines
	end

	r.load_string = function(source, options)
		options = options or {}

		if options.header == nil then
			options.header = true
		end

		options.seperator = options.seperator or ','

		local header = {}
		local tbl = {}

		for idx, line in ipairs(split_lines(source)) do
			if idx == 1 and options.header == true then
				parse_csv_line(line, options.seperator, header)
			else
				tbl[#tbl + 1] = {}
				parse_csv_line(line, options.seperator, tbl[#tbl])
			end
		end

		if type(options.header) == 'table' then
			header = options.header
		end

		local ret = {}

		for row_num, row in ipairs(tbl) do
			ret[#ret + 1] = {}
			for idx, v in ipairs(row) do
				if tonumber(v) ~= nil then
					ret[row_num][header[idx] or idx] = tonumber(v)
				else
					ret[row_num][header[idx] or idx] = v
				end
			end
		end

		return ret
	end

	r.dump_string = function(data, options)
		options = options or {}
		if not options.seperator then
			options.seperator = ','
		end
		if options.header == nil then
			options.header = true
		end

		local keys = {}
		for k, v in pairs(data[1]) do
			keys[#keys+1] = k
		end
		table.sort(keys)

		local r = {}

		if options.header then
			local x = {}
			for _, k in ipairs(keys) do
				x[#x+1] = string.format("%q", k)
			end

			r[#r+1] = table.concat(x, options.seperator)
		end

		for row_num, row in ipairs(data) do
			local x = {}
			for _, key in ipairs(keys) do


				if type(row[key]) == 'number' then
					if math.type(row[key]) == 'integer' then
						-- %d can _rarely_ result in oddness.
						x[#x+1] = string.format("%.0f", row[key])
					else
						-- 0.16f seems to be Lua 5.3's limit.
						local float_rep = string.format("%.16f", row[key])
						x[#x+1] = string.gsub(float_rep, "%.?0+$", "")
					end
				elseif row[key] == nil then
					x[#x+1] = ''
				else
					x[#x+1] = string.format("%q", row[key])
				end
			end
			r[#r+1] = table.concat(x, options.seperator)
		end

		return table.concat(r, '\n')
	end

	local meta = getmetatable(r) or {}
	meta.__type = "library"
	setmetatable(r, meta)

	return root
end

local open_class = function(root)
	root['class'] = {}

	local r = root['class']

	r['Class'] = function(name, ...)
		local c = {}
		local parents = {...}

		-- No name given, create something semi unique
		if name == nil then
			-- Extract the table hex identifier...
			local x = tostring(c)
			name = string.sub(x, string.find(x, ":") + 2)
		end

		-- Populate methods
		setmetatable(c, {__index = function(t, k)
			return (function(k, plist)
				for i=1, #plist do
					local v = plist[i][k]
					if v then return v end
				end
			end)(k, parents)
		end})

		-- Self reference
		c.__index = c

		-- Constructor
		c.new = function(self, o)
			o = o or {}
			setmetatable(o, c)
			local meta = getmetatable(o)

			-- Pass metamethods at construction
			for key, value in pairs(c) do
				if type(key) == 'string' then
					if string.sub(key, 1, 2) == '__' then
						meta[key] = value
					end
				end
			end

			-- Set the object type
			meta.__type = string.format("instance(%s)", name)

			return o
		end

		-- Ability to find parents
		c.parents = function(self)
			return parents
		end

		-- Ability to extract just the class name
		c.class_name = function(self)
			return name
		end

		-- Ability to easily adjust meta methods
		c.meta = function(self)
			local meta = getmetatable(self)
			if meta == nil then
				meta = {}
				setmetatable(self, meta)
			end

			return meta
		end

		-- Set the class type
		getmetatable(c).__type = string.format("class(%s)", name)

		return c
	end

	local meta = getmetatable(root['class']) or {}
	meta.__type = "library"
	setmetatable(root['class'], meta)

	return root
end

local open_base64 = function(root)

	root['base64'] = {}

	local ret = root['base64']

	-- Make sure we have an extract function...
	local extract = bit32 and bit32.extract
	if not extract then
		if bit then
			local shl, shr, band = bit.lshift, bit.rshift, bit.band
			extract = function(v, from, width)
				return band( shr( v, from ), shl( 1, width ) - 1 )
			end
		elseif _G._VERSION >= "Lua 5.3" then
			extract = load[[return function( v, from, width )
            	return ( v >> from ) & ((1 << width) - 1)
        	end]]()
        else
        	extract = function( v, from, width )
	            local w = 0
	            local flag = 2^from
	            for i = 0, width-1 do
	                local flag2 = flag + flag
	                if v % flag2 >= flag then
	                    w = w + 2^i
	                end
	                flag = flag2
	            end
            	return w
        	end
    	end
	end

	local makeencoder = function(s62, s63, spad)
	    local encoder = {}
	    for b64code, char in pairs{[0]='A','B','C','D','E','F','G','H','I','J',
	        'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y',
	        'Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n',
	        'o','p','q','r','s','t','u','v','w','x','y','z','0','1','2',
	        '3','4','5','6','7','8','9',s62 or '+',s63 or'/',spad or'='} do
	        encoder[b64code] = char:byte()
	    end
	    return encoder
	end

	local makedecoder = function(s62, s63, spad)
	    local decoder = {}
	    for b64code, charcode in pairs(makeencoder(s62, s63, spad)) do
	        decoder[charcode] = b64code
	    end
	    return decoder
	end

	local DEFAULT_ENCODER = makeencoder()
	local DEFAULT_DECODER = makedecoder()

	local char, concat = string.char, table.concat

	ret.encode = function(str, encoder, usecaching)
	    encoder = encoder or DEFAULT_ENCODER
	    local t, k, n = {}, 1, #str
	    local lastn = n % 3
	    local cache = {}
	    for i = 1, n-lastn, 3 do
	        local a, b, c = str:byte( i, i+2 )
	        local v = a*0x10000 + b*0x100 + c
	        local s
	        if usecaching then
	            s = cache[v]
	            if not s then
	                s = char(encoder[extract(v,18,6)],
	                	encoder[extract(v,12,6)],
	                	encoder[extract(v,6,6)],
	                	encoder[extract(v,0,6)])
	                cache[v] = s
	            end
	        else
	            s = char(encoder[extract(v,18,6)],
	            	encoder[extract(v,12,6)],
	            	encoder[extract(v,6,6)],
	            	encoder[extract(v,0,6)])
	        end
	        t[k] = s
	        k = k + 1
	    end
	    if lastn == 2 then
	        local a, b = str:byte( n-1, n )
	        local v = a*0x10000 + b*0x100
	        t[k] = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[extract(v,6,6)], encoder[64])
	    elseif lastn == 1 then
	        local v = str:byte( n )*0x10000
	        t[k] = char(encoder[extract(v,18,6)], encoder[extract(v,12,6)], encoder[64], encoder[64])
	    end
	    return concat( t )
	end

	ret.decode = function(b64, decoder, usecaching)
	    decoder = decoder or DEFAULT_DECODER
	    local pattern = '[^%w%+%/%=]'
	    if decoder then
	        local s62, s63
	        for charcode, b64code in pairs( decoder ) do
	            if b64code == 62 then s62 = charcode
	            elseif b64code == 63 then s63 = charcode
	            end
	        end
	        pattern = ('[^%%w%%%s%%%s%%=]'):format( char(s62), char(s63) )
	    end
	    b64 = b64:gsub( pattern, '' )
	    local cache = usecaching and {}
	    local t, k = {}, 1
	    local n = #b64
	    local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
	    for i = 1, padding > 0 and n-4 or n, 4 do
	        local a, b, c, d = b64:byte( i, i+3 )
	        local s
	        if usecaching then
	            local v0 = a*0x1000000 + b*0x10000 + c*0x100 + d
	            s = cache[v0]
	            if not s then
	                local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
	                s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
	                cache[v0] = s
	            end
	        else
	            local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
	            s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
	        end
	        t[k] = s
	        k = k + 1
	    end
	    if padding == 1 then
	        local a, b, c = b64:byte( n-3, n-1 )
	        local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40
	        t[k] = char( extract(v,16,8), extract(v,8,8))
	    elseif padding == 2 then
	        local a, b = b64:byte( n-3, n-2 )
	        local v = decoder[a]*0x40000 + decoder[b]*0x1000
	        t[k] = char( extract(v,16,8))
	    end
	    return concat( t )
	end

	local meta = getmetatable(root['base64']) or {}
	meta.__type = "library"
	setmetatable(root['base64'], meta)

	return root
end

local open_json = function(root)
	root['json'] = {}
	local json = root['json']

	-- Original: https://gist.github.com/tylerneylon/59f4bcf316be525b30ab
	-- Originally released to public domain

	-- Internal functions.

	local function kind_of(obj)
	  if type(obj) ~= 'table' then return type(obj) end
	  local i = 1
	  for _ in pairs(obj) do
	    if obj[i] ~= nil then i = i + 1 else return 'table' end
	  end
	  if i == 1 then return 'table' else return 'array' end
	end

	local function escape_str(s)
	  local in_char  = {'\\', '"', '/', '\b', '\f', '\n', '\r', '\t'}
	  local out_char = {'\\', '"', '/',  'b',  'f',  'n',  'r',  't'}
	  for i, c in ipairs(in_char) do
	    s = s:gsub(c, '\\' .. out_char[i])
	  end
	  return s
	end

	-- Returns pos, did_find; there are two cases:
	-- 1. Delimiter found: pos = pos after leading space + delim; did_find = true.
	-- 2. Delimiter not found: pos = pos after leading space;     did_find = false.
	-- This throws an error if err_if_missing is true and the delim is not found.
	local function skip_delim(str, pos, delim, err_if_missing)
	  pos = pos + #str:match('^%s*', pos)
	  if str:sub(pos, pos) ~= delim then
	    if err_if_missing then
	      return nil, 'Expected ' .. delim .. ' near position ' .. pos
	    end
	    return pos, false
	  end
	  return pos + 1, true
	end

	-- Expects the given pos to be the first character after the opening quote.
	-- Returns val, pos; the returned pos is after the closing quote character.
	local function parse_str_val(str, pos, val)
	  val = val or ''
	  local early_end_error = 'End of input found while parsing string.'
	  if pos > #str then
	  	return nil, early_end_error
	  end
	  local c = str:sub(pos, pos)
	  if c == '"'  then return val, pos + 1 end
	  if c ~= '\\' then return parse_str_val(str, pos + 1, val .. c) end
	  -- We must have a \ character.
	  local esc_map = {b = '\b', f = '\f', n = '\n', r = '\r', t = '\t'}
	  local nextc = str:sub(pos + 1, pos + 1)
	  if not nextc then
	  	return nil, early_end_error
	  end
	  return parse_str_val(str, pos + 2, val .. (esc_map[nextc] or nextc))
	end

	-- Returns val, pos; the returned pos is after the number's final character.
	local function parse_num_val(str, pos)
	  local num_str = str:match('^-?%d+%.?%d*[eE]?[+-]?%d*', pos)
	  local val = tonumber(num_str)
	  if not val then
	  	return nil, 'Error parsing number at position ' .. pos .. '.'
	  end
	  return val, pos + #num_str
	end


	-- Public values and functions.

	function json.stringify(obj, as_key)
	  local s = {}  -- We'll build the string as an array of strings to be concatenated.
	  local kind = kind_of(obj)  -- This is 'array' if it's an array or type(obj) otherwise.

	  if obj == json.null then
	  	return 'null'
	  elseif kind == 'array' then
	    if as_key then
	    	return nil, 'Can\'t encode array as key.'
	    end
	    s[#s + 1] = '['
	    for i, val in ipairs(obj) do
	      if i > 1 then s[#s + 1] = ', ' end
	      s[#s + 1] = json.stringify(val)
	    end
	    s[#s + 1] = ']'
	  elseif kind == 'table' then
	    if as_key then
	    	return nil, 'Can\'t encode table as key.'
	    end
	    s[#s + 1] = '{'
	    for k, v in pairs(obj) do
	      if #s > 1 then s[#s + 1] = ', ' end
	      s[#s + 1] = json.stringify(k, true)
	      s[#s + 1] = ':'
	      s[#s + 1] = json.stringify(v)
	    end
	    s[#s + 1] = '}'
	  elseif kind == 'string' then
	    return '"' .. escape_str(obj) .. '"'
	  elseif kind == 'number' then
	    if as_key then return '"' .. tostring(obj) .. '"' end
	    return tostring(obj)
	  elseif kind == 'boolean' then
	    return tostring(obj)
	  elseif kind == 'nil' then
	    return 'null'
	  else
	    return nil, 'Unjsonifiable type: ' .. kind .. '.'
	  end
	  return table.concat(s)
	end

	json.null = {}  -- This is a one-off table to represent the null value.

	function json.parse(str, pos, end_delim)
	  pos = pos or 1
	  if pos > #str then
	  	return nil, 'Reached unexpected end of input.'
	  end
	  local pos = pos + #str:match('^%s*', pos)  -- Skip whitespace.
	  local first = str:sub(pos, pos)
	  if first == '{' then  -- Parse an object.
	    local obj, key, delim_found = {}, true, true
	    pos = pos + 1
	    while true do
	      key, pos = json.parse(str, pos, '}')
	      if key == nil then
	      	-- Error handling
	      	if type(pos) == 'string' then
	      		return nil, pos
	      	else
	      		return obj, pos
	      	end
	      end
	      if not delim_found then
	      	return nil, 'Comma missing between object items.'
	      end
	      pos, err = skip_delim(str, pos, ':', true)  -- true -> error if missing.
	      -- Error handling
	      if pos == nil then
	      	return nil, err
	      end
	      obj[key], pos = json.parse(str, pos)
	      pos, delim_found = skip_delim(str, pos, ',')
	      -- Error handling
	      if pos == nil then
	      	return nil, delim_found
	      end
	    end
	  elseif first == '[' then  -- Parse an array.
	    local arr, val, delim_found = {}, true, true
	    pos = pos + 1
	    while true do
	      val, pos = json.parse(str, pos, ']')
	      if val == nil then return arr, pos end
	      if not delim_found then
	      	return nil, 'Comma missing between array items.'
	      end
	      arr[#arr + 1] = val
	      pos, delim_found = skip_delim(str, pos, ',')
	      -- Error handling
	      if pos == nil then
	      	return nil, delim_found
	      end
	    end
	  elseif first == '"' then  -- Parse a string.
	    return parse_str_val(str, pos + 1)
	  elseif first == '-' or first:match('%d') then  -- Parse a number.
	    return parse_num_val(str, pos)
	  elseif first == end_delim then  -- End of an object or array.
	    return nil, pos + 1
	  else  -- Parse true, false, or null.
	    local literals = {['true'] = true, ['false'] = false, ['null'] = json.null}
	    for lit_str, lit_val in pairs(literals) do
	      local lit_end = pos + #lit_str - 1
	      if str:sub(pos, lit_end) == lit_str then return lit_val, lit_end + 1 end
	    end
	    local pos_info_str = 'position ' .. pos .. ': ' .. str:sub(pos, pos + 10)
	    return nil, 'Invalid json syntax starting at ' .. pos_info_str
	  end
	end

	local meta = getmetatable(root['json']) or {}
	meta.__type = "library"
	setmetatable(root['json'], meta)

	return root
end

-- TODO: Help library
-- TODO: BigNum library

-- TODO: Hash library
-- jenkins/adler/fletcher implemented in https://git.sr.ht/~shakna/cnoevil3/tree/master/include/evil_hash.h
-- Luhn hash: https://github.com/aiq/luazdf/blob/master/algo/luhn/luhn.lua

-- TODO: TOML library

-- TODO: Parser Library
-- See https://github.com/pocomane/luasnip/blob/master/src/pegcore.lua

-- TODO: Datalog library

local open_uuid = function(root)
	root['uuid'] = {}

	local uuid = root['uuid']

	uuid.constant = {}

	-- UUID.NIL, special case
	uuid.constant['nil'] = '00000000-0000-0000-0000-000000000000'

	-- TODO: UUID v1 (Requires MAC, can be done crossplatform??)

	-- TODO: UUID v2 (Requires MAC, can be done crossplatform??)

	-- TODO: UUID v3 (Requires MD5)

	-- UUID v4, random
	do
		local seeded = false
		uuid.uuid4 = function()
			if seeded == false then
				math.randomseed(os.time())
				seeded = true
			end

			local template ='xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
		    local x = string.gsub(template, '[xy]', function (c)
		        local v = (c == 'x') and math.random(0, 0xf) or math.random(8, 0xb)
		        return string.format('%x', v)
		    end)
		    return x
		end

		uuid.uuida = function()
			if seeded == false then
				math.randomseed(os.time())
				seeded = true
			end

			local t = os.date("*t")

			local year = string.format("%04d", t.year)
			local month = string.format("%02d", t.month)
			local day = string.format("%02d", t.day)
			local node = string.sub(table.concat({year, month, day}), 1, 8)

			local hour = string.format("%02d", t.hour)
			local min = string.format("%02d", t.min)
			local secondary_node = string.sub(table.concat({hour, min}), 1, 4)

			local template = node .. '-' .. secondary_node .. '-axxx-yxxx-xxxxxxxxxxxx'
			local x = string.gsub(template, '[xy]', function (c)
		        local v = (c == 'x') and math.random(0, 0xf) or math.random(8, 0xb)
		        return string.format('%x', v)
		    end)
		    return x
		end
	end

	-- TODO: UUID v5 (Requires SHA-1)

	local meta = getmetatable(root['uuid']) or {}
	meta.__type = "library"
	setmetatable(root['uuid'], meta)

	return root
end

local open_ini = function(root)
	root['ini'] = {}

	local r = root['ini']

	local ini_lines = function(s)
		local r = {}
		local token = {}

		for i=1, #s do
			local c = s:sub(i, i)
			if c == '\n' then
				r[#r+1] = table.concat(token)
				token = {}
			else
				token[#token + 1] = c
			end
		end

		if #token > 0 then
			r[#r+1] = table.concat(token)
		end

		return r
	end

	local ini_strip = function(s)
		-- lstrip
		local idx, idx2 = s:find('^%s*')
		if idx2 >= idx then
			s = s:sub(idx2+1)
		end

		-- rstrip
		local idx, idx2 = s:find('%s*$')
		if idx2 >= idx then
			s = s:sub(1, idx - 1)
		end

		return s
	end

	local ini_parse = function(source)
		local current_heading = nil
		local r = {}

		local lines = ini_lines(source)

		for idx, item in ipairs(lines) do
			line = ini_strip(item)
			if #line > 0 then
				if line:sub(1,1) == '[' and line:sub(#line, #line) == ']' then
					current_heading = ini_strip(line:sub(2, #line - 1))
					r[current_heading] = {}
				elseif line:find('=') ~= nil then
					-- Seperate varname and value
					idx = line:find('=')
					var = ini_strip(line:sub(1, idx - 1))
					val_raw = line:sub(idx + 1)

					-- Ensure we have a valid section
					if current_heading == nil then
						current_heading = 'undefined section'
						if r[current_heading] == nil then
							r[current_heading] = {}
						end
					end

					-- Guess the type of the value
					if tonumber(val_raw) ~= nil then
						r[current_heading][var] = tonumber(val_raw)
					else
						r[current_heading][var] = val_raw
					end
				end
			end
		end

		return r
	end

	r.read_file = function(filename)
		local f = io.open(filename, "r")
		if f == nil then
			return nil
		end

		local d = ini_parse(f:read("*all"))
		f:close()

		return d
	end

	local meta = getmetatable(root['ini']) or {}
	meta.__type = "library"
	setmetatable(root['ini'], meta)

	return root
end

-- Testing library
local open_test = function(root)
	root['unittest'] = {}

	root['unittest']['test'] = function()
		local r = {}
		r.passes = 0
		r.fails = 0

		r.results = function(self)
			if self.fails > 0 then
				print(string.format("FAILURE. Passed: %d/%d",
					self.passes,
					self.passes + self.fails))
			else
				print(string.format("Passed: %d/%d",
					self.passes,
					self.passes + self.fails))
			end

			return self.fails > 0
		end

		r.run = function(self, name, functor)
			local tests = self.passes + self.fails
			local fails = self.fails
			local now = os.clock()
			io.write(string.format("\t%-16s", name))
			functor()
			io.write(string.format("pass:%2d   fail:%2d   %4dms\n",
        		self.passes, self.fails,
        		math.floor((os.clock() - now) * 1000)))
		end

		r.ok = function(self, test)
			if not test then
				self.fails = self.fails + 1
				io.write(string.format("%s:%d error \n",
					debug.getinfo(2, 'S').short_src,
					debug.getinfo(2, 'l').currentline))
			else
				self.passes = self.passes + 1
			end
		end

		r.equal = function(self, a, b)
			if a ~= b then
				self.fails = self.fails + 1
				io.write(string.format("%s:%d (%d != %d)\n",
					debug.getinfo(2, 'S').short_src,
					debug.getinfo(2, 'l').currentline,
					a, b))
			else
				self.passes = self.passes + 1
			end
		end

		local meta = getmetatable(r) or {}
		meta.__type = "instance(test)"
		setmetatable(r, meta)

		return r
	end

	local meta = getmetatable(root['unittest']) or {}
	meta.__type = "library"
	setmetatable(root['unittest'], meta)

	return root
end

local open_lua = function(root)
	root['lua'] = {}

	for k, v in pairs(_G) do
		-- Skip these
		if k == 'arg' or k == '_G' then

		-- Functions
		elseif type(v) == 'function' then
			root['lua'][k] = v
		-- Constants
		elseif type(v) ~= 'table' then
			root['lua'][k] = v
		-- Tables
		else
			-- Copy them so as to not screw up our own references at runtime.
			root['lua'][k] = {}
			for ik, iv in pairs(v) do
				root['lua'][k][ik] = iv
			end
		end
	end

	local meta = getmetatable(root['lua']) or {}
	meta.__type = "library"
	setmetatable(root['lua'], meta)

	return root
end

-- Has to be here, for "exact same metamethod", otherwise __eq not called.
local utf8_equal = function(a, b)
	return tostring(a) == tostring(b)
end

local open_utf8 = function(root)
	local r = root

	-- UTF8 Library
	r['utf8'] = {}

	r['utf8']['create'] = function(source)
		local ret = {}

		ret._content = source

		local meta = getmetatable(ret) or {}
		meta.__type = 'utf8'
		meta.__index = function(self, key)
			if r['utf8'][key] ~= nil then
				return function(self, ...)
					local args = {...}
					return r['utf8'][key](self._content, unpack(args))
				end
			end
			if r['string'][key] ~= nil then
				return function(self, ...)
					local args = {...}
					local v = {r['string'][key](self._content, unpack(args))}
					for i=1, #v do
						if type(v[i]) == 'string' then
							v[i] = r['utf8']['create'](v[i])
						end
					end
					return unpack(v)
				end
			end
		end
		meta.__tostring = function(self)
			return self._content
		end
		meta.__eq = utf8_equal
		meta.__len = function(self)
			return utf8.len(self._content)
		end
		meta.__call = function(tbl, ...)
			return r['utf8']['substring'](tbl._content, ...)
		end

		setmetatable(ret, meta)

		return ret
	end

	-- UTF8 Constants
	r['utf8']['constant'] = {}
	r['utf8']['constant']['single_sequence'] = utf8.charpattern

	r['utf8']['length'] = utf8.len
	r['utf8']['offset'] = utf8.offset
	r['utf8']['codepoint'] = utf8.codepoint
	r['utf8']['iter'] = utf8.codes
	r['utf8']['char'] = utf8.char

	r['utf8']['explode'] = function(...)
		local x = utf8_explode(...)
		for i=1, #x do
			x[i] = r['utf8']['create'](x[i])
		end
		return x
	end

	r['utf8']['reverse'] = function(x)
		local tmp = utf8_explode(x)
		local tmp2 = {}
		for i = #tmp, 1, -1 do
			tmp2[#tmp2 + 1] = tostring(tmp[i])
		end
		return r['utf8']['create'](table.concat(tmp2))
	end

	r['utf8']['substring'] = function(source, start, finish)
		local ret = {}

		if start == nil then
			start = 1
		end
		if finish == nil then
			finish = utf8.len(source)
		end
		if finish < 0 then
			finish = utf8.len(source) - finish
		end

		local i = 1
		for uchar in string.gmatch(source, utf8.charpattern) do
			if i > finish then
				break
			end

			if i >= start and i <= finish then
				ret[#ret+1] = uchar
			end

			i = i + 1
		end

		return r['utf8']['create'](table.concat(ret))
	end

	r['utf8']['hamming'] = function(a, b)
		if #a ~= #b then
			return nil
		end

		a = tostring(a)
		b = tostring(b)

		local distance = 0
		for i=1, #a do
			if string.byte(a, i) ~= string.byte(b, i) then
				distance = distance + 1
			end
		end

		return distance
	end

	local meta = getmetatable(r['utf8']) or {}
	meta.__type = "library"
	meta.__call = function(self, value)
		return self['create'](value)
	end
	meta.__index = function(self, key)
		if rawget(self, key) then
			return rawget(self, key)
		else
			return function(...)
				local v = {r['string'][key](...)}
				for i=1, #v do
					if type(v[i]) == 'string' then
						v[i] = r['utf8']['create'](v[i])
					end
				end
				return unpack(v)
			end
		end
	end
	setmetatable(r['utf8'], meta)

	return r
end

local open_bitop = function(root)
	local r = root

	-- TODO: Luajit's "bit" library compatibility shim.

	-- TODO: We should have a plain-Lua fallback too.

	if bit32 == nil then
		r['bitop'] = {}
		return r
	end

	-- Bit Operations Library
	r['bitop'] = {}
	r['bitop']['extract'] = bit32.extract
	r['bitop']['and0'] = bit32.btest
	r['bitop']['arshift'] = bit32.arshift
	r['bitop']['rshift'] = bit32.rshift
	r['bitop']['lrotate'] = bit32.lrotate
	r['bitop']['rrotate'] = bit32.rrotate
	r['bitop']['replace'] = bit32.replace
	r['bitop']['lshift'] = bit32.lshift
	r['bitop']['band'] = bit32.band
	r['bitop']['bxor'] = bit32.bxor
	r['bitop']['bor'] = bit32.bor
	r['bitop']['bnot'] = bit32.bnot

	local meta = getmetatable(r['bitop']) or {}
	meta.__type = "library"
	setmetatable(r['bitop'], meta)

	return r
end

local open_coroutine = function(root)
	local r = root

	-- Coroutine Library
	r['coroutine'] = {}
	r['coroutine']['create'] = coroutine.create
	r['coroutine']['running'] = coroutine.running
	r['coroutine']['resume'] = coroutine.resume
	r['coroutine']['wrap'] = coroutine.wrap
	r['coroutine']['yield'] = coroutine.yield
	r['coroutine']['status'] = coroutine.status
	r['coroutine']['isyield'] = coroutine.isyieldable

	local meta = getmetatable(r['coroutine']) or {}
	meta.__type = "library"
	setmetatable(r['coroutine'], meta)

	return r
end

local open_time = function(root)
	local r = root

	-- Time Library
	r['time'] = {}
	r['timeit'] = function(functor, ...)
	  -- Note: Impure functions won't work properly.
	  local start = os.clock()
	  for i=1, 100 do
	    functor(...)
	  end
	  local fin = os.clock()
	  return (fin - start) / 100
	end
	r['time']['date'] = os.date
	r['time']['time'] = os.time
	r['time']['clock'] = os.clock
	r['time']['diff'] = os.difftime

	-- TODO: There's some useful calendar functions
	-- @ https://github.com/aiq/luazdf/tree/master/cal
	-- that we could reimplement.

	local meta = getmetatable(r['time']) or {}
	meta.__type = "library"
	setmetatable(r['time'], meta)

	return r
end

local open_os = function(root)
	local r = root

	-- OS Specific Library
	-- Most things have been moved out.
	r['os'] = {}
	r['os']['setlocale'] = os.setlocale
	r['os']['env'] = os.getenv

	-- TODO: whoami
	-- TODO: hostname
	-- TODO: os name

	local meta = getmetatable(r['os']) or {}
	meta.__type = "library"
	setmetatable(r['os'], meta)

	return r
end

local open_struct = function(root)
	local r = root

	-- Struct Library
	-- Break out memory functions into their
	-- own library.
	r['struct'] = {}
	r['struct']['pack'] = string.pack
	r['struct']['packsize'] = string.packsize
	r['struct']['unpack'] = string.unpack

	local meta = getmetatable(r['struct']) or {}
	meta.__type = "library"
	setmetatable(r['struct'], meta)

	return r
end

local open_string = function(root)
	local r = root

	-- String Library
	r['string'] = {}
	r['string']['upper'] = string.upper
	r['string']['length'] = string.len
	r['string']['find'] = string.find
	r['string']['reverse'] = string.reverse
	r['string']['byte'] = string.byte
	r['string']['char'] = string.char
	r['string']['lower'] = string.lower
	r['string']['substitute'] = string.gsub
	r['string']['substring'] = string.sub
	r['string']['match'] = string.match
	r['string']['fill'] = string.rep
	r['string']['match_all'] = string.gmatch

	r['string']['to_hex'] = function(source)
		local val, _ = string.gsub(source, ".", function(c)
    		return string.format("%02X", string.byte(c))
  		end)
  		return val
	end
	r['string']['from_hex'] = function(source)
		local val, _ = string.gsub(source, "..?", function(h)
    		return string.char(tonumber(h, 16))
  		end)
  		return val
	end

	r['string']['hamming'] = function(a, b)
		if #a ~= #b then
			return nil
		end

		local distance = 0
		for i=1, #a do
			if string.byte(a, i) ~= string.byte(b, i) then
				distance = distance + 1
			end
		end

		return distance
	end

	r['string']['starts_with'] = function(source, prefix)
		if type(prefix) == 'string' then
			return string.sub(source, 1, string.len(prefix)) == prefix
		elseif type(prefix) == 'table' then
			for i=1, #prefix do
				if string.sub(source, 1, string.len(prefix[i])) == prefix[i] then
					return true
				end
			end
			return false
		end
	end

	r['string']['ends_with'] = function(source, suffix)
		if type(suffix) == 'string' then
			return string.sub(source, -string.len(suffix)) == suffix
		elseif type(suffix) == 'table' then
			for i=1, #suffix do
				if string.sub(source, -string.len(suffix[i])) == suffix[i] then
					return true
				end
			end
			return false
		end
	end

	r['string']['explode'] = string_explode

	r['string']['title'] = function(source)
		local val, _ = string.gsub(source, "(%s)(%a)", function(w, c)
    		return w .. string.upper(c)
  		end)
  		local first = string.upper(string.sub(val, 1, 1))
  		return first .. string.sub(val, 2)
	end

	r['string']['snake'] = function(source)
		local val, _ = string.gsub(string.lower(source), "%s(%a)", function(c)
    		return '_' .. string.lower(c)
  		end)
  		local first = string.lower(string.sub(val, 1, 1))
  		return first .. string.sub(val, 2)
	end

	r['string']['camel'] = function(source)
		local val, _ = string.gsub(string.lower(source), "%s(%a)", function(c)
    		return string.upper(c)
  		end)
  		local first = string.lower(string.sub(val, 1, 1))
  		return first .. string.sub(val, 2)
	end

	local meta = getmetatable(r['string']) or {}
	meta.__type = "library"
	setmetatable(r['string'], meta)

	return r
end

local open_io = function(root)
	local r = root

	-- I/O Library.
	-- open/close are moved to base available.
	r['io'] = {}
	
	r['io']['popen'] = io.popen
	r['io']['lines'] = io.lines

	r['io']['tmpfile'] = io.tmpfile

	r['io']['type'] = io.type

	-- Extras that usually belong to OS...
	r['io']['tmpname'] = os.tmpname
	r['io']['remove'] = os.remove
	r['io']['rename'] = os.rename
	r['io']['execute'] = os.execute

	r['io']['cwd'] = function()
		return os.getenv("PWD") or os.getenv("cd")
	end
	
	r['io']['basename'] = function(name)
		local path_sep = package.config:sub(1,1)
		local index = name:match(string.format('^.*()%s', path_sep))
		-- No path seperator found:
		if index == nil or index > #name then
			index = 1
		elseif index ~= 1 then
			index = index + 1
		end
		-- Grab the piece of the name we want
		local prename = name:sub(index, #name)

		return prename
	end

	r['io']['dirname'] = dirname

	r['io']['datapath'] = datapath

	local meta = getmetatable(r['io']) or {}
	meta.__type = "library"
	setmetatable(r['io'], meta)

	return r
end

local open_random = function(root)
	local r = root

	-- Seed upon opening
	math.randomseed(os.time())

	-- Random Library
	-- TODO: Probably a few more things we could put here.
	r['random'] = {}
	r['random']['seed'] = function(...)
		local args = {...}
		if #args > 0 then
			math.randomseed(...)
		else
			math.randomseed(os.time())
		end
	end
	r['random']['random'] = math.random
	r['random']['choice'] = function(tbl)
		local min = 1
		local max = #tbl
		return tbl[math.random(min, max)]
	end

	r['random']['shuffle'] = function(tbl)
		shuffled = {}
		for i, v in ipairs(x) do
			local pos = math.random(1, #shuffled+1)
			table.insert(shuffled, pos, v)
		end
		return shuffled
	end

	-- TODO: r['random']['weighted'] = lib.random_weighted

	local meta = getmetatable(r['random']) or {}
	meta.__type = "library"
	setmetatable(r['random'], meta)

	return r
end

local open_functional = function(root)
	local r = root

	r['functional'] = {}

	r['functional']['recur'] = function()
  		-- Tail-call safe way of calling the enclosing function.
  		return debug.getinfo(2, "f").func
	end

	r['functional']['curry'] = function(a, b)

	  local meta = getmetatable(a) or {}
	  assert(type(a) == "function" or
	  	type(meta.__call) == "function",
	  	"curry expects a to be a function, but received a " .. type(a))

	  meta = getmetatable(b) or {}
	  assert(type(b) == "function" or
	  	type(meta.__call) == "function",
	  	"curry expects b to be a function, but received a " .. type(b))

	  return function(...)
	    return a(b(...))
	  end
	end

	r['functional']['apply'] = function(functor, ...)
	  -- TODO: This is an overly naive way of doing it...
	  local args = {...}
	  local meta = getmetatable(functor) or {}
	  assert(type(functor) == "function" or
	  	type(meta.__call) == "function",
	  	"apply expects functor to be a function")
	  return functor(unpack(args))
	end

	r['functional']['map'] = function(functor, ...)
	  local args = {...}

	  assert(type(args) == "table",
	  	"map expects args to be a table, but received a " .. type(args))

	  local meta = getmetatable(functor) or {}
	  assert(type(functor) == "function" or
	  	type(meta.__call) == "function",
	  	"map expects functor to be a function")

	  ret = {}
	  for k, v in ipairs(args) do
	    ret[#ret + 1] = functor(v)
	  end
	  return ret
	end

	r['functional']['filter'] = function(functor, ...)
	  local args = {...}

	  local meta = getmetatable(functor) or {}
	  assert(type(functor) == "function" or
	  	type(meta.__call) == "function",
	  	"filter expects functor to be a function")

	  ret = {}
	  for _, v in pairs(args) do
	    if functor(v) then
	      ret[#ret + 1] = v
	    end
	  end
	  return unpack(ret)
	end

	r['functional']['foldr'] = function(functor, tbl, val)

	  local meta = getmetatable(functor) or {}
	  assert(type(functor) == "function" or
	  	type(meta.__call) == "function",
	  	"Functor must be a function.")

	  assert(type(tbl) == "table", "foldr expects a table.")

	  for k, v in pairs(tbl) do
	    val = functor(v, val)
	  end
	  return val
	end

	r['functional']['set'] = function(tbl)
	  assert(type(tbl) == "table",
	  	"Set only works on tables, but received: " .. type(tbl))

	  local tmp = {}
	  for _, v in ipairs(tbl) do
	    tmp[v] = true
	  end
	  local ret = {}
	  for k, _ in pairs(tmp) do
	    ret[#ret + 1] = k
	  end
	  return ret
	end

	local meta = getmetatable(r['functional']) or {}
	meta.__type = "library"
	setmetatable(r['functional'], meta)

	return r
end

local open_table = function(root)
	local r = root

	r['table'] = {}

	r['table']['move'] = function(...)
		local args = {...}
		local meta = getmetatable(args[1])
		if meta and meta.__readonly then
			error("Can't move from a read only table!", 2)
		end
		meta = getmetatable(args[#args])
		if meta and meta.__readonly then
			error("Can't move to a read only table!", 2)
		end

		return table.move(...)
	end

	r['table']['remove'] = function(tbl, ...)
		local meta = getmetatable(tbl)
		if meta and meta.__readonly then
			error("Can't remove from a read only table!", 2)
		end
		return table.remove(tbl, ...)
	end
	
	r['table']['insert'] = function(tbl, ...)
		local meta = getmetatable(tbl)
		if meta and meta.__readonly then
			error("Can't insert into a read only table!", 2)
		end
		return table.insert(tbl, ...)
	end

	r['table']['invert'] = function(tbl)
		local r = {}
	    for k, v in pairs(tbl) do
	      r[v] = k
	    end
	  	return r
	end
	r['table']['shuffle'] = function(tbl)
	  -- Fisher Yates shuffle
	  assert(type(tbl) == "table",
	  	"shuffle expected a table, but received: " .. type(tbl))

	  for i = #tbl, 1, -1 do
	    local r = math.random(#tbl)
	    tbl[i], tbl[r] = tbl[r], tbl[i]
	  end
	  return tbl
	end
	r['table']['reverse'] = table_reverse

	r['table']['readonly'] = function(t)
		return setmetatable({}, {
			__readonly = true,
			__index = t,
			__newindex = function(self, key, value)
				error("Can't write to read only table.", 2)
			end,
			__len = function(self)
				return #t
			end,
			__ipairs = function(tbl)
				return ipairs(t)
			end,
			__pairs = function(tbl)
				return pairs(t)
			end
		})
	end

	local meta = getmetatable(r['table']) or {}
	meta.__type = "library"
	setmetatable(r['table'], meta)

	return r
end

local open_matrix = function(matrix)
	-- Self referencing...
	matrix.__index = matrix

	matrix.constructor = function(self, data)
		local rows = #data
		local columns = #data[1]

		local ret = self:new(columns, rows)

		for i = 1, columns do
			for j = 1, rows do
				ret:insert(data[j][i], i, j)
			end
		end

		return ret
	end

	matrix.new = function(self, columns, rows)
		self = {}
		self._col = columns
		self._row = rows
		self._mtx = {}

		for i=1, columns * rows do
			self._mtx[i] = 0
		end

		local meta = {}
		meta.__type = 'matrix'

		meta.__eq = matrix.equal
		meta.__add = matrix.add
		meta.__sub = matrix.subtract
		meta.__mul = matrix.multiply
		meta.__div = matrix.divide

		meta.__tostring = function(self)
			return string.format("matrix(%d, %d)", self._col, self._row)
		end

		meta.__index = function(self, key)
			if rawget(self, key) == nil then
				return matrix[key]
			else
				return rawget(self, key)
			end
		end

		meta.__call = matrix.index

		setmetatable(self, meta)

		return self
	end

	matrix.columns = function(self)
		return self._col
	end
	matrix.rows = function(self)
		return self._row
	end

	matrix.index = matrix_index

	matrix.insert = function(self, value, column, row)
		local c = column - 1
		local r = row - 1

		self._mtx[c + r * self._col + 1] = value
	end

	matrix.equal = function(a, b)
		-- If either isn't a matrix then false
		if luaj_type(a) ~= 'matrix' or
			luaj_type(b) ~= 'matrix'
		then
			return false
		end

		-- Differing matrices are false
		if a:columns() ~= b:columns() or
			a:rows() ~= b:rows()
		then
			return false
		end

		-- Check all values are the same
		for i=1, a:columns() do
			for j=1, a:rows() do
				if a:index(i, j) ~= b:index(i, j) then
					return false
				end
			end
		end

		return true
	end

	matrix.add = function(a, b)
		local adder = function(value, m, reverse)
			local out = matrix:new(m:columns(), m:rows())

			for i=1, m:columns() do
				for j=1, m:rows() do
					if reverse == true then
						out:insert(value + m:index(i, j), i, j)
					else
						out:insert(m:index(i, j) + value, i, j)
					end
				end
			end

			return out
		end

		-- Adding numbers
		if type(a) == 'number' then
			return adder(a, b)
		elseif type(b) == 'number' then
			return adder(b, a)
		end

		-- Adding matrices

		-- Check that we have the same size
		if a:columns() ~= b:columns() or
			a:rows() ~= b:rows()
		then
			return nil
		end

		local out = matrix:new(a:columns(), a:rows())

		for i=1, a:columns() do
			for j=1, a:rows() do
				out:insert(a:index(i, j) + b:index(i, j), i, j)
			end
		end

		return out
	end

	matrix.subtract = function(a, b)
		local subber = function(value, m, reverse)
			local out = matrix:new(m:columns(), m:rows())

			for i=1, m:columns() do
				for j=1, m:rows() do
					if reverse == true then
						out:insert(value - m:index(i, j), i, j)
					else
						out:insert(m:index(i, j) - value, i, j)
					end
				end
			end

			return out
		end

		-- Adding numbers
		if type(a) == 'number' then
			return subber(a, b, true)
		elseif type(b) == 'number' then
			return subber(b, a)
		end

		-- Adding matrices

		-- Check that we have the same size
		if a:columns() ~= b:columns() or
			a:rows() ~= b:rows()
		then
			return nil
		end

		local out = matrix:new(a:columns(), a:rows())

		for i=1, a:columns() do
			for j=1, a:rows() do
				out:insert(a:index(i, j) - b:index(i, j), i, j)
			end
		end

		return out
	end

	matrix.multiply = function(a, b)
		local multiplier = function(value, m, reverse)
			local out = matrix:new(m:columns(), m:rows())

			for i=1, m:columns() do
				for j=1, m:rows() do
					if reverse then
						out:insert(value * m:index(i, j), i, j)
					else
						out:insert(m:index(i, j) * value, i, j)
					end
				end
			end

			return out
		end

		-- Adding numbers
		if type(a) == 'number' then
			return multiplier(a, b, true)
		elseif type(b) == 'number' then
			return multiplier(b, a)
		end

		-- Adding matrices

		-- Check that we have the same size
		if a:columns() ~= b:columns() or
			a:rows() ~= b:rows()
		then
			return nil
		end

		local out = matrix:new(a:columns(), a:rows())

		for i=1, a:columns() do
			for j=1, a:rows() do
				out:insert(a:index(i, j) * b:index(i, j), i, j)
			end
		end

		return out
	end

	matrix.divide = function(a, b)
		local divider = function(value, m, reverse)
			if value == 0 then
				return nil
			end

			local out = matrix:new(m:columns(), m:rows())

			for i=1, m:columns() do
				for j=1, m:rows() do
					if reverse == true then
						out:insert(value / m:index(i, j), i, j)
					else
						out:insert(m:index(i, j) / value, i, j)
					end
				end
			end

			return out
		end

		-- Adding numbers
		if type(a) == 'number' then
			return divider(a, b, true)
		elseif type(b) == 'number' then
			return divider(b, a)
		end

		-- Adding matrices

		-- Check that we have the same size
		if a:columns() ~= b:columns() or
			a:rows() ~= b:rows()
		then
			return nil
		end

		local out = matrix:new(a:columns(), a:rows())

		for i=1, a:columns() do
			for j=1, a:rows() do
				if b:index(i, j) == 0 then
					return nil
				else
					out:insert(a:index(i, j) / b:index(i, j), i, j)
				end
			end
		end

		return out
	end

	matrix.dot = function(a, b)
		-- Ensure we have the right shape
		if a:columns() ~= b:rows() then
			return nil
		end

		local out = matrix:new(a:rows(), b:columns())

		for i=1, a:rows() do
			for j = 1, b:columns() do
				local tmp = 0
				for k = 1, a:columns() do
					tmp = tmp + a:index(k, i) * b:index(j, k)
				end
				out:insert(tmp, j, i)
			end
		end

		return out
	end

	-- matrix.reshape
	matrix.reshape = function(self, columns, rows)
		if columns * rows == self:columns() * self:rows() then
			self._col = columns
			self._row = rows
			return true
		else
			return nil
		end
	end

	local meta = getmetatable(matrix) or {}
	meta.__type = "library"
	meta.__call = matrix.constructor

	setmetatable(matrix, meta)
end

local open_luaj = function(root)
	root['luaj'] = {}

	local r = root['luaj']

	r['load'] = load_source
	r['make_env'] = make_env

	local meta = getmetatable(r) or {}
	meta.__type = "library"

	setmetatable(r, meta)

	return root
end

local open_math = function(root)
	local r = root

	-- Math Library
	r['math'] = {}

	-- Math constants
	r['math']['constant'] = {}
	r['math']['constant']['maxinteger'] = math.maxinteger
	r['math']['constant']['mininteger'] = math.mininteger
	r['math']['constant']['huge'] = math.huge
	r['math']['constant']['pi'] = math.pi
	r['math']['constant']['nan'] = 0/0

	r['math']['matrix'] = {}
	local matrix = r['math']['matrix']
	open_matrix(matrix)

	-- Math functions
	r['math']['mod'] = math.fmod
	r['math']['rexp'] = math.frexp
	r['math']['absolute'] = math.abs
	r['math']['toradians'] = math.rad
	r['math']['floor'] = math.floor
	r['math']['todegrees'] = math.deg
	r['math']['power'] = math.pow
	r['math']['hyperbolic_tangent'] = math.tanh
	r['math']['arc_sine'] = math.asin
	r['math']['ceiling'] = math.ceil
	r['math']['logarithm'] = math.log
	r['math']['ldexp'] = math.ldexp
	r['math']['exponent'] = math.exp
	r['math']['unsigned_lt'] = math.ult
	r['math']['cosine'] = math.cos
	r['math']['arc_cosine'] = math.acos
	r['math']['tangent'] = math.tan
	r['math']['log10'] = math.log10
	r['math']['hyperbolic_sine'] = math.sinh
	r['math']['square_root'] = math.sqrt
	r['math']['hyperbolic_cosine'] = math.cosh
	r['math']['arc_tangent2'] = math.atan2
	r['math']['arc_tangent'] = math.atan
	r['math']['sine'] = math.sin
	r['math']['modf'] = math.modf
	r['math']['type'] = math.type

	r['math']['round'] = {}
	r['math']['round']['up'] = function(x, n)
		if n == nil then
			n = 0
		end

		if x > 0 then
			return math.ceil(x * math.pow(10, n)) / math.pow(10, n)
		else
			return math.floor(x * math.pow( 10, n)) / math.pow(10, n)
		end
	end

	r['math']['round']['down'] = function(x, n)
		if n == nil then
			n = 0
		end

		if x > 0 then
			return math.floor(x * math.pow(10, n)) / math.pow(10, n)
		else
			return math.ceil(x * math.pow(10, n)) / math.pow(10, n)
		end
	end

	r['math']['is_nan'] = function(x)
		return x ~= x
	end

	r['math']['is_infinite'] = function(x)
		return x == math.huge or x == -math.huge
	end

	r['math']['is_finite'] = function(x)
		return x > -math.huge and x < math.huge
	end

	r['math']['cosecant'] = function(x)
		return 1 / math.sin(x)
	end

	r['math']['clamp'] = function(x, n, y)
		local t = {x, n, y}
		table.sort(t)
		return t[2]
	end

	r['math']['is_even'] = function(x)
		if x == 0 then
			return false
		end
		return math.fmod(x, 2) == 0
	end

	r['math']['is_odd'] = function(x)
		if x == 0 then
			return false
		end
		return math.fmod(x, 2) ~= 0
	end

	r['math']['in_range'] = function(x, min, max)
		return x >= min and x <= max
	end

	-- TODO: Convert to Roman numerals? And back to Arabic?

	local meta = getmetatable(r['math']) or {}
	meta.__type = "library"
	setmetatable(r['math'], meta)

	return r
end

make_env = function(identifier)
	local r = {}

	r['_VERSION'] = table.concat(luaj_version, '.')

	r['next'] = luaj_next
	r['assert'] = assert
	r['print'] = print
	r['printf'] = function(...)
		return io.stdout:write(string.format(...))
	end

	r['deepcopy'] = function(orig, copies)
	    copies = copies or {}
	    local orig_type = type(orig)
	    local copy
	    if orig_type == 'table' then
	        if copies[orig] then
	            copy = copies[orig]
	        else
	            copy = {}
	            copies[orig] = copy
	            for orig_key, orig_value in next, orig, nil do
	                copy[r['deepcopy'](orig_key, copies)] = r['deepcopy'](orig_value, copies)
	            end
	            setmetatable(copy, r['deepcopy'](getmetatable(orig), copies))
	        end
	    else
	        copy = orig
	    end
	    return copy
	end

	-- TODO: prettyprint
	-- TODO: serialize
	-- TODO: deserialise
	r['memoize'] = memoize

	r['locals'] = get_locals

	-- Iterators
	r['items'] = luaj_pairs
	r['values'] = value_iterator

	-- Sorted pairs
	r['iter'] = iter

	r['tonumber'] = tonumber
	r['select'] = select

	r['type'] = luaj_type

	r['error'] = error
	r['pcall'] = pcall
	r['tostring'] = tostring

	r['unpack'] = unpack
	r['pack'] = table.pack
	r['join'] = table.concat
	r['split'] = split
	r['sort'] = table.sort

	r['max'] = math.max
	r['min'] = math.min
	r['tointeger'] = math.tointeger

	if io ~= nil then
		r['stdin'] = io.stdin
		r['stderr'] = io.stderr
		r['stdout'] = io.stdout
		r['open'] = io.open
		r['close'] = io.close
	end

	r['format'] = string.format
	r['iformat'] = iformat

	r['try'] = try

	r['exit'] = os.exit

	r['fn'] = function(s)
	  return assert(load("return function " .. tostring(s) .. " end"),
	  	"fn was unable to build a valid function from: <" .. tostring(s) .. ">")()
	end

	-- metatable stuff
	r['metatable'] = {}
	r['metatable']['get'] = function(o)
		return getmetatable(o)
	end
	r['metatable']['set'] = function(o, t)
		return setmetatable(o, t)
	end

	r['paths'] = function(name)
		if name == nil then
			name = '?'
		end

		-- Our custom set of paths
		local path_sep = package.config:sub(1,1)
		local paths = {}

		-- Local directory...
		local ref_dir = nil
		if dirname(name) == name then
			ref_dir = '.'
		else
			ref_dir = dirname(name)
			ref_dir = ref_dir:sub(1, #ref_dir - 1)
		end

		paths[#paths + 1] = string.format("%s%s?.lua", ref_dir, path_sep)
		paths[#paths + 1] = string.format("%s%s?%sinit.lua",
			ref_dir, path_sep, path_sep)

		-- Data directory paths...
		paths[#paths + 1] = string.format("%s%s%s%s%s?.lua",
			datapath(), "luaj", path_sep, version(), path_sep)
		paths[#paths + 1] = string.format("%s%s%s%s%s%s%sinit.lua",
			datapath(), "luaj", path_sep, version(), path_sep, "?", path_sep)

		-- Get package paths from Lua...
		local path_list_sep = package.config:sub(3, 3)
		local token = {}
		for i=1, #package.path do
			local c = package.path:sub(i, i)
			if c == path_list_sep then
				if #token > 0 then
					paths[#paths+1] = table.concat(token)
					token = {}
				end
			else
				token[#token+1] = c
			end
		end

		local ref_dir = nil
		if dirname(name) == name then
			ref_dir = '.'
		else
			ref_dir = dirname(name)
		end

		if #token > 0 then
			paths[#paths+1] = table.concat(token)
		end

		local r = {}

		for idx, val in ipairs(paths) do
			local val = val:gsub("?", name)
			val = val .. 'j'
			r[#r+1] = val
		end

		return r
	end

	r['import'] = function(name)
		local path_list = r['paths'](name)
		for idx, v in ipairs(path_list) do
			local f = io.open(v, "r")
			if f ~= nil then
				local source = f:read("*all")
				f:close()
				local data = load_source(source, v, true)
				return data()
			end
		end
	end

	r['switch'] = function(case)
		return function(codetable)
			local f
			f = codetable[case] or codetable.default

			if case ~= nil then
        		f = codetable[case] or codetable.default
      		else
        		f = codetable.missing or codetable.default
      		end

			if f then
				if type(f) == 'function' then
					return f(case)
				else
					return f
				end
			end
		end
	end

	r['operator'] = function(f)
		local mt = { __sub = function(self, b)
			return f(self[1], b)
		end}

		return setmetatable({}, { __sub = function(a, _)
			return setmetatable({ a }, mt) end
		})
	end

	enum = setmetatable({}, {
		__newindex = function(self, key, value)
			error("Cannot assign to an enum.", 2)
		end,
		__index = function(E, k)
			if type(k) ~= 'string' then
				error("Cannot reference a non-string enum.", 2)
			end

			if rawget(E, k) == nil then
				rawset(E, k, {})

				local meta = {
					__tostring = function(self)
						return k
					end,
					__len = function(self)
						return 0
					end,
					__lt = function(a, b)
						return true
					end,
					__gt = function(a, b)
						return true
					end,
					-- Semiprivate metatable!
					__metatable = {__type = "enum"}
				}
				setmetatable(rawget(E, k), meta)
			end
			return rawget(E, k)
		end
	})
	r['enum'] = enum

	-- Copy to builtins
	r['builtins'] = {}
	for k, v in pairs(r) do
		if k ~= 'builtins' then
			r['builtins'][k] = v
		end
	end
	local builtin_meta = getmetatable(r['builtins']) or {}
	builtin_meta.__type = "library"
	setmetatable(r['builtins'], builtin_meta)

	local meta = getmetatable(r) or {}
	meta.__type = "environment"
	meta.__index = function(self, key)

		local library_openers = {
			math = open_math,
			table = open_table,
			functional = open_functional,
			random = open_random,
			io = open_io,
			string = open_string,
			struct = open_struct,
			os = open_os,
			time = open_time,
			coroutine = open_coroutine,
			bitop = open_bitop,
			utf8 = open_utf8,
			lua = open_lua,
			class = open_class,
			unittest = open_test,
			csv = open_csv,
			ini = open_ini,
			contract = open_contract,
			base64 = open_base64,
			cli = open_cli,
			uuid = open_uuid,
			json = open_json,
			luaj = open_luaj,
		}

		local stdlib = function(root)
			root['stdlib'] = {}
			local lib = root['stdlib']

			lib['builtins'] = root['builtins']

			for key, opener in pairs(library_openers) do
				lib[key] = opener({})[key]
			end

			local builtin_meta = getmetatable(lib) or {}
			builtin_meta.__type = "library"
			setmetatable(lib, builtin_meta)

			return root
		end

		if library_openers[key] ~= nil then
			if rawget(self, key) == nil then
				return library_openers[key](self)[key]
			end
			return rawget(self, key)
		elseif key == 'stdlib' then
			if rawget(self, key) == nil then
				return stdlib(self)[key]
			end
			return rawget(self, key)
		else
			return rawget(self, key)
		end
	end
	setmetatable(r, meta)

	-- Expected self reference.
	r._G = r
	r._G.__index = r._G

	return r
end

local get_line = function(s, idx)
	local line_no = 1
	local ret = {}
	for i=1, #s do
		local c = s:sub(i, i)
		if c == '\n' then
			line_no = line_no + 1
		end

		if line_no > idx then
			break
		end

		if line_no == idx then
			if c ~= '\n' then
				ret[#ret + 1] = c
			end
		end
	end

	return table.concat(ret)
end

-- TODO: If we can get a PEG library, we might be able to add a macro system.

load_source = function(source, identifier, lib, env)

	if env == nil then
		env = make_env(identifier)
	end

	if lib ~= nil then
		env['_LIBRARY'] = true
	end

	-- If has a hashbang line, clear that line.
	if source:sub(1, 2) == '#!' then
		source = source:sub(source:find("\n") + 1)
	end

	-- Replace default metatables...
	local meta_header = [[
		lua.debug.setmetatable("",
			{__index = function(tbl, key)
				if type(key) == 'integer' then
					return string.substring(tbl, key, key)
				elseif string[key] ~= nil then
					return string[key]
				elseif lua.string[key] ~= nil then
					return lua.string[key]
				end
			end,

			__call = function(tbl, ...)
				return string.substring(tbl, ...)
			end,

			__mod = iformat}
		);
	]]
	meta_header = string.gsub(meta_header, "\n", "")

	local statement = string.format("%s return function(arg) %s end", meta_header, source .. '\n')

	-- TODO: We might be able to use debug hooks to add this kind of nice error messaging everywhere.

	-- This only handles syntax errors...
	local func, err = load(statement, identifier, "bt", env)
	if err ~= nil then
		-- Get the first character of the error message
		local first = string.sub(err, 1, 1)

		-- Get the line the error occurred on
		local line = 1
		string.gsub(err, ":(%d+):", function(bit)
			line = tonumber(bit)
		end)
		line = line - 1

		-- Remove the "string" from the start of the error message
		err = first .. string.sub(err, 9)

		-- Show the line where the problem happened
		err = err .. ' ->' .. '\n' .. get_line(source, line)

		err = "Error while loading: \n" .. err
		return nil, err
	end
	return func(), nil
end

local repl = function()
	-- Create an environment to maintain
	local env = make_env(identifier)

	-- TODO: Allow dumping env to file...
	-- Do we want to string.dump or dump the concat of every input statement?

	local statement_count = 0

	print(string.format("Luaj v%s", env._VERSION))
	print("Preface statements with = or return to get their value.")
	print("Place 'q' alone on its own line to exit.")

	-- If Lua can find linenoise, we should use it. Otherwise, fall back to this behaviour.
	local L = require 'linenoise'
	if L ~= nil then
		-- TODO: Add completion & hints based on keywords + locals
		-- TODO: Support history file?
		L.enableutf8()
	end

	while true do
		local source
		if L ~= nil then
			local line, err = L.linenoise("> ")
			if err then
				io.stderr:write(string.format("Prompt Error: %s\n", err))
				os.exit(1)
			end
			source = line
		else
			io.stdout:write("> ")
			source = io.stdin:read("*l")
		end
		statement_count = statement_count + 1

		-- Support the Lua REPL shorthand for return
		if source:sub(1, 1) == '=' then
			source = 'return ' .. source:sub(2)
		end

		-- Shorthand to quit the repl
		if source == 'q' then
			os.exit(0)
		end
		
		local func, err = load_source(source, string.format("repl:%d", statement_count), nil, env)
		if err ~= nil then
			io.stderr:write(err .. '\n')
		else
			if L ~= nil then
				L.historyadd(source)
			end

			local d = {pcall(func)}
			local success = table.remove(d, 1)
			local ret = d

			if not success then
				for _, v in ipairs(ret) do
					print(v)
				end
			else
				if #ret > 1 then
					for idx, val in ipairs(ret) do
						local x = iformat("${val}", {val = val})
						if x == "${val}" then
							x = nil
						end
						print(idx, x)
					end
				else
					local x = iformat("${val}", {val = ret[1]})
					if x == "${val}" then
						x = nil
					end
					print(x)
				end
			end
		end
	end

	return 0
end

-- TODO: Either run or compile to bytecode and/or C somehow?

-- Only run if not being imported.
if debug.getinfo(3) == nil then
	local cli_env = {}
	cli_env = open_cli(cli_env)['cli']

	local arg_parser = cli_env.argparse.parser(arg[0])
	arg_parser:positional("file", "The Luaj file to execute.")
	arg_parser:toggle("version", "The version of the Luaj interpreter.")

	local success, data = arg_parser:parse(arg)
	if not success or data.help then
		print(arg_parser:help())
		if success then
			os.exit(0)
		else
			os.exit(1)
		end
	end

	cli_env = nil
	arg_parser = nil

	if data.version then
		print(string.format("Luaj %s", table.concat(luaj_version, '.')))
		os.exit(0)
	end

	local filename = data.file

	if filename == '-' then
		os.exit(repl())
	end

	local f = io.open(filename, "r")
	
	if f == nil then
		io.stdout:write(string.format("Luaj %s\n", table.concat(luaj_version, '.')))
		io.stdout:write(string.format("ERROR: Unable to open file <%s>\n", arg[1]))
		print(arg_parser:help())
		os.exit(1)
	end

	local source = f:read("*all")
	f:close()

	local func, err = load_source(source, filename)
	if err ~= nil then
		io.stderr:write(err .. '\n')
		os.exit(1)
	end

	local new_args = {}
	for k, v in iter(arg) do
		new_args[k - 1] = v
	end

	return func(new_args)
else
	-- Minimum library exposed
	return {
		load = load_source,
		_make_env = make_env,
	}
end
