-- Advanced Source Library
-- The /other/ A/S/L. :3
-- by zorg § ISC @ 2018-2021

-- Safeguards
do
	local M,m,r = love.getVersion()
	assert((M == 11 and m >= 3) or M >= 12,
		"This library needs at least LÖVE 11.3.0 to function.")
	assert(love.audio and love.sound,
		"This library needs both love.audio and love.sound enabled to function.")
end

-- Relative require in the thread code.
local path = ...
if not path:find('%.') then
	path = ''
else
	path = path:gsub('%.[^%.]+$', '')
end

-- The processing thread.
local procThread

-- Channel to receive data through.
local toHere = love.thread.newChannel()

-- Use a named channel to test for the existence of the processing thread; if it exists, use that.
local toProc = love.thread.getChannel('zorg.asl.procThread')

toProc:performAtomic(function(ch)
	ch:push('procThread?')
	ch:push(toHere)
end)

procThread = toHere:demand(0.1)

if not procThread then
	toProc:clear() -- clear toProc queue since query events never got processed... duh.

	procThread = love.thread.newThread(path:gsub('%.','%/') .. '/asl-thread.lua')
	procThread:start(toProc)
	toProc:performAtomic(function(ch)
		ch:push('procThread!')
		ch:push(procThread)
	end)
end

-- List of methods supported by an ASource, and parameter and retval counts.
local method = {}

method.clone = true

method.queue = true
method.getFreeBufferCount = true

method.setPitch = true -- deprecated
method.getPitch = true -- deprecated

method.play = true
method.pause = true
method.rewind = true
method.stop = true

method.isPlaying = true

method.seek = true
method.tell = true

method.setLooping = true
method.isLooping = true
method.setLoopPoints = true
method.getLoopPoints = true

method.getBitDepth = true
method.getChannelCount = true
method.getDuration = true
method.getSampleRate = true

method.getType = true

method.getBufferSize = true
method.setBufferSize = true

method.getPitchShift = true
method.setPitchShift = true
method.getResamplingRatio = true
method.setResamplingRatio = true
method.getTimeStretch = true
method.setTimeStretch = true

method.getEffect = true
method.setEffect = true
method.getFilter = true
method.setFilter = true

method.getAirAbsorption = true
method.setAirAbsorption = true
method.getAttenuationDistances = true
method.setAttenuationDistances = true
method.getCone = true
method.setCone = true
method.getDirection = true
method.setDirection = true
method.getPosition = true
method.setPosition = true
method.getRolloff = true
method.setRolloff = true
method.getVelocity = true
method.setVelocity = true
method.isRelative = true
method.setRelative = true
method.getVolume = true
method.setVolume = true
method.getVolumeLimits = true
method.setVolumeLimits = true

method.release = true
method.type = true
method.typeOf = true

-- The function handling passing queries to the proc. thread.
local transfer = function(instance, ...)
	local arg = {...}

	-- Do thread stuff
	toProc:performAtomic(function(ch)
		-- The method name... hackish; see below in the mt metatable.
		ch:push(instance.methodCall[1])
		-- Send retvals (if any) back into this thread.
		ch:push(toHere)
		-- Which ASource object we're referring to.
		ch:push(instance.id)
		-- Push number of parameters given to method.
		ch:push(#arg)
		-- Push parameters.
		if #arg>0 then
			for i=1, #arg do
				ch:push(arg[i])
			end
		end
	end)

	table.remove(instance.methodCall, 1)

	-- Return values returned by proc. thread; combined with error handling,
	-- because threads are asnync & demanding would freeze the current thread.
	local retval = {}
	local retvalCount
	while not retvalCount do
		retvalCount = toHere:pop()
		local threadError = procThread:getError()
		if threadError then error(threadError) end
	end

	-- Allow chaining calls as long as they're not getters.
	if retvalCount == 0 then return instance end

	for i=1, retvalCount do
		table.insert(retval, toHere:pop())
	end
	return unpack(retval)
end

-- Route all method calls to the transfer function.
local mt = {__index = function(instance, m)
	if method[m] then
		-- Hack: Add the current method's name to the proxy instance so we can refer to that in
		--       the transfer function... also due to how metamethod indexing works, we actually
		--       need a stack for this to work correctly.
		table.insert(instance.methodCall, 1, m)
		return transfer
	end
end}

-- Returns a proxy object that utilizes metamethods to transfer calls to methods over to
-- the processing thread.
local new = function(a,b,c,d)

	-- Send construction request to proc thread.
	toProc:performAtomic(function(ch)
		ch:push('new')
		ch:push(toHere)
		ch:push(a)
		ch:push(b)
		ch:push(c)
		ch:push(d)
	end)

	-- Create hollow instance.
	local asource = {}

	-- Add internal method name list, because metatable calls are apparently instantaneous with
	-- embedded functions (AS:set(AS:get()) has the order set, get, so it calls get twice...)
	asource.methodCall = {}

	-- Make this work more or less like a regular source.
	setmetatable(asource, mt)

	-- Save the returned object id since we'll be using that.
	asource.id = toHere:demand()
	
	--------------
	return asource
end

-- Successfully loaded library, make it available both ways.
love.audio.newAdvancedSource = new
return new