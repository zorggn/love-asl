-- Advanced Source Library
-- The /other/ A/S/L. :3
-- by zorg § ISC @ 2018-2023



--[[
Internal Variables
- Proxy Class:
	None, even love.audio.pause just queries the processing thread for a list of id-s it paused.
- Proxy Instances:
	- id        -> Uniquely identifies sources, even across threads.
	- callStack -> A table containing the method calls in LIFO order; needed due to how mt-s work.
--]]



-- Safeguards.
do
	local M,m,r = love.getVersion()
	assert((M == 11 and m >= 3) or M >= 12,
		"This library needs at least LÖVE 11.3 to function.")
	assert(love.audio and love.sound,
		"This library needs both love.audio and love.sound enabled to function.")
end



-- Relative require the thread code.
local path = ...
do
	if not path:find('%.') then
		path = ''
	else
		path = path:gsub('%.[^%.]+$', '')
	end
end



-- The processing thread.
local procThread

-- Channel to receive data through.
local toHere = love.thread.newChannel()

-- Use a named channel to test for the existence of the processing thread; if it exists, use that.
local toProc = love.thread.getChannel('zorg.asl.procThread')
do
	toProc:performAtomic(function(ch)
		ch:push('procThread?')
		ch:push(toHere)
	end)
	
	-- Timeout if no thread responds.
	procThread = toHere:demand(0.1)
	
	if not procThread then
		-- clear toProc queue since query events never got processed.
		toProc:clear() 
	
		procThread = love.thread.newThread(path:gsub('%.','%/') .. '/asl-thread.lua')
		procThread:start(toProc)
		toProc:performAtomic(function(ch)
			ch:push('procThread!')
			ch:push(procThread)
		end)
	end
end



-- List of methods supported by an ASource; no need to store parameter and retval counts.
local method = {}
do
	method.clone = true

	method.type = true
	method.typeOf = true
	method.release = true

	method.getInternalSource = true -- Not really for usage outside of love.audio monkeypatching...

	method.getPitch = true -- Replaced with getResamplingRatio; still allowed for custom error msg.
	method.setPitch = true -- Replaced with setResamplingRatio; still allowed for custom error msg.

	method.queue = true

	method.getType = true
	method.getSampleRate = true
	method.getBitDepth = true
	method.getChannelCount = true

	method.getBufferSize = true
	method.setBufferSize = true
	method.getBufferVariance = true
	method.setBufferVariance = true
	method.getBufferVarianceDistribution = true
	method.setBufferVarianceDistribution = true

	method.isPlaying = true
	method.play = true
	--method.isPaused = false
	method.pause = true
	--method.isStopped = false
	method.stop = true

	method.tell = true
	method.seek = true
	method.getDuration = true
	method.rewind = true

	method.isLooping = true
	method.setLooping = true
	method.getLoopPoints = true
	method.setLoopPoints = true

	method.getInterpolationMethod = true
	method.setInterpolationMethod = true
	method.getMixMethod = true
	method.setMixMethod = true

	method.getResamplingRatio = true
	method.setResamplingRatio = true
	method.getTimeStretch = true
	method.setTimeStretch = true
	method.getPitchShift = true
	method.setPitchShift = true

	method.getPanLaw = true
	method.setPanLaw = true
	method.getPanning = true
	method.setPanning = true
	method.getStereoSeparation = true
	method.setStereoSeparation = true

	-- These are the methods löve/openalsoft itself gives that aren't reimplemented by the library.
	method.getFreeBufferCount = true

	method.getEffect = true
	method.setEffect = true
	method.getFilter = true
	method.setFilter = true
	method.getActiveEffects = true

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
	method.getVolumeLimits = true
	method.setVolumeLimits = true

	method.getVolume = true
	method.setVolume = true
end



-- Declare mt as a local before the transfer function. Both reference each other, so
local mt



-- The function handling passing queries to, and receiving data from the processing thread.
local transfer = function(instance, ...)
	local arg = {...}

	-- We'll need this later for the clone method.
	local methodName = instance.callStack[1]

	-- Do thread stuff:
	toProc:performAtomic(function(ch)
		-- The method name... hackish; see below in the mt metatable;
		ch:push(methodName)
		-- Send retvals (if any) back into this thread;
		ch:push(toHere)
		-- Which ASource object we're referring to;
		ch:push(instance.id)
		-- Push number of parameters given to method;
		ch:push(#arg)
		-- Push parameters, if they exist.
		if #arg>0 then
			for i=1, #arg do
				if methodName == 'setPanLaw' and type(arg[i]) == 'function' then
					-- Can't send functions to other threads through channels directly.
					ch:push(string.dump(arg[i]))
				else
					ch:push(arg[i])
				end
			end
		end
	end)

	-- We're using a stack; again, see the hack in the metatable definition below.
	table.remove(instance.callStack, 1)

	-- Return values returned, in order, by the proc. thread; combined with error handling,
	-- because threads are asnync & demanding would freeze the current thread.
	local retvalCount
	local retval = {}

	-- The moment this is done, we have all needed parameters, since it's atomic on the other side.
	while not retvalCount do
		retvalCount = toHere:pop()
		local threadError = procThread:getError()
		if threadError then error(threadError) end
	end
	
	for i=1, retvalCount do
		table.insert(retval, toHere:pop())
	end
	
	-- Cloning a source is neither a getter or a setter; need to handle it here separately.
	if methodName == 'clone' then
		local asource = {}
		asource.callStack = {}
		setmetatable(asource, mt)
		asource.id = retval[1]
		return asource
	else
		-- Allow chaining calls as long as they're not getters.
		if retvalCount == 0 then return instance end
		return unpack(retval)
	end
end



-- Route all method calls to the transfer function.
mt = {__index = function(instance, m)
	if method[m] then
		-- Hack: Add the current method's name to the proxy instance so we can refer to that in
		--       the transfer function... also due to how metamethod indexing works, we actually
		--       need a stack for this to work correctly.
		table.insert(instance.callStack, 1, m)
		return transfer
	end
end}



-- Returns a proxy object that utilizes metamethods to transfer calls to methods over to
-- the processing thread.
local new = function(a,b,c,d,e)
	-- Send construction request to processing thread.
	toProc:performAtomic(function(ch)
		ch:push('new')
		ch:push(toHere)
		ch:push(a)
		ch:push(b)
		ch:push(c)
		ch:push(d)
		ch:push(e)
	end)

	-- Create proxy instance.
	local asource = {}

	-- Add internal method name stack, because metatable calls are apparently instantaneous with
	-- chained functions; `AS:set(AS:get())` runs in the order [set, get], so it calls get twice.
	asource.callStack = {}

	-- Make this work more or less like a regular source.
	setmetatable(asource, mt)

	-- Save the returned object id since we'll be using that.
	while not asource.id do
		asource.id = toHere:pop()
		local threadError = procThread:getError()
		if threadError then
			error(threadError)
		end
	end
	
	--------------
	return asource
end



-- Monkeypatch love.audio.play/pause/stop in the thread we loaded the library in so it can
-- correctly handle ASources as well. (They are the only functions that need to be messed with...)
local oldLAPlay, oldLAPause, oldLAStop = love.audio.play, love.audio.pause, love.audio.stop

function love.audio.play(...)
	local temp = select(1, ...)
	if type(temp) == 'table' and temp.typeOf then temp = {...} end
	for i,v in ipairs(temp) do
		if v:typeOf('ASource') then
			v:play()
			temp[i] = v:getInternalSource()
			temp[i]:stop() -- Sync test... doesn't seem to work.
			v.playbackOffset = 0.0 -- Sync test #2
		end
	end
	return oldLAPlay(temp) -- Returns true if all Sources succeeded in being started/resumed.
end

function love.audio.pause(...)
	if select('#', ...) == 0 then
		-- Pause all of the ASources.
		toProc:performAtomic(function(ch)
			ch:push('pauseall')
			ch:push(toHere)
		end)
		-- Get response.
		local pausedCount
		while not pausedCount do
			pausedCount = toHere:pop()
			local threadError = procThread:getError()
			if threadError then error(threadError) end
		end
		-- Iterate over id-s for sources we paused with the above call.
		local temp = {}
		for i=1, pausedCount do
			local id = toHere:pop()
			local asource = {}
			asource.callStack = {}
			setmetatable(asource, mt)
			asource.id = id
			table.insert(temp, asource) 
		end
		-- Call original function, remove returned sources that are part of ASource objects.
		local temp2 = oldLAPause()
		for i,v in ipairs(temp) do
			for j,w in ipairs(temp2) do
				if v:getInternalSource() == w then
					temp2[j] = false
				end
			end
		end
		for i=#temp2, 1, -1 do 
			if temp2[i] == false then table.remove(temp2, i) end
		end
		-- Return combined table.
		for i,v in ipairs(temp2) do table.insert(temp, v) end
		return temp
	else
		-- Only pause select ASources.
		local temp = select(1, ...)
		if type(temp) == 'table' and temp.typeOf then temp = {...} end
		for i,v in ipairs(temp) do
			if v:typeOf('ASource') then
				v:pause()
				temp[i] = v:getInternalSource()
			end
		end
		return oldLAPause(temp)
	end
end

function love.audio.stop(...)
	if select('#', ...) == 0 then
		-- Stop all of the ASources.
		toProc:performAtomic(function(ch)
			ch:push('stopall')
			ch:push(toHere)
		end)
		-- Get response.
		local done
		while not done do
			done = toHere:pop()
			local threadError = procThread:getError()
			if threadError then error(threadError) end
		end
		return oldLAStop()
	else
		-- Only stop select ASources.
		local temp = select(1, ...)
		if type(temp) == 'table' and temp.typeOf then temp = {...} end
		for i,v in ipairs(temp) do
			if v:typeOf('ASource') then
				v:stop()
				temp[i] = v:getInternalSource()
			end
		end
		return oldLAStop(temp)
	end
end



-- Successfully loaded library.
return new