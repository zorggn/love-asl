-- Advanced Source Library
-- Processing Thread
-- by zorg § ISC @ 2018-2019

-- Needed löve modules in this thread.
require('love.thread')
require('love.sound')
require('love.audio')
require('love.timer')

-- The thread itself, needed so we can have one unique processing thread.
local procThread

-- Shared communication channel to this thread.
local toProc = ...

-- List of ASource objects, their numerical indices being used as id-s.
local ASList = {}

-------------------------------

-- Barfing into the math library (of this thread).
math.sgn = function(x) return x<0 and -1.0 or 1.0 end

-- Constants
local SourceType = {['static'] = true,  ['stream']    = true, ['queue'] = true}
local PitchUnit  = {['ratio']  = true,  ['semitones'] = true}
local TimeUnit   = {['seconds'] = true, ['samples']   = true}

-- This is the default push-style API for Queueable Sources, that may be overridden by a
-- pull-styled one.
local Queue = function(instance, buffer)
	assert(instance._type == 'queue',
		"Cannot queue up data manually to non-queueable sources.")
	assert(instance.samplingRate == buffer:getSampleRate(),
		"Buffer sampling rate mismatch.")
	assert(instance.bitDepth == buffer:getBitDepth(),
		"Buffer bit depth mismatch.")
	assert(instance.channelCount == buffer:getChannelCount(),
		"Buffer channel count mismatch.")

	instance.buffer:release()
	instance.buffer = love.sound.newSoundData(
		buffer:getSampleCount(),
		instance.samplingRate,
		instance.bitDepth,
		instance.channelCount)

	-- May not be the most performant, but then again, the pull-style should be used anyway.	
	for i=0, buffer:getSampleCount()-1 do
		instance.buffer:setSample(i, buffer:getSample(i))
	end

	instance.source:queue(instance.buffer)

	return true
end

-- Defining generator functions that fill the internal Buffer object.
local Generator = {}

Generator.static = function(instance)
	
	local p = instance.pointer

	if instance._isPlaying then
		for i=0, instance.bufferSize-1 do

			-- Copy samplepoints to buffer.
			local smp = 0.0
			for ch=1, instance.channelCount do
				smp = instance.data:getSample(math.floor(p), ch)
				smp = math.min(math.max(smp, -1), 1)
				instance.buffer:setSample(i, ch, smp)
			end

			-- Calculate next buffer-internal pointer.
			p = (p + instance.innerOffset)

			if instance.looping then
				if instance.timeDilation >= 0 then
					while p > instance.endpoint do
						p = p - (instance.endpoint - instance.startpoint)
					end
				else
					while p < instance.startpoint do
						p = p + (instance.endpoint - instance.startpoint)
					end
				end
				p = p % instance.data:getSampleCount()
			else
				if p >= instance.data:getSampleCount() or p < 0 then
					-- Fill rest of the buffer with silence.
					for j=i+1, instance.bufferSize-1 do
						for ch=1, instance.channelCount do
							instance.buffer:setSample(j, ch, 0.0)
						end
					end

					instance:stop()
					return
				end
			end
		end

		-- Calculate next buffer-external pointer.
		if instance.looping then
			instance.pointer = (instance.pointer - (instance.outerOffset * instance.bufferSize))
			if instance.timeDilation >= 0 then
				while instance.pointer > instance.endpoint do
					instance.pointer = instance.pointer - (instance.endpoint - instance.startpoint)
				end
			else
				while instance.pointer < instance.startpoint do
					instance.pointer = instance.pointer + (instance.endpoint - instance.startpoint)
				end
			end
		else
			instance.pointer = (instance.pointer - (instance.outerOffset * instance.bufferSize))
		end
		instance.pointer = instance.pointer % instance.data:getSampleCount()

	else
		-- Fill buffer with silence.
		for i=0, instance.bufferSize-1 do
			for ch=1, instance.channelCount do
				instance.buffer:setSample(i, ch, 0.0)
			end
		end
	end	
end

Generator.stream = function(instance) -- TODO
	-- - Probably need to use Decoder:seek(); default buffer size is 16384 smps.
	-- - :isSeekable isn't exposed so we'll hope no one tries to open non-seekable vorbis files.
	-- - 2019 EDIT: :isSeekable is now exposed... but Decoders load the whole file into RAM...
	-- (alternatively, it should return a value that denotes it failed, instead of erroring...)
	-- - If pointer goes out of range of decoded samplepoints, we need to decode another batch.
	-- - Probably keep around a few extra buffers worth of decoded smp-s in the direction we're
	-- playing the file?
	-- - We also need to somehow get the correct sample offset of the returned SoundData...
end

Generator.queue  = function(instance)
	-- This actually just calls a pull-styled callback; deferring the heavy stuff to the user,
	-- but only if the user redefined Source:queue to be a callback (i.e. the function's memory
	-- address is different.)
	if instance._type == 'queue' and instance.queue ~= Queue then
		-- This has issues with the threaded approach, since we can't use channels to pass lua 
		-- functions through.
		instance.queue(instance.buffer)
	end-- TODO
end

-- Makes pitch shifting and time stretching possible, more or less.
function calculatePlaybackCoefficients(instance)
	instance.innerOffset =          instance.pitchShift *
	                       math.sgn(instance.timeDilation) *
	                                instance.resampleRatio
	instance.outerOffset =         -instance.timeDilation *
	                       math.abs(instance.resampleRatio)
end

-------------------------------

-- Class
local ASource = {}

-- Method called in this thread automatically.
function ASource.update(instance, dt)
	while instance.source:getFreeBufferCount() > 0 do
		Generator[instance._type](instance)
		instance.source:queue(instance.buffer)
		instance.source:play()
	end
end

-- Copy-constructor
function ASource.clone(instance)
	local clone = {}

	for k,v in pairs(instance) do
		clone[k] = v
	end

	-- Deep-copy specific fields that need it.

	-- .data -> if it's a SoundData, then it doesn't need to be duplicated since we don't allow
	--          messing with the contents, however, if it's a Decoder, it presents more issues.
	if clone.data:type() == 'Decoder' then
		if instance.orig then
			-- This means it came from a (Dropped)File.
			clone.data = love.sound.newDecoder(instance.orig)
		else
			-- We can just clone the decoder (since löve 11.3)
			clone.data = instance.data:clone()
		end
	end

	-- .source -> clone it neatly.
	clone.source = instance.source:clone()

	-- .buffer -> create a unique SoundData.
	clone.buffer = love.sound.newSoundData(
		clone.bufferSize,
		clone.samplingRate,
		clone.bitDepth,
		clone.channelCount)

	-- Set up more internals.
	calculatePlaybackCoefficients(clone)

	setmetatable(clone, mtASource)

	clone.id = #ASList+1
	ASList[clone.id] = clone

	return clone.id
end

-- QSource related
function ASource.queue(instance, buffer)
	return Queue(instance, buffer)
end

function ASource.getFreeBufferCount(instance)
	return instance.source:getFreeBufferCount()
end

-- Deprecated
function ASource.setPitch(instance)
	error("Function deprecated by Advanced Source Library; " ..
		"for the same functionality, use :setResamplingRatio instead.")
end

function ASource.getPitch(instance)
	error("Function deprecated by Advanced Source Library; " ..
		"for the same functionality, use :getResamplingRatio instead.")
end

-- State manipulation
function ASource.play(instance)
	instance._isPlaying = true
	return true
end

function ASource.pause(instance)
	instance._isPlaying = false
	return true
end

function ASource.rewind(instance)
	instance.pointer = instance.timeDilation >= 0 and 0 or instance.data:getSampleCount()-1
	return true
end

function ASource.stop(instance)
	instance:pause()
	instance:rewind()
	return true
end

function ASource.isPlaying(instance)
	return instance._isPlaying
end

function ASource.seek(instance, position, unit)
	-- TODO: Can we really not? We could mess with the AS object's buffer pointer...
	assert(instance._type ~= 'queue',
		"Can't seek Queuable sources.")

	assert(TimeUnit[unit],
		"Unsupported TimeUnit: " .. tostring(unit))

	if instance._type == 'static' then
		if unit == 'samples' then
			assert(position >= 0 and position < instance.data:getSampleCount(),
				"Attempted to seek outside of data range.")
			instance.pointer = position
		else
			assert(position >= 0 and position < instance.data:getDuration(),
				"Attempted to seek outside of data range.")
			instance.pointer = math.floor(position * instance.samplingRate)
		end
	elseif instance._type == 'stream' then
		if unit == 'samples' then
			assert(position >= 0 and position < instance.data:getDuration() * instance.samplingRate,
				"Attempted to seek outside of data range.")
			instance.pointer = position
		else
			assert(position >= 0 and position < instance.data:getDuration(),
				"Attempted to seek outside of data range.")
			instance.pointer = math.floor(position * instance.samplingRate)
		end
	end
end

function ASource.tell(instance, unit)
	assert(TimeUnit[unit],
		"Unsupported TimeUnit: " .. tostring(unit))

	-- This works with queueable sources as well; it probably did work the same way before, too.
	if unit == 'samples' then
		return instance.pointer
	else
		return instance.pointer / instance.samplingRate
	end
end

-- Looping related
function ASource.setLooping(instance, state)
	assert(instance._type ~= 'queue',
		"Can't set looping behaviour on Queuable sources.")
	assert(type(state) == 'boolean',
		"Parameter must be boolean.")

	instance.looping = state
end

function ASource.isLooping(instance)
	assert(instance._type ~= 'queue',
		"Can't query looping behaviour on Queuable sources.")

	return instance.looping
end

function ASource.setLoopPoints(instance, startpoint, endpoint)
	assert(instance._type ~= 'queue',
		"Can't set loop points on Queuable sources.")

	assert(type(startpoint) == 'number',
		"Given start point parameter not a number.")
	assert(type(endpoint)   == 'number',
		"Given end point parameter not a number.")
	assert(startpoint < endpoint,
		"Given startpoint parameter must be less than endpoint parameter.")
	assert(startpoint >= 0,
		"Given startpoint parameter must be larger than zero.")
	assert(endpoint >= 0,
		"Given endpoint parameter must be larger than zero.")

	-- Also assert based on actual audio file loaded... both with static and stream sources.
	if instance._type == 'static' then
		assert(startpoint < instance.data:getSampleCount(),
			"Given startpoint exceeds length of SoundData.")
		assert(endpoint < instance.data:getSampleCount(),
			"Given endpoint exceeds length of SoundData.")
	elseif instance._type == 'stream' then
		assert(startpoint < instance.data:getDuration() * instance.data:getSampleRate(),
			"Given startpoint exceeds length reported by Decoder.")
		assert(endpoint < instance.data:getDuration() * instance.data:getSampleRate(),
			"Given endpoint exceeds length reported by Decoder.")
	end

	instance.startpoint = startpoint
	instance.endpoint   = endpoint
end

function ASource.getLoopPoints(instance)
	assert(instance._type ~= 'queue',
		"Can't query loop points from Queuable sources.")

	return instance.startpoint, instance.endpoint
end

-- Format getters
function ASource.getBitDepth(instance)
	return instance.bitDepth
end

function ASource.getChannelCount(instance)
	return instance.channelCount
end

function ASource.getDuration(instance, unit)
	assert(TimeUnit[unit],
		"Unsupported TimeUnit: " .. tostring(unit))

	if instance._type == 'static' then
		if unit == 'samples' then
			return instance.data:getSampleCount()
		else
			return instance.data:getDuration()
		end
	elseif instance._type == 'stream' then 
		if unit == 'samples' then
			return instance.data:getDuration() * instance.samplingRate
		else
			return instance.data:getDuration()
		end
	end

	-- Queue sources don't support this since they could go on forever, theoretically;
	-- but löve default behaviour returns -1 here for them, so we should too.
	return -1
end

function ASource.getSampleRate(instance)
	return instance.samplingRate
end

function ASource.getType(instance)
	return instance._type
end


function ASource.getBufferSize(instance)
	return instance.bufferSize
end

function ASource.setBufferSize(instance, samplepoints)
	instance.source:stop()
	assert(type(samplepoints) == 'number' and
		samplepoints > 0 and samplepoints == math.floor(samplepoints),
		"Buffer size must be given as a positive nonzero integer.")
	instance.bufferSize = samplepoints
	instance.buffer = love.sound.newSoundData(
		instance.bufferSize,
		instance.samplingRate,
		instance.bitDepth,
		instance.channelCount)
	instance.source:play()
end

-- Time-domain manipulation
function ASource.getPitchShift(instance, unit)
	if unit == nil then unit = 'ratio' end

	assert(PitchUnit[unit],
		("Pitch shift unit %s unsupported."):format(tostring(unit)))

	if unit == 'ratio' then
		return instance.pitchShift
	else
		return instance.pitchShiftSt
	end
end

function ASource.setPitchShift(instance, amount, unit)

	-- range: [0,)
	-- shift amount saved internally
	-- amount sets first value only
	-- needs special exception for second value logic

	--[[
		0.0,  N/A  ->  0.0,   0.0  * buffer.size -- -inf semitones (true stop) -- not possible.
		----------------------------------------
		0.5,  N/A  ->  0.5,  -any  * buffer.size -- -12  semitones up
		0.75, N/A  ->  0.75, -any  * buffer.size -- -~6  semitones up
		1.0,  N/A  ->  1.0,  -any  * buffer.size -- +-0  semitones up
		1.5,  N/A  ->  1.5,  -any  * buffer.size -- +~6  semitones up
		2.0,  N/A  ->  2.0,  -any  * buffer.size -- +12  semitones up
		----------------------------------------
		0.5,  N/A  ->  0.5,  +any  * buffer.size -- -12 semitones down
		0.75, N/A  ->  0.75, +any  * buffer.size -- -~6 semitones down
		1.0,  N/A  ->  1.0,  +any  * buffer.size -- +-0 semitones down
		1.5,  N/A  ->  1.5,  +any  * buffer.size -- +~6 semitones down
		2.0,  N/A  ->  2.0,  +any  * buffer.size -- +12 semitones down
	--]]

	assert(type(amount) == 'number',
		"Pitch shifting amount must be a number.")

	if unit == nil then unit = 'ratio' end

	assert(PitchUnit[unit],
		("Pitch shift unit %s unsupported."):format(tostring(unit)))

	if (unit == 'ratio') and (amount <= 0) then
		error("Pitch shift amount can't be lower or equal to 0.")
	end

	instance.pitchShift = (unit == 'ratio') and 
		amount or 2^(amount/12)
	instance.pitchShiftSt = (unit == 'semitones') and
		amount or (math.log(amount)/math.log(2))*12

	calculatePlaybackCoefficients(instance)
end

function ASource.getResamplingRatio(instance)
	return instance.resampleRatio
end

function ASource.setResamplingRatio(instance, ratio)

	-- range: (,)
	-- resampling rate saved internally
	-- rate multiplies both values

	--[[
		2.0  ->  2.0,  -2.0  * buffer.size --   2x resample forwards  ( 200%, +12st)
		1.5  ->  1.5,  -1.5  * buffer.size -- 3/2x resample forwards  ( 150%,  +6st)
		1.0  ->  1.0,  -1.0  * buffer.size --   1x resample forwards  ( 100%,   0st)
		0.75 ->  0.75, -0.75 * buffer.size -- 3/4x resample forwards  (  75%,  -6st)
		0.5  ->  0.5,  -0.5  * buffer.size -- 1/2x resample forwards  (  50%, -12st)
		0.25 ->  0.25, -0.25 * buffer.size -- 1/4x resample forwards  (  25%, -24st)
		----------------------------------
		0.0  ->  0.0,   0.0  * buffer.size --   0x resample playback  (true stop)
		----------------------------------
		0.25 -> -0.25,  0.25 * buffer.size -- 1/4x resample backwards ( -25%, -24st)
		0.5  -> -0.5,   0.5  * buffer.size -- 1/2x resample backwards ( -50%, -12st)
		0.75 -> -0.75,  0.75 * buffer.size -- 3/4x resample backwards ( -75%,  -6st)
		1.0  -> -1.0,   1.0  * buffer.size --   1x resample backwards (-100%,   0st)
		1.5  -> -1.5,   1.5  * buffer.size -- 3/2x resample backwards (-150%,  +6st)
		2.0  -> -2.0,   2.0  * buffer.size --   2x resample backwards (-200%, +12st)
	--]]

	assert(type(ratio) == 'number',
		"Resampling ratio must be a number.")

	instance.resampleRatio = ratio

	calculatePlaybackCoefficients(instance)
end

function ASource.getTimeStretch(instance)
	return instance.timeDilation
end

function ASource.setTimeStretch(instance, ratio)

	-- range: (,)
	-- stretch amount saved internally
	-- amount sets second value only
	-- amount's sign applied to first value (* amount>=0 and 1.0 or -1.0)
	-- above works for 0 as well

	--[[
		N/A,  2.0  -> +any,  -2.0  * buffer.size -- 200% playback forwards
		N/A,  1.5  -> +any,  -1.5  * buffer.size -- 150% playback forwards
		N/A,  1.0  -> +any,  -1.0  * buffer.size -- 100% playback forwards
		N/A,  0.75 -> +any,  -0.75 * buffer.size --  75% playback forwards
		N/A,  0.5  -> +any,  -0.5  * buffer.size --  50% playback forwards
		N/A,  0.25 -> +any,  -0.25 * buffer.size --  25% playback forwards
		----------------------------------------
		N/A,  0.0  -> +any,   0.0  * buffer.size --   0% playback forwards  (stutter)
		N/A,  0.0  -> -any,   0.0  * buffer.size --   0% playback backwards (stutter)
		----------------------------------------
		N/A, -0.25 -> -any,   0.25 * buffer.size --  25% playback backwards
		N/A, -0.5  -> -any,   0.5  * buffer.size --  50% playback backwards
		N/A, -0.75 -> -any,   0.75 * buffer.size --  75% playback backwards
		N/A, -1.0  -> -any,   1.0  * buffer.size -- 100% playback backwards
		N/A, -1.5  -> -any,   1.5  * buffer.size -- 150% playback backwards
		N/A, -2.0  -> -any,   2.0  * buffer.size -- 200% playback backwards
	--]]

	assert(type(ratio) == 'number',
		"Time stretching ratio must be a number.")

	instance.timeDilation = ratio

	calculatePlaybackCoefficients(instance)
end

-- Effects related
function ASource.getEffect(instance, ...)
	return instance.source:getEffect(...)
end

function ASource.setEffect(instance, ...)
	return instance.source:setEffect(...)
end

function ASource.getFilter(instance, ...)
	return instance.source:getFilter(...)
end

function ASource.setFilter(instance, ...)
	return instance.source:setFilter(...)
end

-- Spatial functionality
function ASource.getAirAbsorption(instance, ...)
	return instance.source:getAirAbsorption(...)
end
function ASource.setAirAbsorption(instance, ...)
	return instance.source:setAirAbsorption(...)
end
function ASource.getAttenuationDistances(instance, ...)
	return instance.source:getAttenuationDistances(...)
end
function ASource.setAttenuationDistances(instance, ...)
	return instance.source:setAttenuationDistances(...)
end
function ASource.getCone(instance, ...)
	return instance.source:getCone(...)
end
function ASource.setCone(instance, ...)
	return instance.source:setCone(...)
end
function ASource.getDirection(instance, ...)
	return instance.source:getDirection(...)
end
function ASource.setDirection(instance, ...)
	return instance.source:setDirection(...)
end
function ASource.getPosition(instance, ...)
	return instance.source:getPosition(...)
end
function ASource.setPosition(instance, ...)
	return instance.source:setPosition(...)
end
function ASource.getRolloff(instance, ...)
	return instance.source:getRolloff(...)
end
function ASource.setRolloff(instance, ...)
	return instance.source:setRolloff(...)
end
function ASource.getVelocity(instance, ...)
	return instance.source:getVelocity(...)
end
function ASource.setVelocity(instance, ...)
	return instance.source:setVelocity(...)
end
function ASource.isRelative(instance, ...)
	return instance.source:isRelative(...)
end
function ASource.setRelative(instance, ...)
	return instance.source:setRelative(...)
end
function ASource.getVolume(instance, ...)
	return instance.source:getVolume(...)
end
function ASource.setVolume(instance, ...)
	return instance.source:setVolume(...)
end
function ASource.getVolumeLimits(instance, ...)
	return instance.source:getVolumeLimits(...)
end
function ASource.setVolumeLimits(instance, ...)
	return instance.source:setVolumeLimits(...)
end

-- Object super overrides
function ASource.release(instance)
	-- Clean up the whole ASource, not just the internal Source object.
	local id = instance.id
	if instance.data then instance.data:release() end
	instance.buffer:release()
	instance.source:release()
	for k,v in pairs(instance) do k = nil end
	table.remove(ASList, id)
end

function ASource.type(instance)
	return 'ASource'
end

function ASource.typeOf(instance, type)
	if type == 'ASource' or type == 'Source' or type == 'Object' then
		return true
	end
	return false
end

-- Metatable
local mtASource = {__index = function(instance, method)
	if ASource[method] then
		return ASource[method]
	else
		if instance.source[method] then
			return instance.source[method]
		end
	end
end}

-- Generic constructor:
-- - SourceType type, String path
-- - SourceType type, File file
-- - Decoder decoder
-- - SoundData sounddata
-- - Number samplerate, Number bitdepth, Number channels, Number buffercount
new = function(a, b, c, d)

	-- Create instance.
	local asource = {}

	-- Set initial values.
	asource.orig           =   nil -- used for cloning (deep-copying) internal SD or DC objects...
	asource.type           =   nil

	asource._isPlaying     = false

	asource.bitDepth       =     8
	asource.bufferCount    =     8 -- OALS internal buffer count; unrelated to most things here.
	asource.channelCount   =     1
	asource.samplingRate   =  8000

	asource.bufferSize     =  2048 -- Seems optimal for time stretching w/ a samp.rate of 44.1kHz.
	asource.pitchShift     =     1 -- 1 means no pitch modification; tied with parameter below.
	asource.pitchShiftSt   =     0 -- 0 means no semitone offset; tied with parameter above.
	asource.resampleRatio  =     1 -- 1 means regular rate.
	asource.timeDilation   =     1 -- 1 means regular forward playback.

	asource.innerOffset    =     1 -- Rate of samplepoint advancement in one buffer.
	asource.outerOffset    =    -1 -- Rate of buffer advancement.

	asource.pointer        =     0 -- Samplepoint offset into the full track.
	asource.looping        = false
	asource.startpoint     =     0 -- In samplepoints.
	asource.endpoint       =     0 -- In samplepoints.

	-- Handle specific cases just as how vanilla Source objects are handled.
	if type(a) == 'string' then
		assert(SourceType[a],
			("Given SourceType parameter %s not supported"):format(tostring(a)))

		if (type(b) == 'string') or
			(b.type and (b:type() == 'File' or b:type() == 'DroppedFile')) then
			if a == 'static' then
				asource._type = 'static'
				asource.data = love.sound.newSoundData(b)
				asource.orig = b
			elseif a == 'stream' then
				asource._type = 'stream'
				asource.data = love.sound.newDecoder(b)
				asource.orig = b
			else
				error("Queueable Sources can't be created from file or filepath.")
			end

		end
	elseif a.type and a:type() == 'SoundData' then -- shallow copy; uses the same SoundData object!
		asource._type = 'static'
		asource.data = a
		asource.orig = asource.data
	elseif a.type and a:type() == 'Decoder' then -- shallow copy for now; uses the same Decoder!
		asource._type = 'stream'
		asource.data = a -- Waiting on löve to expose Decoder:clone, because race conditions!
		asource.orig = nil
	elseif type(a) == 'number' then
		asource._type = 'queue'
		asource.orig = nil
		asource.samplingRate = a or asource.samplingRate
		asource.bitDepth =     b or asource.bitDepth
		asource.channelCount = c or asource.channelCount
		asource.bufferCount =  d or asource.bufferCount
	else 
		error(("Given parameter %s not suppoted"):format(tostring(a)))
	end

	-- Store data parameters, if extant.
	if asource.data then
		asource.samplingRate = asource.data:getSampleRate()
		asource.bitDepth     = asource.data:getBitDepth()
		asource.channelCount = asource.data:getChannelCount()

		-- Try setting the default loop endpoint to the end of the waveform.
		if asource._type == 'static' then
			asource.endpoint = asource.data:getSampleCount()
		elseif asource._type == 'stream' then
			asource.endpoint = asource.data:getDuration() * asource.samplingRate
		end
	end

	-- Create internal Qsource.
	asource.source = love.audio.newQueueableSource(
		asource.samplingRate,
		asource.bitDepth,
		asource.channelCount,
		asource.bufferCount)

	-- Create internal Buffer.
	asource.buffer = love.sound.newSoundData(
		asource.bufferSize,
		asource.samplingRate,
		asource.bitDepth,
		asource.channelCount)

	-- Set up more internals.
	calculatePlaybackCoefficients(asource)

	-- Make this work more or less like a regular source.
	setmetatable(asource, mtASource)

	-- Add source to internal list
	asource.id = #ASList+1
	ASList[asource.id] = asource

	--print 'Source object hopefully created neatly.'
	
	--------------
	return asource.id
end

-------------------------------

--print "procthread exists!"

while true do

	-- Handle messages in inbound queue. Atomic ops in the other threads should guarantee ordering
	-- of elements.
	local data = toProc:pop()

	if data then

		-- First value is always a string
		if type(data) == 'string' then

			if data == 'procThread!' then

				-- Store the next value in queue as this thread's memory address.
				procThread = toProc:pop()

			elseif data == 'procThread?' then

				-- Send back this thread's memory address using the channel which is the next value
				-- in the queue.
				local ch = toProc:pop()
				ch:push(procThread)

			elseif data == 'new' then

				-- Construct a new ASource using parameters popped from the inbound queue.
				-- ASource objects themselves store the inbound channel of the thread that
				-- requested them.
				local ch, a, b, c, d, id
				ch = toProc:pop()
				a  = toProc:pop()
				b  = toProc:pop()
				c  = toProc:pop()
				d  = toProc:pop()

				id = new(a, b, c, d)

				-- Send back the ASource object's index through the relevant thread's inbound
				-- channel.
				ch:push(id)

			elseif ASource[data] then

				-- Getters/Setters
				-- <methodName> (above), <ch>, <id>, <paramCount>, <parameter1>, ..., <parameterN>
				local ch = toProc:pop()
				local id = toProc:pop()
				local paramCount = toProc:pop()
				local params = {}
				if paramCount > 0 then
					for i=1, paramCount do
						params[i] = toProc:pop()
					end
				end

				local obj
				for i=1, #ASList do if ASList[i].id == id then obj = ASList[i] break end end

				local retval = {obj[data](obj, unpack(params))}

				ch:performAtomic(function(ch)
					ch:push(#retval)
					if #retval > 0 then
						for i=1, #retval do
							ch:push(retval[i])
						end
					end
				end)
			end
		end
	end

	-- Update active ASources.
	for i=1, #ASList do ASList[i]:update() end

	-- Don't hog a core.
	love.timer.sleep(0.002)
end