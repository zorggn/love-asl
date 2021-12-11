-- Advanced Source Library
-- Processing Thread
-- by zorg § ISC @ 2018-2021

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
local BufferUnit = {['milliseconds'] = true, ['samples']   = true}

local PanLaws = {
	gain  = function(pan) return                    1.0  - pan,                           pan end,
	power = function(pan) return math.cos(math.pi / 2.0) * pan, math.sin(math.pi / 2.0) * pan end
}

local ItpMethod  = {[0] = 'nearest', 'linear', 'cubic', 'sinc'}
local iItpMethod = {}; for i=0,#ItpMethod do iItpMethod[ItpMethod[i]] = i end

local function lanczos_window(x,a)
	if x == 0 then
		return 1 -- division by zero protection.
	elseif x >= -a and x < a then
		return (a * math.sin(math.pi * x) * math.sin(math.pi * x / a)) / (math.pi ^ 2 * x ^ 2)
	else
		return 0 -- brickwall edges outside the region we're using.
	end
end

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
	if instance.channelCount == 1 then
		for i=0, buffer:getSampleCount()-1 do
			instance.buffer:setSample(i, buffer:getSample(i))
		end
	else--if instance.channelCount == 2 then

		-- We need to apply our own supported effects on the queue type as well... at least those that we can.
		-- Currently, that means Stereo Separation and Stereo Panning.
		-- Time Stretching and Pitch Shifting might be possible, but AFAIK the push-style API just can't handle arbitrary buffer shrinking/expanding.

		-- Stereo Panning implementation #1
		local pan = {}
		pan[1], pan[2] = instance.panlawfunc(instance.pan)

		for i=0, buffer:getSampleCount()-1 do
			-- No need to interpolate due to TS/PS functionality not being available...
			local smpL, smpR = buffer:getSample(i, 1), buffer:getSample(i, 2)

			-- Stereo Separation implementation
			local M = (smpL + smpR) * 0.5
			local S = (smpL - smpR) * 0.5
			smpL = M + S * instance.separation
			smpR = M - S * instance.separation

			-- Stereo Panning implementation #2
			smpL = smpL * pan[1]
			smpR = smpR * pan[2]

			-- Clamp for safety
			smpL = math.min(math.max(smpL, -1), 1)
			smpR = math.min(math.max(smpR, -1), 1)

			-- Finalize
			instance.buffer:setSample(i, 1, smpL)
			instance.buffer:setSample(i, 2, smpR)
		end
	end

	instance.source:queue(instance.buffer)
	-- No play call here; vanilla QSources didn't automatically play either.

	return true
end

-- Defining generator functions that fill the internal Buffer object.
local Generator = {}

Generator.static = function(instance)

	if not instance._isPlaying then -- Fill buffer with silence.
		for i=0, instance.frameSize-1 do
			for ch=1, instance.channelCount do
				instance.buffer:setSample(i, ch, 0.0)
			end
		end
	else -- Fill buffer with calculated data.
		local ptr = instance.pointer               -- Current offset in source SoundData (double)
		local N   = instance.data:getSampleCount() -- Length of source SoundData

		-- Calculate next buffer offset.
		local frame_offset = instance.frameSize * instance.outerOffset

		-- Copy samplepoints to buffer.
		for i=0, instance.frameSize-1 do

			-- Samplepoint blend amount normalized to [0,1] range across the window.
			local mix = i / (instance.frameSize-1)

			-- Calculate next samplepoint offset.
			local next_smp = i * instance.innerOffset

			if instance.channelCount == 1 then
				-- Get the correct samplepoints for mixing purposes, with the chosen interpolation method.
				local A,B = 0.0, 0.0

				-------------------------------------------------------------------------------------------------
				--/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\--
				-------------------------------------------------------------------------------------------------

				-- Interpolate source samplepoints.
				if instance.interpolation == 0 then
					-- Nearest-Neighbor
					local offset
					offset = math.floor(ptr + 0.5 + next_smp) % N
					A = instance.data:getSample(offset)
					offset = math.floor(ptr + 0.5 + next_smp + frame_offset) % N
					B = instance.data:getSample(offset)
				elseif instance.interpolation == 1 then
					-- Linear
					local offset
					local int,frac
					offset = ptr + next_smp
					int    = math.floor(offset)
					frac   = offset - int
					A = instance.data:getSample( int   %N) * (1.0 - frac) +
					    instance.data:getSample((int+1)%N) * frac
					offset = ptr + next_smp + frame_offset
					int    = math.floor(offset)
					frac   = offset - int
					B = instance.data:getSample( int   % N) * (1.0 - frac) +
					    instance.data:getSample((int+1)% N) * frac
				elseif instance.interpolation == 2 then
					-- Cubic Hermite Spline
					-- https://blog.demofox.org/2015/08/08/cubic-hermite-interpolation/
					-- (With some minor simplifications)
					local offset
					local int,frac
					local x,y,z,w
					local X,Y,Z,W
					offset = ptr + next_smp
					int    = math.floor(offset)
					frac   = offset - int
					x = instance.data:getSample(math.floor(ptr-1 + next_smp) % N)
					y = instance.data:getSample(math.floor(ptr   + next_smp) % N)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp) % N)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp) % N)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					A = X*frac^3 + Y*frac^2 + Z*frac + W
					offset = ptr + next_smp + frame_offset
					int    = math.floor(offset)
					frac   = offset - int
					x = instance.data:getSample(math.floor(ptr-1 + next_smp + frame_offset) % N)
					y = instance.data:getSample(math.floor(ptr   + next_smp + frame_offset) % N)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp + frame_offset) % N)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp + frame_offset) % N)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					B = X*frac^3 + Y*frac^2 + Z*frac + W
				else--if instance.interpolation == 3 then
					-- Lanczos (Sinc) variants: 32 taps (not counting the main lobe's tap)
					-- TODO: This still rings more than the cubic, probably an error in implementation.
					local taps  = 32
					local result = 0.0
					local x = (ptr + next_smp) % N
					for l = -(taps/2), (taps/2) do
						local i = math.floor(ptr+l + next_smp) % N
						result = result + instance.data:getSample(i) * lanczos_window(x - i, taps)
					end
					A = result
					local result = 0.0
					local x = (ptr + next_smp + frame_offset) % N
					for l = -(taps/2), (taps/2) do
						local i = math.floor(ptr+l + next_smp + frame_offset) % N
						result = result + instance.data:getSample(i) * lanczos_window(x - i, taps)
					end
					B = result
				end

				-------------------------------------------------------------------------------------------------
				--/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\--
				-------------------------------------------------------------------------------------------------

				-- Apply attenuation to result in a linear mix through the buffer.
				A = A * (1.0 - mix)
				B = B * (      mix)

				-- Set the samplepoint value to be the sum of the above two,
				-- with clamping for safety.
				instance.buffer:setSample(i, math.min(math.max(A+B, -1.0), 1.0))

			else -- if instance.channelCount == 2 then
				-- Get the correct samplepoints for mixing purposes, with the chosen interpolation method.
				local AL, AR, BL, BR = 0.0, 0.0, 0.0, 0.0
				local L,R = 0.0, 0.0

				-------------------------------------------------------------------------------------------------
				--/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\--
				-------------------------------------------------------------------------------------------------

				-- Interpolate source samplepoints.
				if instance.interpolation == 0 then
					-- Nearest-Neighbor
					local offset
					offset = math.floor(ptr + 0.5 + next_smp) % N
					AL = instance.data:getSample(offset, 1)
					AR = instance.data:getSample(offset, 2)
					offset = math.floor(ptr + 0.5 + next_smp + frame_offset) % N
					BL = instance.data:getSample(offset, 1)
					BR = instance.data:getSample(offset, 2)
				elseif instance.interpolation == 1 then
					-- Linear
					local offset
					local int,frac
					offset = ptr + next_smp
					int    = math.floor(offset)
					frac   = offset - int
					AL = instance.data:getSample( int   %N, 1) * (1.0 - frac) +
					     instance.data:getSample((int+1)%N, 1) * frac
					AR = instance.data:getSample( int   %N, 2) * (1.0 - frac) +
					     instance.data:getSample((int+1)%N, 2) * frac
					offset = ptr + next_smp + frame_offset
					int    = math.floor(offset)
					frac   = offset - int
					BL = instance.data:getSample( int   %N, 1) * (1.0 - frac) +
					     instance.data:getSample((int+1)%N, 1) * frac
					BR = instance.data:getSample( int   %N, 2) * (1.0 - frac) +
					     instance.data:getSample((int+1)%N, 2) * frac
				elseif instance.interpolation == 2 then
					-- Cubic Hermite Spline
					-- https://blog.demofox.org/2015/08/08/cubic-hermite-interpolation/
					-- (With some minor simplifications)
					local offset
					local int,frac
					local x,y,z,w
					local X,Y,Z,W
					offset = ptr + next_smp
					int    = math.floor(offset)
					frac   = offset - int
					x = instance.data:getSample(math.floor(ptr-1 + next_smp) % N, 1)
					y = instance.data:getSample(math.floor(ptr   + next_smp) % N, 1)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp) % N, 1)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp) % N, 1)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					AL = X*frac^3 + Y*frac^2 + Z*frac + W
					x = instance.data:getSample(math.floor(ptr-1 + next_smp) % N, 2)
					y = instance.data:getSample(math.floor(ptr   + next_smp) % N, 2)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp) % N, 2)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp) % N, 2)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					AR = X*frac^3 + Y*frac^2 + Z*frac + W
					offset = ptr + next_smp + frame_offset
					int    = math.floor(offset)
					frac   = offset - int
					x = instance.data:getSample(math.floor(ptr-1 + next_smp + frame_offset) % N, 1)
					y = instance.data:getSample(math.floor(ptr   + next_smp + frame_offset) % N, 1)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp + frame_offset) % N, 1)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp + frame_offset) % N, 1)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					BL = X*frac^3 + Y*frac^2 + Z*frac + W
					x = instance.data:getSample(math.floor(ptr-1 + next_smp + frame_offset) % N, 2)
					y = instance.data:getSample(math.floor(ptr   + next_smp + frame_offset) % N, 2)
					z = instance.data:getSample(math.floor(ptr+1 + next_smp + frame_offset) % N, 2)
					w = instance.data:getSample(math.floor(ptr+2 + next_smp + frame_offset) % N, 2)
					X = -(x * 0.5) + (y * 1.5) - (z * 1.5) + w * 0.5
					Y =  (x      ) - (y * 2.5) + (z * 2.0) - w * 0.5
					Z = -(x * 0.5)             + (z * 0.5)
					W =               y
					BR = X*frac^3 + Y*frac^2 + Z*frac + W
				else--if instance.interpolation == 3 then
					-- Lanczos (Sinc) variants: 32 taps (not counting the main lobe's tap)
					-- TODO: This still rings more than the cubic, probably an error in implementation.
					local taps  = 32
					local resultL, resultR = 0.0, 0.0
					local x = (ptr + next_smp) % N
					for l = -(taps/2), (taps/2) do
						local i = math.floor(ptr+l + next_smp) % N
						resultL = resultL + instance.data:getSample(i,1) * lanczos_window(x - i, taps)
						resultR = resultR + instance.data:getSample(i,2) * lanczos_window(x - i, taps)
					end
					AL, AR = resultL, resultR

					resultL, resultR = 0.0, 0.0
					local x = (ptr + next_smp + frame_offset) % N
					for l = -(taps/2), (taps/2) do
						local i = math.floor(ptr+l + next_smp + frame_offset) % N
						resultL = resultL + instance.data:getSample(i,1) * lanczos_window(x - i, taps)
						resultR = resultR + instance.data:getSample(i,2) * lanczos_window(x - i, taps)
					end
					BL, BR = resultL, resultR
				end

				-------------------------------------------------------------------------------------------------
				--/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\--
				-------------------------------------------------------------------------------------------------

				-- Apply attenuation to result in a linear mix through the buffer.
				AL, AR = AL * (1.0 - mix), AR * (1.0 - mix)
				BL, BR = BL * (      mix), BR * (      mix)

				-- Sum the to be mixed values.
				L, R = AL+BL, AR+BR

				-- Implement stereo separation.
				local M = (L + R) * 0.5
				local S = (L - R) * 0.5
				L = M + S * instance.separation
				R = M - S * instance.separation

				-- Implement stereo panning.
				L = L * instance._panL
				R = R * instance._panR

				-- Set the samplepoint value with clamping for safety.
				instance.buffer:setSample(i, 1, math.min(math.max(L, -1.0), 1.0))
				instance.buffer:setSample(i, 2, math.min(math.max(R, -1.0), 1.0))
			end

			-- Calculate the next sample offset for loop processing.
			ptr = ptr + instance.innerOffset

			if not instance.looping then
				if ptr >= N or ptr < 0 then
					-- Fill the rest of the buffer with silence
					-- if the pointer went out of bounds.
					for j=i+1, instance.frameSize-1 do
						for ch=1, instance.channelCount do
							instance.buffer:setSample(j, ch, 0.0)
						end
					end
					-- Also we stop playback.
					instance:stop()
				end
			else -- if instance.looping then
				-- Simplistic implementation, works in most situations;
				-- Probably could be implemented with modular arithmetic instead.
				-- If next samplepoint offset is out of loop bounds, move it so it isn't.
				if instance.timeDilation >= 0 then
					while ptr > instance.endpoint do
						ptr = ptr - (instance.endpoint - instance.startpoint)
					end
				else
					while ptr < instance.startpoint do
						ptr = ptr + (instance.endpoint - instance.startpoint)
					end
				end
			end
			-- Remove the offset we added before wrapping.
			ptr = (ptr - instance.innerOffset) % N
		end

		-- Calculate next buffer offset
		instance.pointer = instance.pointer + instance.frameSize * instance.timeDilation * math.abs(instance.resampleRatio)

		-- Handle looping behaviour.
		if instance.looping then
			-- Simplistic implementation, works in most situations;
			-- Probably could be implemented with modular arithmetic instead.
			-- If next buffer offset is out of loop bounds, move it so it isn't.
			if instance.timeDilation >= 0 then
				while instance.pointer > instance.endpoint do
					instance.pointer = instance.pointer - (instance.endpoint - instance.startpoint)
				end
			else
				while instance.pointer < instance.startpoint do
					instance.pointer = instance.pointer + (instance.endpoint - instance.startpoint)
				end
			end
		end

		-- Wrap pointer value based on input.
		instance.pointer = instance.pointer % N
	end

	-- Static source type -> needs queueing and playing
	instance.source:queue(instance.buffer, instance.frameSize * (instance.bitDepth/8) * instance.channelCount)
	instance.source:play()
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

	--instance.source:queue(instance.buffer)
	--instance.source:play()
end

Generator.queue  = function(instance) -- TODO
	-- This actually just calls a pull-styled callback; deferring the heavy stuff to the user,
	-- but only if the user redefined Source:queue to be a callback (i.e. the function's memory
	-- address is different.)

	-- We need to apply our own supported effects on the queue type as well... at least those that we can.
	-- Currently, that means Stereo Separation and Stereo Panning.
	-- Time Stretching and Pitch Shifting might be possible, but AFAIK the push-style API just can't handle arbitrary buffer shrinking/expanding.

	if instance._type == 'queue' then
		if instance.queue ~= Queue then
			-- This has issues with the threaded approach, since we can't use channels to pass lua 
			-- functions through.
			instance.queue(instance.buffer) -- Callback that can be anywhere...

			-- This will bog the processing down a smidge, for stereo sources, that is.
			if instance.channelCount == 2 then

				-- We need to apply our own supported effects on the queue type as well... at least those that we can.
				-- Currently, that means Stereo Separation and Stereo Panning.
				-- Time Stretching and Pitch Shifting might be possible, but AFAIK the push-style API just can't handle arbitrary buffer shrinking/expanding.

				-- Stereo Panning implementation #1
				local pan = {}
				pan[1], pan[2] = instance.panlawfunc(instance.pan)

				for i=0, instance.frameSize-1 do
					-- No need to interpolate due to TS/PS functionality not being available...
					local smpL, smpR = instance.buffer:getSample(i, 1), instance.buffer:getSample(i, 2)

					-- Stereo Separation implementation
					local M = (smpL + smpR) * 0.5
					local S = (smpL - smpR) * 0.5
					smpL = M + S * instance.separation
					smpR = M - S * instance.separation

					-- Stereo Panning implementation #2
					smpL = smpL * pan[1]
					smpR = smpR * pan[2]

					-- Clamp for safety
					smpL = math.min(math.max(smpL, -1), 1)
					smpR = math.min(math.max(smpR, -1), 1)

					-- Finalize
					instance.buffer:setSample(i, 1, smpL)
					instance.buffer:setSample(i, 2, smpR)
				end
			end

			instance.source:queue(instance.buffer, instance.frameSize * (instance.bitDepth/8) * instance.channelCount)
			instance.source:play()
		--else-- if instance.queue == Queue then
			-- Data already pushed, processed, enqueued and set to play.
		end
	end
end



-- Makes pitch shifting and time stretching possible.
local function calculatePlaybackCoefficients(instance)
	instance.innerOffset = instance.resampleRatio                                                             * instance.pitchShift * math.sgn(instance.timeDilation)
	instance.outerOffset = instance.resampleRatio * (instance.timeDilation * math.sgn(instance.resampleRatio) - instance.pitchShift * math.sgn(instance.timeDilation))
end

local function calculatePanningCoefficients(instance)
	if instance.channelCount == 2 then
	instance._panL, instance._panR = instance.panlawfunc(instance.pan)
	end
end

-------------------------------

-- Class
local ASource = {}

-- Method called in this thread automatically.
function ASource.update(instance, dt)
	while instance.source:getFreeBufferCount() > 0 do
		Generator[instance._type](instance)
	end
end

-- Copy-constructor
function ASource.clone(instance)
	local clone = {}

	for k,v in pairs(instance) do
		clone[k] = v
	end

	-- Needs to be in the stopped state by default.
	clone._isPlaying = false

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
	calculatePanningCoefficients(clone)

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
	instance.pointer = instance.timeDilation >= 0 and 0 or math.max(0, instance.data:getSampleCount()-1)
	return true
end

function ASource.stop(instance)
	instance:pause()
	instance:rewind()
	return true
end

function ASource.isPlaying(instance)
	-- To those that miss the below functions:
	-- - isPaused  = not isPlaying
	-- - isStopped = not isPlaying and (tell() == 0 or tell() == math.max(0, getSampleCount()-1))
	return instance._isPlaying
end

function ASource.seek(instance, position, unit)
	-- TODO: Can we really not? We could mess with the AS object's buffer pointer...
	assert(instance._type ~= 'queue',
		"Can't seek Queuable sources.")

	unit = unit or 'seconds'

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

	unit = unit or 'seconds'

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
	unit = unit or 'seconds'

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


function ASource.getBufferSize(instance, unit)
	unit = unit or 'milliseconds'

	assert(BufferUnit[unit],
		"Unsupported BufferUnit: " .. tostring(unit))

	if unit == 'samples' then
		return instance.frameSize
	else--if unit == 'milliseconds' then
		return instance.frameSize / instance.samplingRate * 1000
	end
end

function ASource.setBufferSize(instance, amount, unit)
	unit = unit or 'milliseconds'

	assert(BufferUnit[unit],
		"Unsupported BufferUnit: " .. tostring(unit))

	if unit == 'samples' then
		assert(type(amount) == 'number' and amount > 0 and amount <= 65536 and amount == math.floor(amount),
			"Buffer size in samplepoints must be given as a positive nonzero integer, 65536 at most.")
		instance.frameSize = amount
	else--if unit == 'milliseconds' then
		assert(type(amount) == 'number' and amount * instance.samplingRate * 0.001 > 0.0 and amount * instance.samplingRate * 0.001 <= 65536,
			"Buffer size in milliseconds must be given as a positive nonzero number, equivalent to 65536 samplepoints at most.")
		instance.frameSize = math.floor(amount * instance.samplingRate * 0.001 + 0.5)
	end
end

-- Time-domain manipulation
function ASource.getPitchShift(instance, unit)
	unit = unit or 'ratio'

	assert(PitchUnit[unit],
		("Pitch shift unit %s unsupported."):format(tostring(unit)))

	if unit == 'ratio' then
		return instance.pitchShift
	else
		return instance.pitchShiftSt
	end
end

function ASource.setPitchShift(instance, amount, unit)

	-- range: (0,)
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

	unit = unit or 'ratio'

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



-- Interpolation related
function ASource.getInterpolationMethod(instance)
	return ItpMethod[instance.interpolation]
end

function ASource.setInterpolationMethod(instance, method)
	assert(iItpMethod[method],
		("Interpolation method %s unsupported."):format(tostring(method)))
	instance.interpolation = iItpMethod[method]
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

function ASource.getActiveEffects(instance, ...)
	return instance.source:getActiveEffects(...)
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
function ASource.getVolumeLimits(instance, ...)
	return instance.source:getVolumeLimits(...)
end
function ASource.setVolumeLimits(instance, ...)
	return instance.source:setVolumeLimits(...)
end
function ASource.getVolume(instance, ...)
	return instance.source:getVolume(...)
end
function ASource.setVolume(instance, ...)
	return instance.source:setVolume(...)
end

-- Stereo Panning related
function ASource.getPanning(instance)
	return instance.pan
end

function ASource.setPanning(instance, balance)
	if balance < 0.0 or balance > 1.0 then
		error "Panning values must be between 0 and 1!"
	end
	instance.pan = balance
	calculatePanningCoefficients(instance)
end

function ASource.getPanLaw(instance)
	return instance.panlaw == 'custom' and instance.panlawfunc or instance.panlaw
end

function ASource.setPanLaw(instance, law)
	if type(law) == 'string' and not (law == 'gain' or law == 'power') then
		error "Given panning law string not supported; must be 'gain' or 'power'!"
	end
	if type(law) == 'function' then
		-- Test given function with some inputs
		for i,v in ipairs{0.00, 0.25, 0.33, 0.50, 0.67, 1.00} do
			local ok, l,r = pcall(law, v)
			if not ok then
				error(("The given pan law function is errorenous: %s"):format(l))
			else
				if type(l) ~= 'number' or type(r) ~= 'number' then
					error "The given pan law function must return two numbers!"
				else
					if l < 0.0 or l > 1.0 or r < 0.0 or r > 1.0 then
						error "The given pan law function's return values must be between 0.0 and 1.0!"
					end
				end
			end
		end
		instance.panlaw = 'custom'
		instance.panlawfunc = law
	else --if type(law) == 'string' then
		instance.panlaw = law
		instance.panlawfunc = PanLaws[law]
	end
	calculatePanningCoefficients(instance)
end

-- Stereo Separation related
function ASource.getStereoSeparation(instance)
	return instance.separation
end

function ASource.setStereoSeparation(instance, ssep)
	if ssep < 0.0 or ssep > 1.0 then
		error "Stereo Separation values must be between 0 and 1!"
	end
	instance.separation = ssep
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

	asource.bufferSize     = 65536 -- Set to maximum allowed value, never changed.
	asource.frameSize      =   400 -- 50 ms by default, this is a placeholder value.
	asource.pitchShift     =     1 -- 1 means no pitch modification; tied with parameter below.
	asource.pitchShiftSt   =     0 -- 0 means no semitone offset; tied with parameter above.
	asource.resampleRatio  =     1 -- 1 means regular rate.
	asource.timeDilation   =     1 -- 1 means regular forward playback. (TODO: Rename to stretch?)
	asource.interpolation  =     1 -- see at the top for the enums.

	asource.innerOffset    =     1 -- Rate of samplepoint advancement in one buffer.
	asource.outerOffset    =    -1 -- Rate of buffer advancement.
	
	asource._panL          =   0.5 -- Intermediate values for panning adjustment
	asource._panR          =   0.5 -- -"-

	asource.pointer        =     0 -- Samplepoint offset into the full track.
	asource.looping        = false
	asource.startpoint     =     0 -- In samplepoints.
	asource.endpoint       =     0 -- In samplepoints.

	asource.separation     =   1.0 -- Percentage control of Stereo Separation.
	asource.pan            =   0.5 -- Percentage control of Stereo Panning.
	asource.panlaw         = 'gain' -- Panning law to use; either the strings 'gain' or 'power', or 'custom' for an arbitrary function(pan) -> Lattenuation,Rattenuation
	asource.panlawfunc     = PanLaws.gain -- Hold onto the func ptr to make things simpler.

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
	elseif a.type and a:type() == 'Decoder' then -- deep copy; clones a new decoder.
		asource._type = 'stream'
		asource.data = a:clone()
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
	calculatePanningCoefficients(asource)
	asource.frameSize =  50.0 * asource.samplingRate * 0.001

	-- Make this work more or less like a regular source.
	setmetatable(asource, mtASource)

	-- Add source to internal list
	asource.id = #ASList+1
	ASList[asource.id] = asource
	
	--------------
	return asource.id
end

-------------------------------

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
