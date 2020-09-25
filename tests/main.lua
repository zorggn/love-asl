local ASource = require 'asl.asl'
local soundData = love.sound.newSoundData('generator.ogg')
local source = ASource(soundData)
source:play()

source:seek(1) -- Theoretically this should try to seek to 1 seconds, but hangs the process.
--source:seek(225183, 'samples') -- This works.