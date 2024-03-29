Advanced Source Library
----------------------------------------------------------

### Info

ASL is a threaded wrapper on Löve (Audio) Source objects; it adds extra functionality to them.

Currently supported Löve version(s): 11.3, 11.4 (current)

### Usage

- `local new = require 'asl'` which returns the constructor that makes ASource objects.

For those that want to, they can always add the function themselves into löve's own table structure:
- `love.audio.newAdvancedSource = require 'asl'`

### Considerations

- Since this library does monkeypatch a few things internally, please don't bug the löve2D developers with issues if you're using this library, before doing some due diligence making sure the error doesn't stem from this library (in which case, do create an issue here for me to solve!)

### Additions to Source functionality

- Threaded implementation, can be used from multiple threads as well, it will ever only use one internal processing thread.
- QueueableSource and Buffer-based timing for accurate playback and position tracking.
- Implements custom loop points.
- Implements panning and stereo separation support.
- Implements time-domain time-stretching and pitch-shifting methods.
- Implements dynamic buffer resizing based on uniform noise to mitigate specific harmonic distortion noise, for convenience.
- Implements reverse playback.
- Implements different interpolation methods during modified playback (resampled, stretched, and/or shifted)
- Implements the ability to use stereo sound files as mono, for simpler 3D spatialization support.
- All methods that don't return any values are chainable.

### API Changes

#### Deprecations
- `Source:getPitch` is now `Source:getResamplingRatio`.
- `Source:setPitch` is now `Source:setResamplingRatio`.

#### Additions
- `Source:rewind` re-added, which is syntax sugar for `Source:seek(0)` or `Source:seek(Source:getSampleCount()-1`, depending on playback direction.

- `Source:getLoopPoints` added, returns `startpoint` and `endpoint`, in samplepoints.
- `Source:setLoopPoints` added, with parameters `startpoint` and `endpoint`; in samplepoints.

- `Source:getBitDepth` and `Source:getSampleRate` added.

- `Source:getBufferSize` added, with parameter `unit`; in either `samples`(samplepoints) or `milliseconds`, the latter being default.
- `Source:setBufferSize` added, with parameters `amount` and `unit`; in either `samples`(samplepoints) or `milliseconds`, the latter being default; buffer sizes can be between 1 milliseconds and 10 seconds long; default is ~50 ms equivalent, rounded down.

- `Source:getPitchShift` added, with parameter `unit`; in either as a non-negative `ratio`, or in `semitones`.
- `Source:setPitchShift` added, with parameters `amount` and `unit`; in either as a non-negative `ratio`, or in `semitones`; modifies pitch only.

- `Source:getResamplingRatio` added.
- `Source:setResamplingRatio` added, with parameter `ratio` as a ratio; modifies both playback speed and pitch.

- `Source:getTimeStretch` added.
- `Source:setTimeStretch` added, with parameter `ratio` as a ratio; modifies playback speed only.

- `Source:getInterpolationMethod` added.
- `Source:setInterpolationMethod` added, with parameter `method`; which can be one of the following strings: `nearest, linear, cubic, sinc`.

- `Source:getPanning` added.
- `Source:setPanning` added, with parameter `pan`; 0.0 is full left, 1.0 is full right, 0.5 is centered; the curve is defined by the specific panning law selected. Default is `0.5`.

- `Source:getPanLaw` added.
- `Source:setPanLaw` added, with parameter `law`; which can be one of the following strings: `gain`, `power` or a custom function taking a number within the range [0,1] as input, and returning two numbers in the domain [0,1] that scale the left and right channels respectively. Default is `gain`.
The two laws are constant-gain/amplitude and constant-power/loudness laws, the first attentuates the volume at the center by -6dB (50%), the second only by -3dB (1/sqrt(2)).

- `Source:getStereoSeparation` added.
- `Source:setStereoSeparation` added, with parameter `amount`; -1.0 means mid channel output only, 0.0 means original, 1.0 means side channel output only. Default is `0.0`.

- `Source:getFrameSize` added, with parameter `unit`; in either `samples`(samplepoints) or `milliseconds`, the latter being default.
- `Source:setFrameSize` added, with parameters `amount` and `unit`; in either `samples`(samplepoints) or `milliseconds`, the latter being default; buffer sizes can be between 1 milliseconds and 10 seconds long; default is ~35 milliseconds.

- `Source:getFrameVariance` added, with parameter `unit`, in either `samples`(samplepoints), `milliseconds`, or as a `percentage`; milliseconds being default.
- `Source:setFrameVariance` added, with parameters `amount` and `unit`; in either `samples`(samplepoints), `milliseconds`, or as a `percentage`; milliseconds being default. Setting variance above 0 will vary the TSM frame size within the signed limits given, as long as they're within 1 millisecond and 10 seconds inclusive.

- `Source:getFrameVarianceDistribution` added.
- `Source:setFrameVarianceDistribution` added, with parameters `uniform` and `normal`, with the latter being default.

- `Source:getMixMethod` added.
- `Source:setMixMethod` added, with parameters `auto`, `linear`, `sqroot`, `cosine` and `noise`, with the first being default. It's best to leave this alone for most use-cases.

#### Modifications
- `Source:queue` may now be defined as a "pull-style" callback; if it isn't, it will work as the vanilla "push-style" method. (Note: queue type source support not yet implemented.)

- `Object:release` modified to release all extra internals of the new Objects; **must be called explicitly if one doesn't want dead objects cluttering up the processing thread.**
- `Object:type` modified to return the string `ASource`.
- `Object:typeOf` modified to also return true for `ASource` as a specialization of the `Source` type.

- The constructor function has different parameter ordering, and combines both `love.audio.newSource` for the first 5, and `love.audio.newQueueableSource` for the last variant:
	- string path, SourceType,            buffercount, aurality
	- File,        SourceType,            buffercount, aurality
	- FileData,    SourceType,            buffercount, aurality
	- Decoder,     SourceType,            buffercount, aurality
	- SoundData,                          buffercount, aurality
	- samplerate, bitdepth, channelCount, buffercount, aurality

where buffercount and aurality are optional parameters.

The buffercount parameter sets how many OpenAL-side buffers get made for the internal queueable source; less means less delay, but playback may underrun (it'll start to crackle).

The aurality parameter forces the internal QSource and buffers to be either mono or stereo, regardless of the channel count of the input itself; this means that ostensibly stereo data can also be used with 3D spatialization.

### Version History

#### V1.00 (2018.12.09)

	- Still available in the `non-threaded` branch.

#### V2.0 (2019.04.06)

	- Refactored lib to be threaded and thread-safe.

#### V2.1 (2021.07.03)

	- Added Stereo Panning implementation (direct method, not simulated by OpenAL's spatialization APIs.)

#### V2.2 (2021.07.07)

	- Added Stereo Separation implementation.

#### V2.2.1 (2021.08.15)

	- Some fixes regarding range checking and default values, one missing function added (getActiveEffects).

#### V3.0 (2021.12.11)

	- Complete reimplementation of time-scale modifications to achieve pop-less functionality; it still uses time-domain methods (time-scale modification and resampling) so depending on the settings, it might still sound weird.
	- Changed the behaviour of buffers; they are not recreated on size change anymore; the maximum sized ones will be created, and those can be limited to a smaller range instead.
	- Changed default buffer size to be equivalent to 50 ms.

	- Added interpolation methods for higher quality resampling, including cubic hermite spline and 32-tap lanczos sinc implementations.
	- Added milliseconds unit to the setter and getter of the buffer size.

#### V4.0 (2022.02.15)

	-- Refactored library code.

	- Added (via monkeypatching) support for love.audio.play/pause/stop to also handle ASources correctly with all variants supported.
	- Added methods to have an ASource dynamically change its effective buffer size by a given amount in a specificed +/- range based on an uniform distribution to get rid of specific tonal "blend-modulation" noise that happens at small buffer sizes combined with slowed playback.
	- Added dates to version history in this document.

	- Changed the constructor a fair bit, see the API changes section above.
	- Now possible to create either mono or stereo sources regardless of the input data channel count (meaning stereo data can be used with OALS' spatialization functionality, although this does come with internally mixing the two channels into one.)
	- Changed stereo separation limit from 100% to 200%; the latter half of the range will gradually remove the mid-channel.
	- Changed setLoopPoints method to accept partial parameters if one only wants to modify one of the values.
	- Changed loop handling to allow disjunct loop regions that wrap around the end/beginning of the data.
		- Also made sure initial playback and seeking to arbitrary places does not lock playback into the loop region; if such functionality is needed, seek into the loop region.
		- Note: Disjunt loop regions are currently buggy, this is a known issue and will be fixed.

	- Removed library adding itself to the love.audio table... it really shouldn't do that by itself unprompted.

	- Fixed clone method not quite working as expected.
	- Fixed bug with loop points not being correctly handled depending on buffer size and/or TSM parameters.
	- Fixed bug regarding the handling of individual proxy instances sometimes pointing to the wrong ones.

	- The library currently only supports creating "static" source types, and will error otherwise.

#### V4.1 (2022.02.26)

	- Fixed constructor bugs.
	- Fixed bugs relating to methods that the library doesn't override.
	- Fixed sinc interpolator ringing bug.

#### V4.2 (2022.04.20)

	- Added getMixMethod/setMixMethod to select how TSM frames are mixed together, either through linear or cosine interpolation (to preserve power); default setting is automatic method selection whether TSM is active, or if just resampling, even at +/- 100%.

	- Fixed disjunct loop region behaviour.

	- Made sure loop regions are not forcefully "jumped into" while the playhead is outside said regions; looping is only really happening once we enter the region proper, during playback; "jumping out" of the region is also possible, resetting the looping behaviour like if the playhead was never in the region in the first place.

#### V4.3 (2022.09.17)

	- Fixed monkeypatched love.audio.play/pause/stop not working on ASources. (love.audio.play does not sync up multiple ASources regarding playback though.)

	- Renamed "cosine" mixing method to square root, and added an actual cosine-based one, along with a white-noise based one for no real reason other than it being interesting.

#### V4.4 (2023.02.19)

	- Made TSM frame size be independent from the chosen buffer size, meaning that frame size changes won't change how much input time delay there is.

	- Split setBufferSize and getBufferSize into two; the separate functions added are setFrameSize and getFrameSize.

	- Renamed setBufferVariance and getBufferVariance to setFrameVariance and getFrameVariance.

	- Added setFrameVarianceDistribution and getFrameVarianceDistribution methods to change how the buffer's length gets varied each time one is filled; can be either uniform or normal distribution (with a constant deviation).

	- Changed panning law internals a bit, including bugfixes; now also stored as an enumeration, like everything else. (The custom one is still stored as a per-instance function as well, if defined.)

	- Removed noise-based mixing method, it was not useful.

#### V4.5 (2023.07.08) - CURRENT

	- Fixed cloning not setting Source-specific internal state. (Temporary; permanent fix will come when i figure out why calling source:clone can fail.)

#### V?.? () - TODO

	- Move out loading code from processing thread so it never stalls...

	- Add callback functionality to handle processed buffers (one use-case being visualizers, for example)

	- Have the number of OpenAL-Soft internal buffers not change processing delay. (Note: More testing needed.)

	- Fix error behaviour (show handler screen instead of crashing)

	- Do automatic refcount across threads... possible?

	- Add missing versions to :queue. (rest of the parameters, that is)
	- Add all advanced functionality to `stream` type ASources.
	- Add all advanced functionality to `queue` type ASources. Probably using threading to call worker functions...
		- The queue method also supports all variants that löve supports, except that the lightuserdata variant does
		  not need the format parameters, since it'll use the ASource's given format anyway.

	- Implement a way to have each ASource be instanceable internally (==> multiple voices).

#### Remarks:

	- Tagging support: This library should work out-of-the-box with Tesselode's Ripple library, which implements such features, if needed... unfortunately issues have been reported, so this might not be hassle-free... TODO look into solutions.

### License
This library licensed under the ISC License.
