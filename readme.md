Advanced Source Library
----------------------------------------------------------

### Info

ASL is a threaded wrapper on Löve (Audio) Source objects; it adds extra functionality to them.

Currently supported Löve version(s): 11.3

### Usage

There are two ways to use this library:

- `require 'asl'`, then one can use `love.audio.newAdvancedSource` to create an ASource Object.
- `local new = require 'asl'` which returns the constructor into `new`.

### Considerations

- Since this library does monkeypatch a few things, please don't bug the löve2D developers with issues if you're using this library, before doing some due diligence making sure the error doesn't stem from this library (in which case, do create an issue here for me to solve!)

### Additions to Source functionality

- Threaded implementation, can be used from multiple threads as well, it will ever only use one internal processing thread.
- QueueableSource and Buffer-based timing for accurate playback and position tracking.
- Implements custom loop points.
- Implements panning and stereo separation support.
- Implements time-domain time-stretching and pitch-shifting methods.
- Implements reverse playback.
- Implements different interpolation methods during modified playback (resampled, stretched, and/or shifted)
- All methods that don't return any values are chainable.

### API Changes

#### Deprecations
- `Source:getPitch` is now `Source:getResamplingRatio`.
- `Source:setPitch` is now `Source:setResamplingRatio`.

#### Additions
- `Source:rewind` re-added, which is just syntax sugar for `Source:seek(0)`.

- `Source:getLoopPoints` added, returns `startpoint` and `endpoint`, in samplepoints.
- `Source:setLoopPoints` added with parameters `startpoint` and `endpoint`, in samplepoints.

- `Source:getBitDepth` and `Source:getSampleRate` added.

- `Source:getBufferSize` added with parameter `unit`, in either samplepoints or milliseconds, the latter being default.
- `Source:setBufferSize` addedwith parameter `unit`, in either samplepoints or milliseconds, the latter being default; buffer sizes can be between 32 and 65536 samplepoints long; default is ~50 ms equivalent rounded down.
- `Source:getPitchShift` added with parameter `unit`, in either as a non-negative ratio, or in semitones.
- `Source:setPitchShift` added with parameters `amount` and `unit`, in either as a non-negative ratio, or in semitones; modifies pitch only.
- `Source:getResamplingRatio` added.
- `Source:setResamplingRatio` added, with parameter `ratio` as a ratio; modifies both playback speed and pitch.
- `Source:getTimeStretch` added.
- `Source:setTimeStretch` added, with parameter `ratio` as a ratio; modifies playback speed only.
- `Source:getInterpolationMethod` added.
- `Source:setInterpolationMethod` added, with parameter `method`, which can be one of the following strings: `nearest, linear, cubic, sinc`.

- `Source:getPanning` added.
- `Source:setPanning` added, 0.0 is full left, 1.0 is full right, 0.5 is centered; the curve is defined by the specific panning law selected. Default is `0.5`.
- `Source:getPanLaw` added.
- `Source:setPanLaw` added with parameter `law`, which can be one of the following strings: `gain, power` or a custom function taking a number within the range [0,1] as input, and returning two numbers in the domain [0,1] that scale the left and right channels respectively. Default is `gain`.
The two laws are constant-gain/amplitude and constant-power/loudness laws, the first attentuates the volume at the center by -6dB (50%), the second only by -3dB (1/sqrt(2)).
- `Source:getStereoSeparation` added.
- `Source:setStereoSeparation` added, 0.0 means both channels are mixed down to monaural (mono), 1.0 means the original stereo signal is kept. Default is `1.0`.

#### Modifications
- `Source:queue` may now be defined as a "pull-style" callback; if it isn't, it will work as the vanilla "push-style" method.
- `Object:release` modified to release all extra internals of the new Objects; must be called explicitly if one doesn't want dead objects cluttering up the processing thread.
- `Object:type` modified to return the string `ASource`.
- `Object:typeOf` modified to also return true for `ASource` as a specialization of the `Source` type.

### Version History

#### V1.0

	- Still available in the `non-threaded` branch.

#### V2.0

	- Refactored lib to be threaded and thread-safe.

#### V2.1

	- Added Stereo Panning implementation (direct method, not simulated by OpenAL's spatialization APIs.)

#### V2.2

	- Added Stereo Separation implementation.

#### V2.21

	- Some fixes regarding range checking and default values, one missing function added (getActiveEffects).

#### V3.0

	- Complete reimplementation of time-scale modifications to achieve pop-less functionality; it still uses time-domain methods (time-scale modification and resampling) so depending on the settings, it might still sound weird.
	- Added interpolation methods for higher quality resampling, including cubic hermite spline and 32-tap lanczos sinc implementations.
	- Changed the behaviour of buffers; they are not recreated on size change anymore; the maximum sized ones will be created, and those can be limited to a smaller range instead.
	- Added milliseconds unit to the setter and getter of the buffer size.
	- Changed default buffer size to be equivalent to 50 ms.

#### TODO:

- Implement advanced functionality to `stream`-type Sources as well, that use Decoders internally.
- Maybe Implement tagging support... just to have people use this instead of TESound. (There's Tesselode's Ripple though...)

### License
This library licensed under the ISC License.
