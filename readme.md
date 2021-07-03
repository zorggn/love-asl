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
- Implements rudimentary Time-stretching and Pitch-shifting methods.
- Implements reverse playback.
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
- `Source:getBufferSize` added.
- `Source:setBufferSize` added, buffer sizes can be between 32 and 65536 samplepoints long; default is 2048.
- `Source:getPitchShift` added with parameter `unit`, in either as a ratio, or in semitones.
- `Source:setPitchShift` added with parameters `amount` and `unit`, in either as a ratio, or in semitones; modifies pitch only.
- `Source:getResamplingRatio` added.
- `Source:setResamplingRatio` added, with parameter `ratio` as a ratio; modifies both playback speed and pitch.
- `Source:getTimeStretch` added.
- `Source:setTimeStretch` added, with parameter `ratio` as a ratio; modifies playback speed only.

- `Source:getPanning` added.
- `Source:setPanning` added, 0.0 is full left, 1.0 is full right, 0.5 is centered; the curve is defined by the specific panning law selected. Default is `0.5`.
- `Source:getPanLaw` added.
- `Source:setPanLaw` added with parameter `law`, which can be one of the following strings: `gain, power` or a custom function taking a number within the range [0,1] as input, and returning two numbers in the domain [0,1] that scale the left and right channels respectively. Default is `gain`.
The two laws are constant-gain/amplitude and constant-power/loudness laws, the first attentuates the volume at the center by -6dB (50%), the second only by -3dB (1/sqrt(2)).

#### Modifications
- `Source:queue` may now be defined as a "pull-style" callback; if it isn't, it will work as the vanilla "push-style" method.
- `Object:release` modified to release all extra internals of the new Objects; must be called explicitly if one doesn't want dead objects cluttering up the processing thread.
- `Object:type` modified to return the string `ASource`.
- `Object:typeOf` modified to also return true for `ASource` as a specialization of the `Source` type.

### Version History

#### V1.0

	Still available in the `non-threaded` branch.

#### V2.0

	Refactored lib to be threaded and thread-safe.

#### V2.1

	Added 2D Panning implementation (direct method, not simulated by OpenAL's spatialization APIs)

#### TODO:

- Implement advanced functionality to `stream`-type Sources as well, that use Decoders internally.
- Implement some kind of windowing to make pitch shifting & time stretching click/pop less.
- Maybe Implement tagging support... just to have people use this instead of TESound. (There's Tesselode's Ripple though...)

### License
This library licensed under the ISC License.
