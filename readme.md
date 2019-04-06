Advanced Source Library
----------------------------------------------------------

### Info

ASL is a threaded wrapper on Löve Source objects; it adds extra functionality to them.

Currently supported Löve versions: 11.x

### Usage

There are two ways to use this library:

- `require 'asl'`, then one can use `love.audio.newAdvancedSource` to create an ASource Object.
- `local new = require 'asl'` which returns the constructor into `new`.

### Additions to Source functionality

- Threaded implementation, can be used from multiple threads as well, it will ever only use one internal processing thread.
- QueueableSource and Buffer-based timing for accurate playback and position tracking.
- Implements custom loop points.
- Implements rudimentary Time-stretching and Pitch-shifting methods.
- Implements reverse playback.

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

#### TODO:

- Implement advanced functionality to `stream`-type Sources as well, that use Decoders internally.
- Implement some kind of windowing to make pitch shifting & time stretching click/pop less.

### License
This library licensed under the ISC License.