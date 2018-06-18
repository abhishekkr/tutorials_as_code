
## 

### step.1 get audio in browser

* create a context

```
const audioContext = new AudioContext();
```

* get mic data

```
navigator.getUserMedia({
      audio: true
      }, stream => {
        // a stream of user's audio input device
      },  error => {
        // gotta catch them all
      });
```

deprecated like this in FFox and Chrome, so now
```
navigator.getUserMedia({
      audio: true
      }).then(stream => {
            // a stream of user's audio input device
          },  error => {
            // gotta catch them all
          });
```

*  create an input node from media stream

```
const inputNode = audioContext.createMediaStreamSource(stream);
inputNode.connect(audioContext.destination);
```

---

### step.2 add fx to sound

* volume pedal

```
// Input -> Gainer -> Output

const gainNode = audioContext.createGain();
gainNode.gain.value = .5; // The volume is 50%

inputNode.connect(gainNode);
gainNode.connect(audioContext.destination);
```

* distortion pedal

```
// Input -> Waveshaper -> Output

const waveShaperNode = audioContext.createWaveShaper();
waveShaperNode.oversample = '4x';
waveShaperNode.curve = fancyMathToCalculateCurve(); //example
```

* delay pedal

```
// Input -> Gain -> Gain  ->   Gain -> Output
//             '--> Delay '<-> Gain

const delayNode = audioContext.createDelay();
delayNode.delayTime.value = 1; // 1 second delay
```

* flanger pedal
> a bit like delay but with dynamic time

```
//                  ,---------------Gain -> Output
//  Input      -> Gain  -> Delay--->,'
//                   '---|-.        |
//  Oscillator -> Gain --' '---<--Gain

const oscillatorNode = audioContext.createOscillator();
oscillatorNode.type = 'sine';
oscillatorNode.frequency.value = 1000; // 1000Hz
```

* reverb pedal

```
// Input -> Gain -> Convolver -> Gain -,
//             '--> Gain -------->   Gain -> Output

const convolverNode = audioContext.createConvolver();

fetch('/path/to/impulse-response-file', {
      method: 'get'
}).then(response => {
      return response.arrayBuffer();
}).then(buffer => {
      audioContext.decodeAudioData(buffer, buffer => {
                convolverNode.buffer = buffer;
      });
});
```

* lot of other nodes

> AnalyserNode
> BiquadFilterNode
> ChannelSplitterNode
> ChannelMergerNode
> PannerNode
> DynamicCompressorNode
> more...

---

### started a library to help

[audio-fx](https://github.com/sambego/audio-effects)

---
---
