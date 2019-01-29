/*
 * step-wise intro WebAudio API using comments
 *
 * detailed mozilla doc: https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Using_Web_Audio_API
 *
 * step-01,02 from source: https://www.youtube.com/watch?v=oaemcUfcYcg
 *
 * step-03,04 from source: https://www.youtube.com/watch?v=zmSM2JCqjaM
 *
 * source: https://www.youtube.com/watch?v=mdBS2nD3G1A | just binds html input for interactive oscillator changes
 */

function step01(){
  var audioContext = new AudioContext(); // primary object to avail

  // input is connected to a 'node',
  // which can be connected to another node and so on

  // input used here will be 'oscillator'
  var oscillator = audioContext.createOscillator();
  // connect it to your AudioContext destination ~> output/eg:speaker
  oscillator.connect(audioContext.destination);

  // start your oscillator
  oscillator.start();

  setTimeout(function oscillatorStop() {
    oscillator.stop();}, 125); //int is for microsecond
};

function step02(){
  var audioContext = new AudioContext();
  var oscillator = audioContext.createOscillator();
  // can connect oscillator to AudioContext via intermediary filter
  // 2 primary filter IIR Filter and Biquad Filter
  var filter = audioContext.createBiquadFilter();

  oscillator.connect(filter);
  filter.connect(audioContext.destination);

  filter.type = "highpass"; // allowing only higher frequency
  // for more Biquad filter types: https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode
  // below: 1st param is frequency rate of filter;
  //   2nd param when to start;
  //   3rd param is exponential n constant for how slow effect shall happen
  filter.frequency.setTargetAtTime(25, audioContext.currentTime, 1);

  oscillator.start(audioContext.currentTime);
  setTimeout(function oscillatorStop() {
    oscillator.stop();}, 175);
};

function step03(){
  var audioContext = new AudioContext();
  var oscillator = audioContext.createOscillator();

  oscillator.type = "sawtooth"; // default: sine
  // shape of waves
  // other options: https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode/type

  oscillator.audioprocess = function(){
    console.log("step03 audio processed");
  };
  oscillator.onended = function(){
    console.log("step03 audio finished");
  };

  oscillator.connect(audioContext.destination);

  oscillator.start(audioContext.currentTime);
  oscillator.stop(audioContext.currentTime + 1); // stop after 1sec

  // once stopped, need to create another oscillator
};

function step04(){
  var audioContext = new AudioContext();
  var oscillator = audioContext.createOscillator();

  oscillator.type = "triangle";
  oscillator.connect(audioContext.destination);

  // 130.4 is frequency of middle C note; set at start
  oscillator.frequency.setValueAtTime(130.4, audioContext.currentTime);

  // detune can be used to change notes; each note is 200cents more than previous, 100cent for half-step
  oscillator.detune.setValueAtTime(100, audioContext.currentTime + 0.5); // to C#
  oscillator.detune.setValueAtTime(200, audioContext.currentTime + 1.0); // to D
  oscillator.detune.setValueAtTime(300, audioContext.currentTime + 1.5); // to D#
  oscillator.detune.setValueAtTime(400, audioContext.currentTime + 2); // to E
  oscillator.detune.setValueAtTime(600, audioContext.currentTime + 2.5); // to F
  oscillator.detune.setValueAtTime(700, audioContext.currentTime + 3); // to F#
  oscillator.detune.setValueAtTime(800, audioContext.currentTime + 3.5); // to G
  oscillator.detune.setValueAtTime(900, audioContext.currentTime + 4); // to G#
  oscillator.detune.setValueAtTime(1000, audioContext.currentTime + 4.5); // to A
  oscillator.detune.setValueAtTime(1100, audioContext.currentTime + 5); // to A#
  oscillator.detune.setValueAtTime(1200, audioContext.currentTime + 5.5); // to B
  oscillator.detune.setValueAtTime(1400, audioContext.currentTime + 6); // to C

  oscillator.start(audioContext.currentTime);
  oscillator.stop(audioContext.currentTime + 7);

  // to re-init oscillator post usage
  /*
  oscillator.onended = function(){
    oscillator = audioContext.createOscillator();
    oscillator.connect(audioContext.destination);
    // it will lose previous stae
  };
  */
};
