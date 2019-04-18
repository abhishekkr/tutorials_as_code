"""
### Prepare

* install required python packages

```
pip3 install SpeechRecognition gtts pygame pyaudio pocketsphinx
```

* might see errors for audio development packages missing, then will need to install audio development libraries like below

```
## for Fedora, use pacman, apt-get or whatever pkg-manager you use
sudo dnf install -y pulseaudio-libs-devel
```
"""

import os

import speech_recognition as sr
import pocketsphinx
from gtts import gTTS

#quiet the endless 'insecurerequest' warning
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

from pygame import mixer
mixer.init()


def microphone_index():
    mic_name = "default"
    try:
        mic_name = os.environ["SR_MIC"]
    except:
        print("using %s microphone, can customized providing entire name" % ("SR_MIC"))
    for idx, name in enumerate(sr.Microphone.list_microphone_names()):
        if name == mic_name:
            return idx
    return None


def persist_n_play(tts):
  try:
    tts.save("response.mp3")
    mixer.music.load('response.mp3')
    mixer.music.play()
  except Exception as e:
    print(e)
    print("-----------------------")


def recognize_audio(recognizer, audio):
  try:
    response = recognizer.recognize_sphinx(audio)
    #response = recognizer.recognize_google(audio)
    print("I think you said '" + response + "'")
    tts = gTTS(text="I think you said: "+str(response), lang='en')
    persist_n_play(tts)
  except sr.UnknownValueError:
    print("Sphinx could not understand audio")
  except sr.RequestError as e:
    print("Sphinx error; {0}".format(e))


while (True == True):
  # obtain audio from the microphone
  r = sr.Recognizer()
  with sr.Microphone(device_index=microphone_index()) as source:
    print("Please wait. Calibrating microphone... please wait 5 seconds")
    # listen for 5 second and create the ambient noise energy level
    r.adjust_for_ambient_noise(source, duration=5)
    print("Say something!")
    audio = r.listen(source, phrase_time_limit=5)
    recognize_audio(r, audio)
