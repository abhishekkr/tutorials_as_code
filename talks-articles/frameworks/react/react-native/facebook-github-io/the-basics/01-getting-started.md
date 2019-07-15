
## Getting Started

* install dependencies
> * nodejs 8.3 or greater
> * a JDK 8 or newer
> * android sdk, android studio

```
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/emulator
```

* install react-native cli

```
npm install -g react-native-cli
```

* create new app

```
react-native init someApp
```

* try vanilla app

```
cd someApp
react-native start

react-native run-android
```

---
