
## Handling Touches

* Combinations of gestures, tapping, scrolling, zooming, etc. need to be managed.

### Buttons

* `Button` provides basic button component.

```
<Button
  onPress={() => {
    Alert.alert('You called?');
  }}
  title="Call Me"
/>
```

* Pressing button calls `onPress` assigned function.

* Can specify a color for button.

```
import React, { Component } from 'react';
import { Alert, Button, StyleSheet, View } from 'react-native';

export default class ButtonBasics extends Component {
  _onCallSomeone() {
    Alert.alert('You called.')
  }
  _onSubmit() {
    Alert.alert('Will take care of that.')
  }
  _onCancel() {
    Alert.alert('No problem.')
  }

  render() {
    return (
      <View style={styles.container}>
        <View style={styles.buttonContainer}>
          <Button
            onPress={this._onCallSomeone}
            title="Call Someone"
          />
        </View>
        <View style={styles.alternativeLayoutButtonContainer}>
          <Button
            onPress={this._onSubmit}
            title="Submit"
          />
          <Button
            onPress={this._onCancel}
            title="Cancel"
            color="#841584"
          />
        </View>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
   flex: 1,
   justifyContent: 'center',
  },
  buttonContainer: {
    margin: 20
  },
  alternativeLayoutButtonContainer: {
    margin: 20,
    flexDirection: 'row',
    justifyContent: 'flex-end'
  }
});
```

### Touchables

* If button doesn't fit the look, can use any of Touchables components.

* These don't provide any default styling.

* Different touchable component will depend on what kind of feedback you need.

* `TouchableHighlight` used where button or link would be used. Background is darkened on press.

```
<TouchableHighlight onPress={this._onPressButton} underlayColor="white">
  <View> <Text>TouchableHighlight</Text> </View>
</TouchableHighlight>
```

* `TouchableNativeFeedback` on Android shows link surface reaction ripples on user touch.

```
<TouchableNativeFeedback onPress={this._onPressButton}
    background={Platform.OS === 'android' ? TouchableNativeFeedback.SelectableBackground() : ''}>
  <View> <Text>TouchableNativeFeedback</Text> </View>
</TouchableNativeFeedback>
```

* `TouchableOpacity` used when feedback needs reducing opacity on press.

```
<TouchableOpacity onPress={this._onPressButton}>
  <View> <Text>TouchableOpacity</Text> </View>
</TouchableOpacity>
```

* `TouchableWithoutFeedback` to handle a tap gesture without feedback.

```
<TouchableWithoutFeedback onPress={this._onPressButton}>
  <View> <Text>TouchableWithoutFeedback</Text> </View>
</TouchableWithoutFeedback>
```

* `onLongPress` props could used to manage long press by user.

```
<TouchableHighlight onPress={this._onPressButton} onLongPress={this._onLongPressButton} underlayColor="white">
  <View> <Text>Touchable with Long Press</Text> </View>
</TouchableHighlight>
```

* A working App.js

```
import React, { Component } from 'react';
import { Alert, Platform, StyleSheet, Text, TouchableHighlight, TouchableOpacity, TouchableNativeFeedback, TouchableWithoutFeedback, View } from 'react-native';

export default class Touchables extends Component {
  _onPressButton() {
    Alert.alert('You tapped the button!')
  }

  _onLongPressButton() {
    Alert.alert('You long-pressed the button!')
  }


  render() {
    return (
      <View style={styles.container}>
        <TouchableHighlight onPress={this._onPressButton} underlayColor="white">
          <View style={styles.button}>
            <Text style={styles.buttonText}>TouchableHighlight</Text>
          </View>
        </TouchableHighlight>

        <TouchableOpacity onPress={this._onPressButton}>
          <View style={styles.button}>
            <Text style={styles.buttonText}>TouchableOpacity</Text>
          </View>
        </TouchableOpacity>

        <TouchableNativeFeedback onPress={this._onPressButton}
            background={Platform.OS === 'android' ? TouchableNativeFeedback.SelectableBackground() : ''}>
          <View style={styles.button}>
            <Text style={styles.buttonText}>TouchableNativeFeedback</Text>
          </View>
        </TouchableNativeFeedback>

        <TouchableWithoutFeedback onPress={this._onPressButton}>
          <View style={styles.button}>
            <Text style={styles.buttonText}>TouchableWithoutFeedback</Text>
          </View>
        </TouchableWithoutFeedback>

        <TouchableHighlight onPress={this._onPressButton} onLongPress={this._onLongPressButton} underlayColor="white">
          <View style={styles.button}>
            <Text style={styles.buttonText}>Touchable with Long Press</Text>
          </View>
        </TouchableHighlight>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    paddingTop: 60,
    alignItems: 'center'
  },
  button: {
    marginBottom: 30,
    width: 260,
    alignItems: 'center',
    backgroundColor: '#2196F3'
  },
  buttonText: {
    padding: 20,
    color: 'white'
  }
});
```
---

* Scrolling lists, swiping pages and pinch-to-zoom check on `ScrollView` in next chapter.

---
