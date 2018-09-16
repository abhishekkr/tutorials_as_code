
## Style

* You style your application using Javascript. All components accept `style` prop.

* Style names and [values](https://facebook.github.io/react-native/docs/colors) are similar to CSS, just camel case.

* Use `StyleSheet.create` to create styles.

```
import React, { Component } from 'react';
import { AppRegistry, StyleSheet, Text, View } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
    let pic = {
      uri: "http://127.0.0.1:8080/some.png"
    }
    return (
      <View>
        <Text style={styles.red}>some red</Text>
        <Text style={styles.someblue}>some blue</Text>
        <Text style={styles.green}>some green</Text>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  someblue: {
    color: 'blue',
    fontWeight: '100',
    fontSize: 50,
  },
  red: {
    color: 'red',
  },
});

// skip this line if using Create React Native App
AppRegistry.registerComponent('SomeProject', () => LotsOfStyles);
```

* Can use to 'cascade' styles as in CSS.

---
