
## Height and Width

* Component's height and width determined for size on screen.

* All dimensions are unitless, representing density-independent pixels.

* Fixed dimensions of a Component by adding a fixed `width` and `height` to style.

```
import React, { Component } from 'react';
import { Text, View } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
    return (
      <View style={{flex:1}}>
        <View style={{width: 50, height: 50, backgroundColor: 'powderblue'}}>
          <Text>Hello world!</Text>
        </View>
        <View style={{width: 150, height: 150, backgroundColor: 'skyblue'}}>
          <Text>Hello planet!</Text>
        </View>
        <View style={{width: 100, height: 100, backgroundColor: 'steelblue'}}>
          <Text>Hello universe!</Text>
        </View>
      </View>
    );
  }
}
```

* Using `flex` in a component's style to have dynamic dimensions based on space.

* `flex:1`  tells a component to fill available space, shared evenly amongst each other component with same parent.

* Larger the `flex` given, higher ratio component takes compared to siblings.

```
import React, { Component } from 'react';
import { Text, View } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
    return (
      <View style={{flex:1}}>
        <View style={{flex: 1, backgroundColor: 'powderblue'}}>
          <Text>Hello world!</Text>
        </View>
        <View style={{flex: 2, backgroundColor: 'skyblue'}}>
          <Text>Hello planet!</Text>
        </View>
        <View style={{flex: 3, backgroundColor: 'steelblue'}}>
          <Text>Hello universe!</Text>
        </View>
        <View style={{flex: 0.5, backgroundColor: 'steelblue'}}>
          <Text>Hello universe!</Text>
        </View>
      </View>
    );
  }
}
```

---
