
## Basics

* React Native is like React, just using native components instead of web components.

* It ships with [ES2015 features](https://babeljs.io/docs/en/learn/).

* A basic hello-world section `App.js`, using View and Text components as JSX.

```
import React, { Component } from 'react';
import { Text, View } from 'react-native';

type Props = {};

export default class HelloWorldApp extends Component<Props> {
  render() {
    return (
      <View>
        <Text>Hello world!</Text>
      </View>
    );
  }
}
```

* JSX is need to be returned by `render()` for view.

---
