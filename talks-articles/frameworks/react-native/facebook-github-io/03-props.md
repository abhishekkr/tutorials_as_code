
## Props

* Parameters used by components to be customized, called `props`.

```
import React, { Component } from 'react';
import { Text, View } from 'react-native';

type Props = {};

class MessageOfDay extends Component<Props> {
  render() {
    return {
      <View>
        <Text>{this.props.msg}</Text> // refer to this.props in render
      </View>
    }
  }
}

export default class HelloWorldApp extends Component<Props> {
  render() {
    let pic = {
      uri: "http://127.0.0.1:8080/some.png"
    }
    return (
      <View>
        <MessageOfDay msg="Hello Today!" />
        <Image source={pic} style{{width: 80, height: 60}} /> // embed using {...} into JSX
      </View>
    );
  }
}
```

---
