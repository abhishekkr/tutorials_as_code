
## State

* 2 types of data control component, `props` and `state`.

* `props` set by parent and are fixed for component lifetime.

* `state` gets used for mutable data.

* Should initialize `state` in constructor, then call `setState` to change it.

* So for a moving sprite say, the image is a `prop` but its position vectors are kept in `state`.

* Instead of using `setState`, you can also use a state container to control data flow.
> like [Redux](https://redux.js.org/) or [Mobix](https://mobx.js.org/)

```
import React, { Component } from 'react';
import { Text, View } from 'react-native';

type Props = {};

class Clock extends Component<Props> {
  constructor(props) {
    super(props);
    state = {date: new Date()};
  }

  componentDidMount(props){
    setInterval(() => {
        this.setState(previousState => {
          return { date: new Date() };
        });
      }, 1000);
  }

  render() {
    return {
      <View>
        <Text>{this.state.date.toLocaleTimeString()}</Text> // refer to this.props in render
      </View>
    }
  }
}

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
