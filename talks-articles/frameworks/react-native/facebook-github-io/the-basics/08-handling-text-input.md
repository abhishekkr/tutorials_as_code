
## Handling Text Input

* `TextInput` allowing user to input text
> * `onChangeText` prop takes function for entered text
> * `onSubmitEditing` prop for submitted text

```
import React, { Component } from 'react';
import { Text, TextInput, View } from 'react-native';

export default class HelloWorldApp extends Component {
  constructor(props) {
    super(props);
    this.state = {text: 'text about it'};
  }

  render() {
    return (
      <View style={{padding: 25}}>
        <TextInput
          style={{height: 75}}
          placeholder="Type here to turn words into pizza slice and space to fries"
          onChangeText={(text) => this.setState({text})}
        />
        <Text style={{padding: 25, fontSize: 50}}>
          {this.state.text.split(' ').map((word) => word && '🍕').join('🍟')}
        </Text>
      </View>
    );
  }
}
```

* Text entered be stored as `state`, as it will change.

* Actions possible with [such controlled components](https://reactjs.org/docs/forms.html#controlled-components)

* [Reference docs](https://facebook.github.io/react-native/docs/textinput) for text input.

---
