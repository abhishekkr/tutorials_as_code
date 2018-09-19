
## Networking

* To use network hosted resources.

### Using Fetch

* [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) is available for your networking needs.

* Can simply use `fetch` from an arbitrary URL

```
fetch('http://echo.jsontest.com/insert-key-here/insert-value-here/key/value');
```

* Can have customized http fetch with [other list of properties](https://developer.mozilla.org/en-US/docs/Web/API/Request).

```
fetch('http://somesite/key/value', {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content--Type': 'application/json',
    },
    body: JSON.stringify({
      firstKey: 'someVal',
      otherKey: 'otherVal',
    }),
});
```

##### Handling Response

* It's inherently an asynchronous operation. Fetch methods will return a [Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) making it straighforward to write code.

```
function getMyjsonFromApiAsync() {
  return fetch('http://somesite/myjson')
    .then((response) => response.json())
    .then((responseJson) => { return responseJson.myjson })
    .catch((error) => {console.error(error)});
}
```

* Can also use ES2017 `async/wait` syntax in React Native app, as following.

```
async function getMyjsonFromApiAsync() {
  try {
    let response = await fetch('http://somesite/myjson');
    let responseJson = await response.json();
    return responseJson.myjson;
  } catch (error) {
    console.error(error);
  }
}
```

* Can add a loading screen as following, before load.

```
if(this.state.isLoading){
  return(
    <View style={{flex: 1, padding: 20}}>
      <ActivityIndicator/>
    </View>
  )
}
```

* Fetch example pulling json of podcasts, showing title/section/description/with-link. Shown using FlatList.

```
import React from 'react';
import { FlatList, ActivityIndicator, Text, View, Linking  } from 'react-native';

export default class FetchExample extends React.Component {

  constructor(props){
    super(props);
    this.state ={ isLoading: true}
  }

  componentDidMount(){
    return fetch('https://raw.githubusercontent.com/ghosh/awesome-podcasts/master/podcasts.json')
      .then((response) => response.json())
      .then((responseJson) => {
        this.setState({
          isLoading: false,
          dataSource: responseJson.podcasts,
        }, function(){
        });
      })
      .catch((error) =>{
        console.error(error);
      });
  }



  render(){
    if(this.state.isLoading){
      return(
        <View style={{flex: 1, padding: 20}}>
          <ActivityIndicator/>
        </View>
      )
    }

    return(
      <View style={{flex: 1, paddingTop:20}}>
        <FlatList
          data={this.state.dataSource}
          renderItem={
            ({item}) => <View>
              <Text style={{fontWeight: 'bold'}}
                onPress={() => {
                    Linking.openURL(item.source).catch((error) => {console.error(error)});
                  }}>
              {item.name} [{item.category}]</Text>
              <Text>{item.description}{"\n\n"}</Text>
            </View>
          }
          keyExtractor={({id}, index) => id}
        />
      </View>
    );
  }
}
```

* By default, iOS will block any non-ssl request. For http you'll need to [add an App Transport Security exception](https://facebook.github.io/react-native/docs/integration-with-existing-apps#test-your-integration).
> Safer to add exceptions just for required domains.

### Using other networking libraries

* [XMLHttpRequest API](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest) is built in to React Native. You can use third party libraries such as [frisbee](https://github.com/niftylettuce/frisbee) or [axios](https://github.com/mzabriskie/axios).

* No concept of CORS.

```
import React from 'react';
import { FlatList, ActivityIndicator, Text, View, Linking  } from 'react-native';

export default class FetchExample extends React.Component {

  constructor(props){
    super(props);
    this.state ={ isLoading: true}
  }

  componentDidMount(){
    var request = new XMLHttpRequest();
    request.onreadystatechange = (e) => {
      if (request.readyState !== 4) {
        return;
      }
      if (request.readyState === 4 && request.status === 200) {
        console.log('success', request.responseText);
          this.setState({
            isLoading: false,
            dataSource: request.responseText,
          }, function(){
          });
      } else {
        console.warn('error');
      }
    };

    request.open('GET', 'http://joeroganexp.joerogan.libsynpro.com/rss');
    request.send();
  }

  render(){
    if(this.state.isLoading){
      return(
        <View style={{flex: 1, padding: 20}}><ActivityIndicator/></View>
      )
    }
    return(
      <View style={{flex: 1, paddingTop:20}}>
        <Text style={{fontWeight: 'bold'}}> {this.state.dataSource} </Text>
      </View>
    );
  }
}
```

### WebSocket Support

* Supports [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket), providing full-duplex communication channels over single TCP connection.

```
import React from 'react';
import { FlatList, ActivityIndicator, Text, View, Linking  } from 'react-native';

export default class FetchExample extends React.Component {

  constructor(props){
    super(props);
    this.state ={ isLoading: true}
  }

  componentDidMount(){
    var ws = new WebSocket("wss://echo.websocket.org/");

    ws.onopen = () => {
      ws.send('websocket is working'); // send a message
    };

    ws.onmessage = (e) => {
      this.setState({
        isLoading: false,
        dataSource: e.data,
      }, function(){
      });
    };

    ws.onerror = (e) => {
      this.setState({
        isLoading: false,
        dataSource: e.message,
      }, function(){
      });
    };

    ws.onclose = (e) => {
      console.log(e.code, e.reason);
    };
  }

  render(){
    if(this.state.isLoading){
      return(
        <View style={{flex: 1, padding: 20}}><ActivityIndicator/></View>
      )
    }
    return(
      <View style={{flex: 1, paddingTop:20}}>
        <Text style={{fontWeight: 'bold'}}> {this.state.dataSource} </Text>
      </View>
    );
  }
}
```

---
