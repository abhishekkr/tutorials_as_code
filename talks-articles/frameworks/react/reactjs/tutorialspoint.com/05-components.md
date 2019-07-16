
## Components

### Stateless Example

* let's split `App` component into `Header` and `Content` and add inside JSX tree in App component; only `App` component need to be exported

```
import React, { Component } from 'react';

class App extends Component{
   render(){
      return(
         <div>
            <Header/>
            <Content/>
         </div>
      );
   }
}

class Header extends Component {
  render(){
    return (
      <div>
        <h2>React Component</h2>
      </div>
    );
  }
}

class Content extends Component {
  render(){
    return (
      <div>
      <h5>Need some content</h5>
      <p>here is content</p>
      </div>
    );
  };
}

export default App;
```

* using `ReactDOM.render()` from [react-app/main.js](./react-app/main.js) generates result


### Stateful Example

* State maintained at `Content` component gets passed to `User` as `data` and used.

```
class Content extends Component {
  constructor(){
    super();
    this.state = {
      myclass: "main_content",
      users: [ {id: 1, name: "Alice", phone: "+0-000111"},
        {id: 2, name: "Bob", phone: "+0-000222"},
        {id: 3, name: "Charlie", phone: "+0-000133"},
        {id: 4, name: "Delta", phone: "+0-000114"},
      ]
    };
  }

  render(){
    return (
      <div className={this.state.myclass}>
        <table>
          <tbody>
          {this.state.users.map( (user, i) => <User key={i.toString()} data={user}/>)}
          </tbody>
        </table>
      </div>
    );
  };
}

class User extends Component {
  constructor(props) {
    super(props);
  }

  render(){
    return (
        <tr>
          <td>name: {this.props.data.name}</td>
          <td>id: {this.props.data.id}</td>
        </tr>
    );
  };
}
```

> here `key={i}` gets used within `map()`, required by React to only update required part of Virtual DOM

---
