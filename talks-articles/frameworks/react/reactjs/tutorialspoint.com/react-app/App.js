import React, { Component } from 'react';

class App extends Component{
  constructor(){
    super();
    this.state = {
      myclass: "main"
    };
  }

   render(){
      return(
         <div className={this.state.myclass}>
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
  constructor(){
    super();
    this.state = {
      myclass: "main_content",
      users: [
        {id: 1, name: "Alice", phone: "+0-000111"},
        {id: 2, name: "Bob", phone: "+0-000222"},
        {id: 3, name: "Charlie", phone: "+0-000133"},
        {id: 4, name: "Delta", phone: "+0-000114"},
      ]
    };
  }

  render(){
    return (
      <div className={this.state.myclass}>
        <h5>Need some content</h5>
        <p>here is content</p>
        <table>
          <tbody>
          {
            this.state.users.map(
              (user, i) => <User key={i.toString()} data={user}/>
            )
          }
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

export default App;
