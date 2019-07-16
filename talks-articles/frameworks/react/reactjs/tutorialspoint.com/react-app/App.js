import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';

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
            <Content contentHeader={this.props.headerProp} contentSubHeader={this.props.subheaderProps}/>
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
  constructor(props){
    super(props);
    this.state = {
      myclass: "main_content",
      users: [
        {id: 1, name: "Alice", phone: "+0-000111"},
        {id: 2, name: "Bob", phone: "+0-000222"},
        {id: 3, name: "Charlie", phone: "+0-000133"},
        {id: 4, name: "Delta", phone: "+0-000114"},
      ]
    };

    this.addUserElise = this.addUserElise.bind(this); // else 'this' is not available
  };
  addUserElise() {
    this.setState(
      (prevState, props) => {
        let elise = {id: 5, name: "Elise", phone: "n/a"};
        prevState.users.push(elise);
        return {users: prevState.users};
      }
    );
  };

  render(){
    return (
      <div className={this.state.myclass}>
        <h5>{this.props.contentHeader}</h5>
        <p>{this.props.contentSubHeader}</p>
        <hr/>
        <div>
          <table>
            <tbody>
            {
              this.state.users.map(
                (user, i) => <User key={i.toString()} data={user}/>
              )
            }
            </tbody>
          </table>
          <button onClick={this.addUserElise}>add elise</button>
        </div>
        <hr/>
        <SamplePropTypes/>
        <hr/>
        <SampleForceUpdate/>
        <hr/>
      </div>
    );
  };
}

class SampleForceUpdate extends Component {
  constructor(){
    super();
    this.forceNewRandom = this.forceNewRandom.bind(this);
    this.updateColorForXyz = this.updateColorForXyz.bind(this);
  };

  forceNewRandom(){
    this.forceUpdate();
  };

  updateColorForXyz(){
    let xyz = document.getElementById("xyz");
    ReactDOM.findDOMNode(xyz).style.color = 'red';
  };

  render(){
    return (
      <div>
        <hr/>
        <button onClick={this.updateColorForXyz}>make-it-red</button>
        <button onClick={this.forceNewRandom}>randomize</button>
        <span id="xyz">Random: {Math.random()}</span>
      </div>
    );
  };
}

class SamplePropTypes extends Component {
  render(){
    return (
      <div className="props_with_types">
        <span>{this.props.propArray}</span>,
        <span>{this.props.propBool ? "T" : "F"}</span>,
        <span>{this.props.propFunc(100)}</span>,
        <span>{this.props.propNumber}</span>,
        <span>{this.props.propString}</span>,
        <span>{this.props.propObject.id}:{this.props.propObject.name}</span>,
      </div>
    );
  };
}

class User extends Component {
   componentWillMount() {
      console.log('Component WILL MOUNT!')
   }
   componentDidMount() {
      console.log('Component DID MOUNT!')
   }
   componentWillReceiveProps(newProps) {
      console.log('Component WILL RECIEVE PROPS!')
   }
   shouldComponentUpdate(newProps, newState) {
      return true;
   }
   componentWillUpdate(nextProps, nextState) {
      console.log('Component WILL UPDATE!');
   }
   componentDidUpdate(prevProps, prevState) {
      console.log('Component DID UPDATE!')
   }
   componentWillUnmount() {
      console.log('Component WILL UNMOUNT!')
   }
  constructor(props) {
    super(props);
  };

  render(){
    return (
        <tr>
          <td>name: {this.props.data.name}</td>
          <td>id: {this.props.data.id}</td>
        </tr>
    );
  };
}


SamplePropTypes.propTypes = {
   propArray: PropTypes.array.isRequired,
   propBool: PropTypes.bool.isRequired,
   propFunc: PropTypes.func,
   propNumber: PropTypes.number,
   propString: PropTypes.string,
   propObject: PropTypes.object
 };
SamplePropTypes.defaultProps = {
   propArray: [10,12,23,54,75],
   propBool: true,
   propFunc: function(e){return e},
   propNumber: 1,
   propString: "String value...",

   propObject: {
      name: "fff",
      id: "0x",
   }
 };
App.defaultProps = {
   subheaderProps: "here is content"
};
export default App;
