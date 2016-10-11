import React from 'react';
import {render} from 'react-dom';

var Link = require('react-router').Link

export default class App extends React.Component {
  render () {
  	return (
  		<div>
  		  <Link to="/">No filter set</Link><br />
  		  <Link to="/completed">Completed</Link><br />
  		  <Link to="/closed">Closed</Link><br />
  		  <p> Hello React!</p>
  		  <p> Value of filter route parameter is: {this.props.routeParams.filter}</p>
		</div>
  	)
  }
}
