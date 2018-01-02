
import React, { Component } from 'react'
import { Link } from 'react-router-dom'
import Layout from './Layout'

class Main extends Component {
  render () {
    return (
      <div>
        <p>An example application using:</p>
        <ul>
          <li>React</li>
          <li>React-redux</li>
          <li>Redux-auth-wrapper</li>
          <li>Haskell Snap framework with a custom JWT authorization module</li>
        </ul>
        <p>Go to: <Link to='/todos'>Todo List</Link></p>
      </div>
    )
  }
}

export default Layout(Main)
