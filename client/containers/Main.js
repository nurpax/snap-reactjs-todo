
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import Layout from '../components/Layout'
import { Alert } from '../components/helpers'

import { getUser } from '../auth'

var Link = require('react-router').Link

class Main extends Component {
  static propTypes = {
    user: PropTypes.object
  }

  render () {
    return (
      <Layout user={this.props.user}>
        <p>An example application using:</p>
        <ul>
          <li>React</li>
          <li>React-redux</li>
          <li>Redux-auth-wrapper</li>
          <li>Haskell Snap framework with a custom JWT authorization module</li>
        </ul>
        <p>Go to: <Link to='/todos'>Todo List</Link></p>
      </Layout>
    )
  }
}

function mapStateToProps (state) {
  return {
    user: getUser(state)
  }
}

export default connect(mapStateToProps)(Main)
