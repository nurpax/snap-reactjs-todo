
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { Link } from 'react-router-dom'
import Layout from '../components/Layout'

import { getUser } from '../auth'

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
