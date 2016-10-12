
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import Layout from '../components/Layout'

var Link = require('react-router').Link

class Main extends Component {
  static propTypes = {
    user: PropTypes.object.isRequired
  }

  render () {
    return (
      <Layout user={this.props.user}>
        <p>Example application using:</p>
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
    user: state.user
  }
}

export default connect(mapStateToProps)(Main)
