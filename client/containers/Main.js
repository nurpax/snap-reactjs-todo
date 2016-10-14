
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import Layout from '../components/Layout'

var Link = require('react-router').Link

class Main extends Component {
  static propTypes = {
    user: PropTypes.object.isRequired
  }

  render () {
    let login = this.props.user ? null : <Link to='/login'>Login</Link>
    return (
      <Layout user={this.props.user}>
        <Link to='/todos'>Todo list</Link>
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
