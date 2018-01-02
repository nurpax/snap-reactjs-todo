import React, { Component } from 'react'
import { connect } from 'react-redux'
import { getUser } from '../auth'
import Layout from '../components/Layout'

// Use this to wrap any top-level components that need to be rendered inside
// the site 'Layout'.  This plugs in authenticated user information
// automatically.
export default function LayoutWrap (MainComponent) {
  class Wrapper extends Component {
    render () {
      return (
        <Layout user={this.props.user}>
          <MainComponent {...this.props} />
        </Layout>
      )
    }
  }

  function mapStateToProps (state) {
    return {
      user: getUser(state)
    }
  }

  return connect(mapStateToProps)(Wrapper)
}
