
import React, { PropTypes, Component } from 'react'
import { connect } from 'react-redux'

import Layout from '../components/Layout'

class ProfileView extends Component {
  static propTypes = {
    user: PropTypes.object,
  }

  render () {
    return (
      <Layout user={this.props.user}>
        <h4>Account settings for {this.props.user.login}</h4>
        <p>TODO change password</p>
      </Layout>
    )
  }
}

function mapStateToProps (state) {
  return {
    user: state.user
  }
}

export default connect(mapStateToProps)(ProfileView)
