
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

var Link = require('react-router').Link

class Main extends Component {
  static propTypes = {
    user: PropTypes.object.isRequired
  }

  render () {
    let login = this.props.user ? null : <Link to='/login'>Login</Link>
    return (
      <div>
        <h1>Snap / React / Redux todo app</h1>
        <Link to='/todos'>Todo list</Link><br />
        {login}
      </div>
    )
  }
}

function mapStateToProps (state) {
  return {
    user: state.user
  }
}

export default connect(mapStateToProps)(Main)
