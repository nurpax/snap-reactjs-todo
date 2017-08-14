
import React from 'react'
import { Link } from 'react-router-dom'
import { connect } from 'react-redux'

import { logout } from '../auth'

const Logout = function ({ logout, to, children }) {
  return <Link to={to} onClick={(e) => logout()}>{children}</Link>
}

export default connect(false, { logout })(Logout)
