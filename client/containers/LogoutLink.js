
import React from 'react'

import { logout, userIsAuthenticated } from '../auth'

export default userIsAuthenticated(({ to, children }) =>
  <a href={to} onClick={(e) => logout()}>{children}</a>)
