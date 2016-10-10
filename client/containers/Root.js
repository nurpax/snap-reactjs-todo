import React, { PropTypes } from 'react';
import { Provider } from 'react-redux';
import { Router, Route, browserHistory } from 'react-router';

import App from './App';
import configureStore from '../configureStore'
import { fetchTodos } from '../actions'

const store = configureStore()

const Root = () => (
  <Provider store={store}>
    <Router history={browserHistory}>
      <Route path="/(:filter)" component={App} />
    </Router>
  </Provider>
);

store.dispatch(fetchTodos())

export default Root;
