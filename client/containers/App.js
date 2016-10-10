
import React, { Component, PropTypes } from 'react'
import { render } from 'react-dom';
import { connect } from 'react-redux'

var Link = require('react-router').Link

class App extends Component {
  constructor(props) {
    super(props)
  }

  render () {
    let todos = this.props.todos.map(todo => <li key={todo.id}>{todo.text} <small>{todo.savedOn}</small></li>)
  	return (
        <div>
          <h1>Snap / React / Redux todo app</h1>
          <Link to="/">No filter set</Link><br/>
          <h2>Todos</h2>
          <ul>
            {todos}
          </ul>
        </div>
  	)
  }
}

App.propTypes = {
  todos: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired
}

function mapStateToProps(state) {
  return {
    todos: state.todos
  }
}

export default connect(mapStateToProps)(App)
