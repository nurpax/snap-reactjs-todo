
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { fetchTodos } from '../actions'

var Link = require('react-router').Link

class App extends Component {
  static propTypes = {
    loadTodoList: PropTypes.func.isRequired,
    todos: PropTypes.array.isRequired
  }

  componentDidMount () {
    this.props.loadTodoList()
  }

  render () {
    let todos = this.props.todos.map(todo => <li key={todo.id}>{todo.text} <small>{todo.savedOn}</small></li>)
    return (
      <div>
        <h1>Snap / React / Redux todo app</h1>
        <h2>Todos</h2>
        <ul>
          {todos}
        </ul>
      </div>
    )
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    loadTodoList: () => {
      dispatch(fetchTodos())
    }
  }
}

function mapStateToProps (state) {
  return {
    todos: state.todos
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
