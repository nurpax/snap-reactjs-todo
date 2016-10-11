
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

var Link = require('react-router').Link

class App extends Component {
  render () {
    let todos = this.props.todos.map(todo => <li key={todo.id}>{todo.text} <small>{todo.savedOn}</small></li>)
    return (
      <div>
        <h1>Snap / React / Redux todo app</h1>
        <Link to='/'>Filter 1</Link><br />
        <Link to='/nofilt'>Filter 2</Link><br />
        <Link to='/nofilt2'>Filter 3</Link><br />
        <Link to='/test'>Test</Link><br />
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

function mapStateToProps (state) {
  return {
    todos: state.todos
  }
}

export default connect(mapStateToProps)(App)
