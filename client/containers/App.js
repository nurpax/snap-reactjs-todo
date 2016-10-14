
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { fetchTodos } from '../actions'

import Layout from '../components/Layout'

var Link = require('react-router').Link

class NewTodoForm extends Component {
  static propTypes = {
  }

  state = {
    todo: ''
  }

  constructor(props) {
    super(props)
    this.handleChange = this.handleChange.bind(this)
  }

  onClick = (e) => {
    e.preventDefault()
    this.setState({todo: ''})
  };

  handleChange(event) {
    this.setState({todo: event.target.value});
  }

  render () {
    return (
      <form>
        <div className="row">
          <div className="six columns">
            <input className="u-full-width"
                value={this.state.todo}
                onChange={this.handleChange}
                type="text" placeholder="Todo.."
                id="newTodo" />
          </div>
          <div className="six columns">
            <input onClick={this.onClick} className="button-primary" type="submit" value="Add" />
          </div>
        </div>
      </form>
    )
  }
}

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
      <Layout user={this.props.user}>
        <h2>Todos</h2>
        <ul>
          {todos}
        </ul>
        <NewTodoForm />
      </Layout>
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
    user: state.user,
    todos: state.todos
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
