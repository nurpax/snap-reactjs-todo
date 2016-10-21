
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { fetchTodos, saveTodo, setFilter } from '../actions'
import { getSortedTodos } from '../selectors'

import Layout from '../components/Layout'
import TodoItem from './TodoItem'

// TODO this class is kinda kludgy.  Would be better if it hooked into redux
// state directly instead of managing its own internal state.  Buggy too,
// state is not correctly retained across routes because redux state is out of
// sync.
class Choose extends Component {
  static propTypes = {
    choices: PropTypes.arrayOf(PropTypes.string).isRequired,
    onChange: PropTypes.func.isRequired
  }

  static baseButtonStyle = {
    color: '#777',
    width: '44px',
    fontSize: '11px',
    paddingTop: '4px',
    paddingBottom: '4px',
    paddingLeft: '4px',
    paddingRight: '4px',
    borderRadius: '4px',
    display: 'inline-block',
    textAlign: 'center',
    textDecoration: 'none',
    textTransform: 'uppercase'

  }
  static inactiveButtonStyle = {
    border: '0px solid #fff',
    marginLeft: '1px', // adjust left & right for border width
    marginRight: '1px',
    ...Choose.baseButtonStyle
  }

  static activeButtonStyle = {
    border: '1px solid #bbb',
    ...Choose.baseButtonStyle
  }

  state = { selected: 0 }

  onAnchorClick = (e) => {
    e.preventDefault()
    let idx = parseInt(e.target.dataset.id, 10)
    this.setState({ selected: idx })
    this.props.onChange(this.props.choices[idx])
  }

  render () {
    let links = this.props.choices.map(function (name, idx) {
      let link = this.state.selected === idx
        ? <span style={Choose.activeButtonStyle} >{name}</span>
        : <a style={Choose.inactiveButtonStyle} data-id={idx} onClick={this.onAnchorClick} href='#'>{name}</a>
      return <span key={idx}>{link}</span>
    }, this)
    return <span>{links}</span>
  }

}

class NewTodoForm extends Component {
  static propTypes = {
    saveTodo: PropTypes.func.isRequired
  }

  state = {
    todo: ''
  }

  constructor (props) {
    super(props)
    this.handleChange = this.handleChange.bind(this)
  }

  onClick = (e) => {
    e.preventDefault()
    this.props.saveTodo({text: this.state.todo, completed: false})
    this.setState({todo: ''})
  };

  handleChange (event) {
    this.setState({todo: event.target.value})
  }

  render () {
    return (
      <form>
        <div className='row'>
          <div className='six columns'>
            <input className='u-full-width'
              value={this.state.todo}
              onChange={this.handleChange}
              type='text' placeholder='Todo..'
              id='newTodo' />
          </div>
          <div className='six columns'>
            <input onClick={this.onClick} className='button-primary' type='submit' value='Add' />
          </div>
        </div>
      </form>
    )
  }
}

class App extends Component {
  static propTypes = {
    loadTodoList: PropTypes.func.isRequired,
    saveTodo: PropTypes.func.isRequired,
    setFilter: PropTypes.func.isRequired,
    todos: PropTypes.array.isRequired,
    user: PropTypes.object
  }

  constructor (props) {
    super(props)
    this.handleFilterChange = this.handleFilterChange.bind(this)
  }

  handleFilterChange (selected) {
    if (selected === 'All') {
      this.props.setFilter('all')
    } else {
      this.props.setFilter('active')
    }
  }

  componentDidMount () {
    this.props.loadTodoList()
  }

  render () {
    let filterChoices = ['Active', 'All']
    let saveTodo = this.props.saveTodo
    let todos = this.props.todos.map(todo => <TodoItem key={todo.id} todo={todo} saveTodo={saveTodo} />)
    return (
      <Layout user={this.props.user}>
        <h2>Todos</h2>
        <p><strong>Show: </strong><Choose onChange={this.handleFilterChange} choices={filterChoices} /></p><br />
        <ul className='todos'>
          {todos}
        </ul>
        <NewTodoForm saveTodo={saveTodo} />
      </Layout>
    )
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    loadTodoList: () => {
      dispatch(fetchTodos())
    },
    saveTodo: (todo) => {
      dispatch(saveTodo(todo))
    },
    setFilter: (filter) => {
      dispatch(setFilter(filter))
    }
  }
}

function mapStateToProps (state) {
  return {
    user: state.user,
    todos: getSortedTodos(state)
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
