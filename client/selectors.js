
import { createSelector } from 'reselect'

const getTodos = (state) => state.todos
const getFilter = (state) => state.filter

function sorted (arr, f) {
  return arr.concat().sort(f)
}

export const getSortedTodos = createSelector(
  [ getTodos, getFilter ],
  (todos, filter) => {
    return sorted(todos, function (a, b) {
      if (a.completed === b.completed) {
        return a.savedOn >= b.savedOn
      }
      return a.completed >= b.completed
    }).filter(todo => {
      if (filter === 'active') {
        return !todo.completed
      }
      return true
    })
  }
)
