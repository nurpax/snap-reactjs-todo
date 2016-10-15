
import { createSelector } from 'reselect'

const getTodos = (state) => state.todos

function sorted (arr, f) {
  return arr.concat().sort(f);
}

export const getSortedTodos = createSelector(
  [ getTodos ],
  (todos) => {
    return sorted(todos, function (a, b) {
      if (a.completed == b.completed) {
        return a.savedOn >= b.savedOn
      }
      return a.completed >= b.completed
    })
  }
)
