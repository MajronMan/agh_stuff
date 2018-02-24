const async = require('async')

const printAsync = (s, cb) => {
   const delay = Math.floor((Math.random()*10)+50)
   setTimeout(() => {
       console.log(s)
       cb && cb()
   }, delay)
   return true
}
 
const task = (p, lim, cb) => ((p <= lim) && printAsync(p, () => task(p+1, lim, cb))) || (cb() === undefined) 
const loop = (n, k) => task(1, 3, 
    () => (n > 0 && loop(n-1)) || console.log("done")
)

const wateryTask = (p, lim) => cb => task(p, lim, cb)
const _createTasks = (n, lim, acc) => (n > 0 && _createTasks(n-1, lim, [...acc, wateryTask(1, lim)])) || acc 
const createTasks = (n, k) => _createTasks(n, k, [])
const wateryLoop = (n, k) => async.waterfall(createTasks(n, k))


wateryLoop(3, 4)