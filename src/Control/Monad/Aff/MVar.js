'use strict'

var QUEUE_BLOCK_SIZE = 1000

function Queue() {
  if (!(this instanceof Queue)) return new Queue()
  var b = createBlock()
  this._blocks = 1
  this._first  = b
  this._fp     = 0
  this._last   = b
  this._lp     = 0
}

function createBlock(o) {
  var b = Object.create(null)
  if (o) {
    b[0] = o
  }
  b.n = null
  return b
}

function _sizeQueue(queue) {
  return QUEUE_BLOCK_SIZE * (queue._blocks - 1) + queue._lp - queue._fp
}

function _isEmpty(queue) {
  return queue._blocks === 1 && queue._lp >= queue._fp
}

Object.defineProperty(Queue.prototype, 'size', {
  get: function () {
    return _sizeQueue(this)
  }
})

Object.defineProperty(Queue.prototype, 'isEmpty', {
  get: function () {
    return _isEmpty(this)
  }
})

Queue.prototype.enqueue = function (o) {
  if (this._lp === QUEUE_BLOCK_SIZE) {
    var newBlock = createBlock(o)
    this._blocks++
    this._last.n = newBlock
    this._last = newBlock
    this._lp = 1
  } else {
    this._last[this._lp++] = o
  }
}

Queue.prototype.dequeue = function () {
  if (this._blocks === 1 && this._fp >= this._lp) return null
  var qfb = this._first, r = qfb[this._fp]
  qfb[this._fp] = null
  if (++this._fp === QUEUE_BLOCK_SIZE) {
    if (this._blocks === 1) {
      this._lp = 0;
    } else {
      this._blocks--;
      this._first = this._first.n;
    }
    this._fp = 0
  } else if (this._blocks === 1 && this._fp >= this._lp) {
    this._lp = this._fp = 0
  }
  return r
}

function _queueIterator(queue) {
  var b = queue._first, bp = queue._fp, lb = queue._last, lp = queue._lp
  return {
    next: function () {
      if (b === null || (b === lb && bp >= lp)) {
        return { done: true }
      } else {
        var r = b[bp]
        if (++bp === QUEUE_BLOCK_SIZE) {
          b = b.n
          bp = 0
          if (b === null) lb = null
        }
        return { done: false, value: r }
      }
    }
  }
}

Queue.prototype.iter = function () {
  return _queueIterator(this)
}

var MVAR_EMPTY  = 1 << 0
var MVAR_FULL   = 1 << 1
var MVAR_KILLED = 1 << 2

function MVar() {
  this.readers = new Queue()
  this.writers = new Queue()
  this.waiters = 0
  this.val     = undefined
  this._state  = 0
  _setMVarEmpty(this)
}

function _isMVarFull (mv) {
  return (mv._state & MVAR_FULL) !== 0
}

function _setMVarValue (mv, value) {
  mv._state = mv._state | MVAR_FULL
  mv.val = value
}

function _unsetMVarFull (mv) {
  mv._state = mv._state & (~MVAR_FULL)
  mv.val = undefined
}

function _isMVarEmpty (mv) {
  return (mv._state & MVAR_EMPTY) !== 0
}

function _setMVarEmpty(mv) {
  mv._state = mv._state | MVAR_EMPTY
  mv.val = undefined
}

function _unsetMVarEmpty(mv) {
  mv._state = mv._state & (~MVAR_EMPTY)
}

function _isMVarKilled(mv) {
  return (mv._state & MVAR_KILLED) !== 0
}

function _setMVarKilled(mv, err) {
  mv._state = mv._state | MVAR_KILLED
  mv.val = err
}

exports._makeMVar = function (nonCanceler) {
  return function (success) {
    success(new MVar())
    return nonCanceler
  }
}

function createAffRec(success, error) {
  var w = Object.create(null)
  w.success = success
  w.error = error
  return w
}

function createWriter(success, error, value) {
  var w = Object.create(null)
  w.success = success
  w.error = error
  w.value = value
  return w
}

function _queueWaiter(waiter, mv) {
  var i = this.waiters
  mv.waiters++
  mv[i] = waiter
}

function _wakeUpWaiters(mv, val) {
  if (mv.waiters > 0) {
    var l = mv.waiters
    for (var i = 0; i < l; i++) {
      mv[i].success(val)
      mv[i] = undefined
    }
    mv.waiters = 0
  }
}

function notifyMVarFull(mv, val) {
  _wakeUpWaiters(mv, val)
  // wake up one reader
  var reader = mv.readers.dequeue()
  if (reader) {
    if (_isMVarFull(mv)) {
      _unsetMVarFull(mv)
    }
    _setMVarEmpty(mv)
    reader.success(val)
  } else {
    // no reader
    if (_isMVarEmpty(mv)) {
      _unsetMVarEmpty(mv)
    }
    _setMVarValue(mv, val)
  }
}

function notifyMVarEmpty(mv) {
  var writer = mv.writers.dequeue()
  if (writer) {
    if (_isMVarEmpty(mv)) {
      _unsetMVarEmpty(mv)
    }
    _setMVarValue(mv, writer.value)
    writer.success()
  } else {
    if (_isMVarFull(mv)) {
      _unsetMVarFull(mv)
    }
    _setMVarEmpty(mv)
  }
}

exports._takeMVar = function (nonCanceler, mv) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.val)
    } else if (_isMVarFull(mv)) {
      success(mv.val)
      notifyMVarEmpty(mv)
    } else {
      mv.readers.enqueue(createAffRec(success, error))
    }
    return nonCanceler
  }
}

exports._tryTakeMVar = function (nonCanceler, nothing, just, mv) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.val)
    } else if (_isMVarFull(mv)) {
      success(just(mv.val))
      notifyMVarEmpty(mv)
    } else {
      success(nothing)
    }
    return nonCanceler
  }
}

exports._readMVar = function (nonCanceler, mv) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.val)
    } else if (_isMVarFull(mv)) {
      success(mv.val)
    } else {
      _queueWaiter(createAffRec(success, error), mv)
    }
    return nonCanceler
  }
}

exports._putMVar = function (nonCanceler, mv, val) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.val)
    } else if (_isMVarFull(mv)) {
      mv.writers.enqueue(createWriter(success, error, val))
    } else {
      notifyMVarFull(mv, val)
      success()
    }
    return nonCanceler
  }
}

exports._tryPutMVar = function (nonCanceler, mv, val) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.val)
    } else if (_isMVarFull(mv)) {
      success(false)
    } else {
      notifyMVarFull(mv, val)
      success(true)
    }
    return nonCanceler;
  }
}

exports._killMVar = function (nonCanceler, mv, err) {
  return function (success, error) {
    if (_isMVarKilled(mv)) {
      error(mv.error)
    } else {
      var readers = mv.readers, writers = mv.writers, waiters = mv.waiters
      if (waiters > 0) {
        for (var i = 0, wl = waiters; i < wl; i++) {
          mv[i].error(err)
          mv[i] = undefined
        }
        mv.waiters = 0
      }
      // readers
      if (readers) {
        for (var _iterator = readers.iter(), _step, item; !(_step = _iterator.next()).done; ) {
          _step.value.error(err)
        }
        mv.reader = undefined
      }
      // writers
      if (writers) {
        for (var _iterator = writers.iter(), _step, item; !(_step = _iterator.next()).done; ) {
          _step.value.error(err)
        }
        mv.writer = undefined
      }
      _setMVarKilled(mv, err)
      success()
    }
  }
}
