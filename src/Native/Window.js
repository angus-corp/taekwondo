var cloose = function (Task) {
  return function () {
    return Task.asyncFunction(function (cb) {
      window.close();
      return cb(Task.fail(null));
    });
  };
};

var make = function make(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Window = elm.Native.Window || {};

  if (elm.Native.Window.values) return elm.Native.Window.values;

  var Task = Elm.Native.Task.make(elm);

  return elm.Native.Window.values = {
    'close': cloose(Task)
  };
};

Elm.Native.Window = {};
Elm.Native.Window.make = make;
