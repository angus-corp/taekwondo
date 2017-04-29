var _angus_corp$taekwondo$Native_NativeModule = (function() {

function close() {
  return _elm_lang$core$Native_Scheduler.nativeBinding(function (cb) {
    window.close();
    cb(_elm_lang$core$Native_Scheduler.fail(_elm_lang$core$Native_Utils.Tuple0));
  });
}

return {
  close: close
};

})();
