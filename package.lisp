(defpackage :server
  (:use :clamp :experimental :iter :usocket)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:export :start-game :read-input :defcont :cont :rem-cont
           :temp-cont :rem-temp-cont :start-server
           :game :player :ai :human :need :players
           :send :send-hu :send-ai :send-log :disconnect
           :socket :game-error :invalid-move))
