(defpackage :server
  (:use :clamp :experimental :iter :usocket)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:export :start-game :read-input :defcont :cont :rem-cont
           :start-server :set-cont :socket*
           :game :player :game* :need :players :flags
           :send :send-log :disconnect
           :socket :game-error :invalid-move :signal-game-error :signal-invalid-move))
