(in-package :asdf-user)

(defsystem "game-server"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.4"
  :author "malisper"
  :depends-on ("clamp-experimental" "usocket" "cl-ppcre")
  :components ((:file "package")
               (:file "continuations" :depends-on ("package"))
               (:file "handlers" :depends-on ("package"))
               (:file "game" :depends-on ("handlers" "package"))
               (:file "server" :depends-on ("package" "continuations" "game"))
               (:file "ttt" :depends-on ("package" "continuations" "game" "server"))
               (:file "sttt" :depends-on ("package" "continuations" "game" "server"))))
