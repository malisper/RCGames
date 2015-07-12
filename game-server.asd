(in-package :asdf-user)

(defsystem "game-server"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.4"
  :author "malisper"
  :depends-on ("clamp-experimental" "usocket" "cl-ppcre")
  :components ((:file "package")
               (:file "error" :depends-on ("package"))
               (:file "classes" :depends-on ("package"))
               (:file "continuations" :depends-on ("package" "error" "classes"))
               (:file "game" :depends-on ("error" "package" "continuations"))
               (:file "server" :depends-on ("package" "continuations" "game" "classes"))
               (:file "ttt" :depends-on ("package" "continuations" "game" "server"))
               (:file "sttt" :depends-on ("package" "continuations" "game" "server"))))
