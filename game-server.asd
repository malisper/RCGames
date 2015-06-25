(in-package :asdf-user)

(defsystem "game-server"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.3"
  :author "malisper"
  :depends-on ("clamp-experimental" "usocket" "cl-ppcre")
  :components ((:file "package")
               (:file "server" :depends-on ("package"))))
