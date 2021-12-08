;;;; lgo.asd

(asdf:defsystem #:lgo
  :description "Go board game in Common Lisp"
  :author "Dan Beauchesne"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :components ((:file "package")
               (:file "math" :depends-on ("package"))
               (:file "game-logic" :depends-on ("math"))
               (:file "lgo" :depends-on ("game-logic"))))

 (asdf:defsystem #:lgo/tests
   :depends-on (:lgo :fiveam)
   :components ((:module "tests"
                 :serial t
                 :components ((:file "package")
                              (:file "main")))))
