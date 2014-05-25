;;;; Scala configurations and services

;; setup vars

;; loading
(require 'ensime)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (ensime-scala-mode-hook)))

