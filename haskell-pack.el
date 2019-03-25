;;; haskell-pack.el --- Haskell configuration

;; Copyright (C) 2013-2018  Antoine R. Dumont (@ardumont)
;; Author: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; haskell-pack

(require 'deferred)

;; utilities

(defun haskell-pack/command-installed-p! (package)
  "Determine if PACKAGE is installed on the machine."
  (zerop (shell-command (format "export PATH=~/.local/bin:$PATH; which %s" package))))

(defun haskell-pack/cabal-installed-p! ()
  "Determine if cabal is installed on the machine or not."
  (haskell-pack/command-installed-p! "stack"))

(defun haskell-pack/install-hs-package (hs-package)
  "Install HS-PACKAGE if needs be and if possible."
  (lexical-let ((package hs-package))
    (when (and (haskell-pack/cabal-installed-p!) (not (haskell-pack/command-installed-p! hs-package)))
      (deferred:$
        (deferred:process "stack" "install" package)
        (deferred:nextc it
          (lambda (x)
            (message "Package '%s' is%s installed!" package (if (haskell-pack/command-installed-p! package) "" " still not"))))))))

(defun haskell-pack/install-hs-packages (packages)
  "Trigger cabal install of PACKAGES."
  (mapc 'haskell-pack/install-hs-package packages))

(require 'flymake)
;; Forces flymake to underline bad lines, instead of fully
;; highlighting them;
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'flymake-shell)
(require 'ghci-completion)

(require 'shm)
(define-key shm-map (kbd "C-i") 'shm/tab)
(define-key shm-map (kbd "C-M-i") 'shm/backtab)
(define-key shm-map (kbd "C-m")   'shm/simple-indent-newline-same-col)
(define-key shm-map (kbd "C-M-m") 'shm/simple-indent-newline-indent)
(define-key shm-map (kbd "C-h")   'shm/del)
(define-key shm-map (kbd "C-S-h") 'shm/delete)
(define-key shm-map (kbd "C-M-h") 'shm/backward-kill-word)

(haskell-pack/install-hs-package "structured-haskell-mode")

(require 'flymake-hlint)
(haskell-pack/install-hs-package "hlint")

(require 'haskell-mode)

(define-key haskell-mode-map (kbd "C-c , C-f") 'flymake-popup-current-error-menu)

;; install hooks
(add-hook 'haskell-mode-hook (lambda () (structured-haskell-mode 1)))

;; turn-on-haskell-* are not compatible with structured-haskell-mode (shm)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; trying out the default font color for the moment
;; (set-face-background 'shm-current-face "#373737")
;; (set-face-background 'shm-quarantine-face "#443333")

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
;; compilation on the fly setup
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    "Activate default haskell modes."
	    (subword-mode +1)
	    (turn-on-haskell-doc-mode)
	    (turn-on-ghci-completion)
	    ;; Ignore compiled Haskell files in filename completions
	    (add-to-list 'completion-ignored-extensions ".hi")))

;; Main mode

;; interactive-haskell-mode is the mode! (inferior-haskell is deprecated)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-stylish-on-save t) ;; On save, let stylish format code adequately
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-type 'stack-ghci)
 ;; '(haskell-font-lock-symbols 'unicode)
 '(haskell-font-lock-symbols nil))

;; Install needed cabal packages to be fully compliant with this setup
(haskell-pack/install-hs-packages '("stylish-haskell"
				    "hasktags"))

(provide 'haskell-pack)
;;; haskell-pack.el ends here
