;;; haskell-pack.el --- Haskell configuration

;;; Commentary:

;;; Code:

;; haskell-pack

(require 'install-packages-pack)
(install-packages-pack/install-packs '(flymake
                                       flymake-shell
                                       haskell-mode
                                       ghci-completion
                                       flymake-hlint
                                       smartscan
                                       w3m
                                       shm
                                       deferred))

(require 'flymake)
(require 'haskell-mode)
(require 'deferred)

;; utilities

(defun haskell-pack/cabal-installed-p! ()
  "Determine if cabal is installed on the machine or not."
  (haskell-pack/command-installed-p! "cabal"))

(defun haskell-pack/command-installed-p! (package)
  "Determine if PACKAGE is installed on the machine."
  (zerop (shell-command (format "which %s" package))))

(defun haskell-pack/cabal-install-package (cabal-package)
  "Install CABAL-PACKAGE (if not already installed) through cabal (if cabal is installed)."
  (lexical-let ((package cabal-package))
    (when (and (haskell-pack/cabal-installed-p!) (not (haskell-pack/command-installed-p! package)))
      (deferred:$
        (deferred:process "cabal" "install" package)
        (deferred:nextc it
          (lambda (x)
            (message "Package '%s' is%s installed!" package (if (haskell-pack/command-installed-p! package) "" " still not"))))))))

(defun haskell-pack/cabal-install-packages (packages)
  "Trigger cabal install of PACKAGES."
  (mapc 'haskell-pack/cabal-install-package packages))

;; structured-haskell-mode setup
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; Install needed cabal packages to be fully compliant with this setup
(haskell-pack/cabal-install-packages '("structured-haskell-mode"
                                       "stylish-haskell"
                                       "hasktags"
                                       "hlint"))

;; On save, let stylish format code adequately
(custom-set-variables '(haskell-stylish-on-save t))

;; turn-on-haskell-* are not compatible with structured-haskell-mode (shm)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; trying out the default font color for the moment
;; (set-face-background 'shm-current-face "#eee8d5")
;; (set-face-background 'shm-quarantine-face "lemonchiffon")

(defun haskell-pack/structured-haskell-bindings ()
  "Add some more bindings for the structured-haskell-mode-map."
  (interactive)
  ;; buffer map
  (define-key shm-map (kbd "C-i")   'shm/tab)
  (define-key shm-map (kbd "C-M-i") 'shm/backtab)
  (define-key shm-map (kbd "C-m")   'shm/simple-indent-newline-same-col)
  (define-key shm-map (kbd "C-M-m") 'shm/simple-indent-newline-indent)
  (define-key shm-map (kbd "C-h")   'shm/del)
  (define-key shm-map (kbd "C-S-h") 'shm/delete)
  (define-key shm-map (kbd "C-M-h") 'shm/backward-kill-word))

(add-hook 'structured-haskell-mode-hook 'haskell-pack/structured-haskell-bindings)

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; compilation on the fly setup

(require 'flymake-hlint)
(add-hook 'haskell-mode-hook 'flymake-hlint-load)

;;;;;;;;;;;;;;;;;;;;; haskell

(defun haskell-pack/mode-defaults ()
  "Activate default haskell modes."
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-ghci-completion)
  ;; Ignore compiled Haskell files in filename completions
  (add-to-list 'completion-ignored-extensions ".hi")
  (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
  (require 'w3m-haddock)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock))

(add-hook 'haskell-mode-hook 'haskell-pack/mode-defaults)

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them;
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'smartscan)
(add-hook 'haskell-mode-hook (lambda () (smartscan-mode)))

;; Main mode

;; interactive-haskell-mode is the mode! (inferior-haskell is deprecated)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-type 'cabal-repl))

;; (custom-set-variables
;;  '(haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "haskell-lab.nix" "--command" )
;;                                                       (list (shell-quote-argument (mapconcat 'identity argv " "))))))
;;  '(haskell-process-type cabal-repl))

(custom-set-variables
 '(haskell-font-lock-symbols 'unicode)
 ;; '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
 '(haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
                                                      (list (mapconcat 'identity argv " ")))))
 '(haskell-process-type 'cabal-repl)
 '(haskell-process-log t))

(provide 'haskell-pack)
;;; haskell-pack.el ends here
