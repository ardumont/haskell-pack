;;; haskell-pack.el --- Haskell configuration

;;; Commentary:

;;; Code:

;; haskell-pack

(use-package deferred)

;; utilities

(defun haskell-pack/cabal-installed-p! ()
  "Determine if cabal is installed on the machine or not."
  (haskell-pack/command-installed-p! "cabal"))

(defun haskell-pack/command-installed-p! (package)
  "Determine if PACKAGE is installed on the machine."
  (zerop (shell-command (format "export PATH=$PATH:~/.cabal/bin; which %s" package))))

(defun haskell-pack/install-hs-package (hs-package)
  "Install HS-PACKAGE if needs be and if possible."
  (lexical-let ((package hs-package))
    (when (and (haskell-pack/cabal-installed-p!) (not (haskell-pack/command-installed-p! package)))
      (deferred:$
        (deferred:process "stack" "install" package)
        (deferred:nextc it
          (lambda (x)
            (message "Package '%s' is%s installed!" package (if (haskell-pack/command-installed-p! package) "" " still not"))))))))

(defun haskell-pack/install-hs-packages (packages)
  "Trigger cabal install of PACKAGES."
  (mapc 'haskell-pack/install-hs-package packages))

(use-package flymake
  :config
  ;; Forces flymake to underline bad lines, instead of fully
  ;; highlighting them;
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow"))))))

(use-package flymake-shell)
(use-package ghci-completion)
(use-package w3m)

(use-package flymake-hlint
  :config (haskell-pack/install-hs-package "hlint"))

(use-package shm
  :config
  (haskell-pack/install-hs-package "structured-haskell-mode")
  (add-hook 'structured-haskell-mode-hook (lambda ()
                                            ;; buffer map
                                            (define-key shm-map (kbd "C-i")   'shm/tab)
                                            (define-key shm-map (kbd "C-M-i") 'shm/backtab)
                                            (define-key shm-map (kbd "C-m")   'shm/simple-indent-newline-same-col)
                                            (define-key shm-map (kbd "C-M-m") 'shm/simple-indent-newline-indent)
                                            (define-key shm-map (kbd "C-h")   'shm/del)
                                            (define-key shm-map (kbd "C-S-h") 'shm/delete)
                                            (define-key shm-map (kbd "C-M-h") 'shm/backward-kill-word))))

(use-package smartscan)

(use-package haskell-mode
  :config
  ;; Install needed cabal packages to be fully compliant with this setup
  (haskell-pack/install-hs-packages '("stylish-haskell"
                                      "hasktags"))
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  ;; On save, let stylish format code adequately
  (custom-set-variables '(haskell-stylish-on-save t))

  ;; turn-on-haskell-* are not compatible with structured-haskell-mode (shm)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  ;; trying out the default font color for the moment
  ;; (set-face-background 'shm-current-face "#eee8d5")
  ;; (set-face-background 'shm-quarantine-face "lemonchiffon")

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  ;; compilation on the fly setup
  (add-hook 'haskell-mode-hook 'flymake-hlint-load)
  (add-hook 'haskell-mode-hook (lambda ()
                                 "Activate default haskell modes."
                                 (subword-mode +1)
                                 (turn-on-haskell-doc-mode)
                                 (turn-on-ghci-completion)
                                 ;; Ignore compiled Haskell files in filename completions
                                 (add-to-list 'completion-ignored-extensions ".hi")
                                 (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
                                 (require 'w3m-haddock)
                                 (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)))
  (add-hook 'haskell-mode-hook 'smartscan-mode)

  ;; Main mode

  ;; interactive-haskell-mode is the mode! (inferior-haskell is deprecated)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-tags-on-save t)
   '(haskell-process-type 'stack-ghci)
   ;; '(haskell-font-lock-symbols 'unicode)
   '(haskell-font-lock-symbols nil))

  ;; (custom-set-variables
  ;;  '(haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "haskell-lab.nix" "--command" )
  ;;                                                       (list (shell-quote-argument (mapconcat 'identity argv " "))))))
  ;;  '(haskell-process-type cabal-repl))

  ;; (custom-set-variables
  ;;  ;; '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
  ;;  ;; not all haskell files needs a setup (.xmonad/xmonad.hs for one), use .dir-locals.el for the moment
  ;;  ;; '(haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
  ;;  ;;                                                      (list (mapconcat 'identity argv " ")))))
  ;;  '(haskell-process-type 'cabal-repl)
  ;;  '(haskell-process-log t))

  ;; .dir-locals.el example
  ;; ((haskell-mode . ((haskell-process-wrapper-function . (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
  ;;                                                          (list (mapconcat 'identity argv " ")))))
  ;;                   (haskell-process-type . cabal-repl))))
  )

(provide 'haskell-pack)
;;; haskell-pack.el ends here
