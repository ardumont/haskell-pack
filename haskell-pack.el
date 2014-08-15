;;; haskell-pack.el --- Haskell configuration

;;; Commentary:

;;; Code:

;; haskell-pack

(require 'install-packages-pack)
(install-packs '(flymake
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
(require 'inf-haskell)
(require 'deferred)

;; utilities

(defun haskell-pack/cabal-installed-p! ()
  "Determine if cabal is installed on the machine or not."
  (haskell-pack/command-installed-p! "cabal"))

(defun haskell-pack/command-installed-p! (package)
  "Determine if PACKAGE is installed on the machine."
  (zerop (shell-command (format "which %s" package))))

(defmacro haskell-pack/cabal-install (package)
  "Install PACKAGE (if not already installed) through cabal (if cabal is installed)."
  `(when (and ,(haskell-pack/cabal-installed-p!) (not (haskell-pack/command-installed-p! ,package)))
     (deferred:$
       (deferred:process "cabal" "install" ,package)
       (deferred:nextc it
         (lambda (x)
           (message "Package '%s'%s installed!" ,package (if (haskell-pack/command-installed-p! ,package) "" " still not")))))))

;; structured-haskell-mode setup

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; Install needed structured-haskell-mode if not already installed
(haskell-pack/cabal-install "structured-haskell-mode")
(haskell-pack/cabal-install "stylish-haskell")

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
(add-hook 'haskell-mode-hook (lambda () (interactive) (inf-haskell-mode 1)))

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

(setq haskell-tags-on-save t)

;; ######### switching between buffers with universal bindings C-c C-z

(defvar *HASKELL-PACK/LAST-HASKELL-BUFFER* (make-hash-table :test 'equal))
(defvar *HASKELL-PACK/MAJOR-MODE-BUFFER*   'haskell-mode)
(defvar *HASKELL-PACK/MAJOR-MODE-REPL*     'haskell-interactive-mode)

(defun haskell-pack/switch-to-relevant-repl-buffer! ()
  "Select the repl buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (let ((cbuf (current-buffer)))
    (puthash (haskell-interactive-switch) cbuf *HASKELL-PACK/LAST-HASKELL-BUFFER*))
  *HASKELL-PACK/LAST-HASKELL-BUFFER*)

(defun haskell-pack/switch-to-last-haskell-buffer! ()
  "Switch to the last haskell buffer.
The default keybinding for this command is
the same as `haskell-pack/switch-to-relevant-repl-buffer',
so that it is very convenient to jump between a
haskell buffer and the REPL buffer."
  (let* ((key-haskell-repl (current-buffer))
         (last-buffer      (gethash key-haskell-repl *HASKELL-PACK/LAST-HASKELL-BUFFER*)))
    (when (and (eq major-mode *HASKELL-PACK/MAJOR-MODE-REPL*) (buffer-live-p last-buffer))
      (message "Switch back from %s to %s" key-haskell-repl last-buffer)
      (pop-to-buffer last-buffer))))

(defun haskell-pack/swich-to-haskell-repl-or-back! ()
  "If inside an haskell buffer, jump to the repl.
Otherwise, jump back to the latest haskell buffer from whence it came from."
  (interactive)
  (funcall (cond ((eq major-mode *HASKELL-PACK/MAJOR-MODE-BUFFER*) 'haskell-pack/switch-to-relevant-repl-buffer!)
                 ((eq major-mode *HASKELL-PACK/MAJOR-MODE-REPL*)   'haskell-pack/switch-to-last-haskell-buffer!))))

;; Main mode

;; interactive-haskell-mode is the mode! (inferior-haskell is deprecated)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(add-hook 'interactive-haskell-mode-hook ;; haskell buffer mode
          (lambda ()
            ;; From the haskell buffer, go inside a repl
            (define-key interactive-haskell-mode-map (kbd "C-c C-z") 'haskell-pack/swich-to-haskell-repl-or-back!)
            (define-key interactive-haskell-mode-map (kbd "C-c C-b") 'haskell-pack/swich-to-haskell-repl-or-back!)))

(add-hook 'haskell-interactive-mode-hook ;; repl mode
          (lambda ()
            ;; From the repl, get back to the last haskell buffer
            (define-key haskell-interactive-mode-map (kbd "C-c C-z") 'haskell-pack/swich-to-haskell-repl-or-back!)
            (define-key haskell-interactive-mode-map (kbd "C-c C-b") 'haskell-pack/swich-to-haskell-repl-or-back!)))

(provide 'haskell-pack)
;;; haskell-pack.el ends here
