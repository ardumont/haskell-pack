(install-packs '(;; compile on the fly
                 flymake
                 flymake-shell
                 haskell-mode
                 ghci-completion
                 ;; flymake-easy
                 ;; flymake-haskell-multi
                 ;; flymake-hlint
                 ))

(require 'haskell-mode)
(require 'inf-haskell)

;; (require 'flymake-haskell-multi)
;; (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

;; (require 'flymake-hlint)
;; (add-hook 'haskell-mode-hook 'flymake-hlint-load)

(live-add-pack-lib "hs-lint.el")
(live-add-pack-lib "ac-haskell.el")

;;;;;;;;;;;;;;;;;;;;; haskell

(defun haskell-pack-mode-defaults ()
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (turn-on-ghci-completion)
  ;; Ignore compiled Haskell files in filename completions
  (add-to-list 'completion-ignored-extensions ".hi"))

(add-hook 'haskell-mode-hook 'haskell-pack-mode-defaults)

;;;;;;;;;;;;;;;;;;;;; load the general bindings

(defun flymake-haskell-init ()
  "When flymake triggers, generates a tempfile containing the
  contents of the current buffer, runs `hslint` on it, and
  deletes file. Put this file path (and run `chmod a+x hslint`)
  to enable hslint: https://gist.github.com/1241073"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables flymake-mode for haskell, and sets <C-c d> as command
  to show current error."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them; remove this if you prefer full highlighting.
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'hs-lint)    ;; https://gist.github.com/1241059
(require 'haskell-ac) ;; https://gist.github.com/1241063

;; (defun my-haskell-mode-hook ()
;;   "hs-lint binding, plus autocompletion and paredit."
;;   (local-set-key "\C-cl" 'hs-lint)
;;   (setq ac-sources
;;         (append '(ac-source-yasnippet
;;                   ac-source-abbrev
;;                   ac-source-words-in-buffer
;;                   my/ac-source-haskell)
;;                 ac-sources))
;;   (dolist (x '(haskell literate-haskell))
;;     (add-hook
;;      (intern (concat (symbol-name x)
;;                      "-mode-hook"))
;;      'turn-on-paredit)))

(eval-after-load 'haskell-mode
  '(progn
     (require 'flymake)
     (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
     (add-hook 'haskell-mode-hook 'flymake-haskell-enable)
     ;; (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))
     ))

;; ######### HELP IN SWITCHING BETWEEN BUFFERS

(defvar HASKELL-PACK-LAST-HASKELL-BUFFER (make-hash-table :test 'equal))
(defvar HASKELL-PACK-REPL-MODE 'inferior-haskell-mode)

(defun haskell-pack-switch-to-relevant-repl-buffer ()
  "Select the repl buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (interactive)
  (let ((current-buf         (current-buffer)) ;; current-buffer-name from which we switch to
        (haskell-repl-buffer (switch-to-haskell)))
    (puthash haskell-repl-buffer current-buf HASKELL-PACK-LAST-HASKELL-BUFFER)))

(defun haskell-pack-switch-to-last-haskell-buffer ()
  "Switch to the last haskell buffer.
The default keybinding for this command is
the same as `haskell-pack-switch-to-relevant-repl-buffer',
so that it is very convenient to jump between a
haskell buffer and the REPL buffer."
  (interactive)
  (let* ((cbuf (current-buffer))
         (last-haskell-pack (gethash cbuf HASKELL-PACK-LAST-HASKELL-BUFFER)))
    (message "Trying to switch from %s to %s" (buffer-name cbuf) last-haskell-pack)
    (when (and (eq major-mode HASKELL-PACK-REPL-MODE) (buffer-live-p last-haskell-pack))
          (pop-to-buffer last-haskell-pack))))

;; on haskell-mode, C-c C-z is switch-to-haskell
(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-pack-switch-to-relevant-repl-buffer)

;; on inf-haskell, C-c C-z is on comint-stop-subjob
(define-key inferior-haskell-mode-map (kbd "C-c C-z") 'haskell-pack-switch-to-last-haskell-buffer)
