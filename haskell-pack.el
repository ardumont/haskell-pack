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

(require 'haskell-mode)

(require 'flymake)
;; Forces flymake to underline bad lines, instead of fully
;; highlighting them;
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; compilation on the fly setup
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
 ;; '(haskell-font-lock-symbols 'unicode)
 )

(provide 'haskell-pack)
;;; haskell-pack.el ends here
