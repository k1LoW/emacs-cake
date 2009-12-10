;;; ac-cake.el --- CakePHP Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2009 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org

;;; Code:

;;require
(require 'auto-complete)

(defvar ac-cake-index nil
  "Index of CakePHP candidates.")

(defun ac-cake-setup ()
  "Setup ac-cake."
  (add-hook 'after-save-hook 'ac-cake-build-index))

(defun ac-cake-build-index ()
  "Build index."
  (unless (not
           (and (cake-set-app-path) (executable-find "grep")))
    (ignore-errors
      (setq ac-cake-index nil)
      (with-temp-buffer
        ;;Model Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "models/*.php --with-filename")
         nil (current-buffer))
        ;;Component Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "controllers/components/*.php --with-filename")
         nil (current-buffer))
        ;;Behavior Method
        (call-process-shell-command
         (concat "grep '[^_]function' "
                 cake-app-path
                 "models/behaviors/*.php --with-filename")
         nil (current-buffer))
        (goto-char (point-min))
        (flush-lines "^ *$")
          (while (not (eobp))
            (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                (goto-char (point-max))
              (setq class-name (cake-camelize (match-string 1)))
              (setq function-name (match-string 2))
              (delete-region (point) (save-excursion (end-of-line) (point)))
              (push (concat class-name "->" function-name) ac-cake-index)
              ))
          ac-cake-index))))

(defvar ac-source-cake
  '((init . (lambda () (unless ac-cake-index
                         (ac-cake-build-index))))
    (candidates . ac-cake-index)
    (requires . 3))
  "Source for CakePHP")

;; Hook
(add-hook 'cake-hook 'ac-cake-setup)

(provide 'ac-cake)

;;; Code ends