;;; ac-cake.el --- CakePHP Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-
(require 'auto-complete)

(defvar ac-cake-index nil)

(defun ac-build-cake-index ()
  (setq ac-cake-index nil)
  (unless (not
           (and (cake-set-app-path) (executable-find "grep")))
    (with-temp-buffer (anything-candidate-buffer 'local)
                      (call-process-shell-command
                       (concat "grep '[^_]function' "
                               cake-app-path
                               "models/*.php --with-filename")
                       nil (current-buffer))
                      (goto-char (point-min))
                      (while (not (eobp))
                        (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                            (goto-char (point-max))
                          (setq cp (match-beginning 0))
                          (setq function-name (match-string 2))
                          (setq cake-singular-name (match-string 1))
                          (setq class-name (cake-camelize cake-singular-name))
                          (goto-char cp)
                          (delete-region (point) (save-excursion (end-of-line) (point)))
                          (push (concat class-name "->" function-name) ac-cake-index)
                          )))
    ))

(defun ac-cake-trigger ()
  (if (not ac-prefix)
      nil
    (progn
      ;;(message "%s" (substring ac-point (- ac-point 2)))
      (if (eq "->" (substring ac-point (- ac-point 2)))
          t
        nil)
      )))
;;(ac-cake-trigger)
;;(ac-cake-candidate)

(defvar ac-source-cake
  '((init . (lambda() (ac-build-cake-index)))
    (candidates . ac-cake-index)
    )
  "Source for CakePHP")
;;(setq ac-sources '(ac-source-cake))


