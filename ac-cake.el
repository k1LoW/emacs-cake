;;; ac-cake.el --- CakePHP Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-
(require 'auto-complete)

(defun ac-cake-candidate ()
  (let ((ac-cake-index nil))
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
                            ))))
    ac-cake-index))

;;(ac-cake-candidate)

(defvar ac-source-cake
  '((candidates
     . ac-cake-candidate)
    (cache))
  "Source for CakePHP")
;;(setq ac-sources '(ac-source-cake))


