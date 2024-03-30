;;; orderless-set-operations.el --- this file is not part of GNU Emacs. -*- lexical-binding: t -*-
;;
;; Filename: orderless-set-operations.el
;; Description: Rudimentary completion set operations a la Icicles
;; Author: David Feller
;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") (orderless "1.1"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Rudimentary completion set operations a la Icicles
;;
;;; Code:

(require 'orderless)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup orderless-set-operations ()
  "Rudimentary completion set operations a la Icicles."
  :group 'minibuffer)

(defvar oso-update-hook nil)

(defvar-local oso--stack-ov nil)

(defvar-local oso--narrow nil)

(defvar-local oso--set-stack nil
  "The completion set stack.

Set operations operate on the completion sets in the stack
and the current completion set.")

(defface oso-narrow-indicator-face
  '((t (:inherit isearch)))
  "Face for modeline narrow indicator."
  :group 'orderless-set-operations)

(defface oso-stack-face
  '((t (:foreground "black")))
  "Face for modeline set stack display."
  :group 'orderless-set-operations)

(defvar-keymap oso-completion-set-map
  "M-SPC" 'oso-push
  "C-M-SPC" 'oso-unwrap
  "C-@" 'oso-reduce
  "C-|" 'oso-reverse
  "C-!" 'oso-complement
  "C-<" 'jump-to-register
  "C->" 'oso-completion-set-to-register
  "<backtab>" 'oso-toggle-narrow
  "C-~" 'oso-swap
  "C-M--" 'oso-difference
  "C-+" 'oso-union
  "C-*" 'oso-intersection
  "C-_" 'oso-symmetric-difference
  "C-^" 'oso-unwrap
  "C-#" 'oso-filter)

(cl-defstruct (oso-set)
  operation operands description)

(defun oso--build-pattern-predicate (string table pred complement)
  "Build a predicate for `completing-read' from STRING using
`orderless-compile'. If COMPLEMENT is non-nil do negative matching
of the resulting orderless pattern.

TABLE and PRED are `minibuffer-completion-table' and
`minibuffer-completion-predicate'."
  (pcase-let* ((limit (car (completion-boundaries string table pred "")))
               (pattern (substring string limit))
               (`(,pred . ,regexps) (orderless-compile pattern))
               (case-fold-search-p
                (if orderless-smart-case
                    (cl-loop for regexp in regexps
                             always (isearch-no-upper-case-p regexp t))
                  completion-ignore-case)))
    (make-oso-set
     :operation (if complement
                    (lambda (str)
                      (and (cl-loop with case-fold-search = case-fold-search-p
                                    for re in regexps
                                    thereis (not (string-match-p re str)))
                           (or (not pred) (funcall pred str))))
                  (lambda (str)
                    (and (cl-loop with case-fold-search = case-fold-search-p
                                  for re in regexps
                                  always (string-match-p re str))
                         (or (not pred) (funcall pred str)))))
     :operands string
     :description (if complement
                      (concat "(∁ \"" string "\")")
                    (concat "\"" string "\"")))))

(defun oso--build-predicate (predicate _t _p complement)
  "Build a predicate for `completing-read' from STRING using
`orderless-compile'. If COMPLEMENT is non-nil do negative matching
of the resulting orderless pattern.

TABLE and PRED are `minibuffer-completion-table' and
`minibuffer-completion-predicate'."
  (make-oso-set
   :operation (if complement
                  (lambda (str) (not (funcall (cdr predicate) str)))
                (cdr predicate))
   :operands nil
   :description (if complement
                    (concat "(∁ " (car predicate) ")")
                  (car predicate))))

(cl-defmethod register-val-describe ((val oso-set) _arg)
  (princ (format "Completion set:\n%s" (oso-set-description val))))

(cl-defmethod register-val-jump-to ((val oso-set) _arg)
  (push val oso--set-stack)
  (oso--update-stack))

(defun oso-completion-set-to-register (register)
  "Save current completion set to register REGISTER."
  (interactive
   (list (register-read-with-preview "Completion Set to register: ")))
  (if oso--set-stack
      (set-register register (car oso--set-stack))
    (user-error "Too few elements in stack")))

(defun oso--clear-stack ()
  "Clear the completion set stack."
  (setq oso--set-stack nil)
  (oso--update-stack))

(defun oso--completion-predicate (&rest str)
  (or (not oso--narrow)
      (not oso--set-stack)
      (progn
        (setq str (car str)
              str (if (consp str) (car str) str)
              str (if (symbolp str) (symbol-name str) str))
        (funcall (oso-set-operation (car oso--set-stack)) str))))

(defun oso--setup ()
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (when-let (buf (get-buffer "*oso-stack*"))
                (delete-windows-on buf)
                (kill-buffer buf)))
            nil t)
  (if minibuffer-completion-predicate
      (add-function :after-while
                    (local 'minibuffer-completion-predicate)
                    #'oso--completion-predicate
                    `((name . "oso-predicate")
                      (oso . t)))
    (setq-local minibuffer-completion-predicate #'oso--completion-predicate))
  (setq-local oso--stack-ov (make-overlay (point-min) (point-min) nil t t))
  (overlay-put oso--stack-ov 'priority -100)
  (use-local-map (make-composed-keymap oso-completion-set-map (current-local-map))))

(defun oso--advice (&rest app)
  (minibuffer-with-setup-hook #'oso--setup (apply app)))

(cl-defgeneric oso--split ()
  (car (completion-boundaries (minibuffer-contents)
                              minibuffer-completion-table
                              minibuffer-completion-predicate
                              "")))

(defun oso-toggle-narrow ()
  (interactive)
  (setq oso--narrow (not oso--narrow))
  (oso--update-stack))

(defun oso-pop (&optional clear)
  "Pop the head of the completion set stack and make it the
current completion set.

If CLEAR is non-nil clear the current completion set."
  (interactive "P")
  (cond (clear (oso--clear-stack))
        (oso--set-stack (pop oso--set-stack))
        (t (user-error "Too few elements in stack")))
  (oso--update-stack))

(defun oso-push (&optional complement)
  "Push current completion set to the stack.

If COMPLEMENT is non-nil push the complement of the current completion
set to the stack."
  (interactive "P")
  (let* ((beg (oso--split))
         (start (minibuffer--completion-prompt-end))
         (pat (substring (minibuffer-contents) beg)))
    (if (string= pat "")
        (when (car oso--set-stack)
          (push (copy-oso-set (car oso--set-stack)) oso--set-stack))
      (push (oso--build-pattern-predicate
             pat
             minibuffer-completion-table
             minibuffer-completion-predicate
             complement)
            oso--set-stack)
      (delete-region (+ start beg) (point-max))))
  (oso--update-stack))

(defun oso--maybe-push ()
  (let* ((beg (oso--split))
         (start (minibuffer--completion-prompt-end))
         (pat (substring (minibuffer-contents) beg)))
    (unless (string= pat "")
      (push (oso--build-pattern-predicate
             pat
             minibuffer-completion-table
             minibuffer-completion-predicate
             nil)
            oso--set-stack)
      (delete-region (+ start beg) (point-max))))
  (oso--update-stack))

(defun oso-complement ()
  "Complement the head of the completion set stack."
  (interactive)
  (oso--maybe-push)
  (when oso--set-stack
    (let* ((set (pop oso--set-stack))
           (memberp (oso-set-operation set)))
      (push (make-oso-set
             :operation (lambda (str)
                          (not (funcall memberp str)))
             :operands (list set)
             :description (concat "(∁ " (oso-set-description set) ")"))
            oso--set-stack))
    (oso--update-stack)))

(cl-defmacro oso-set-op (name symbol (str predicate1 predicate2) &body body)
  (declare (indent defun))
  (let ((set (gensym))
        (set1 (gensym))
        (set2 (gensym)))
    `(defun ,(intern (concat "oso-" (symbol-name name))) (&optional interactive-p)
       ,(if (stringp (car body)) (pop body) "")
       (interactive (list t))
       (when interactive-p (oso--maybe-push))
       (when (cdr oso--set-stack)
         (let* ((,set2 (pop oso--set-stack))
                (,set1 (pop oso--set-stack))
                (,predicate1 (oso-set-operation ,set1))
                (,predicate2 (oso-set-operation ,set2))
                (,set
                 (make-oso-set
                  :operation (lambda (,str) ,@body)
                  :operands (list ,set1 ,set2)
                  :description (concat "(" ,symbol " "
                                       (oso-set-description ,set1)
                                       " "
                                       (oso-set-description ,set2)
                                       ")"))))
           (push ,set oso--set-stack)))
       (oso--update-stack))))

(oso-set-op union "∪" (str p1 p2)
  "Union all completion sets."
  (or (funcall p1 str) (funcall p2 str)))

(oso-set-op intersection "∩" (str p1 p2)
  "Union all completion sets."
  (and (funcall p1 str) (funcall p2 str)))

(oso-set-op symmetric-difference "∆" (str p1 p2)
  "Union all completion sets."
  (xor (funcall p1 str) (funcall p2 str)))

(oso-set-op difference "∖" (str p1 p2)
  "Union all completion sets."
  (and (funcall p1 str) (not (funcall p2 str))))

(defun oso-swap ()
  "Union all completion sets."
  (interactive)
  (if (>= (length oso--set-stack) 2)
      (let* ((set1 (pop oso--set-stack))
             (set2 (pop oso--set-stack)))
        (push set1 oso--set-stack)
        (push set2 oso--set-stack))
    (user-error "Too few elements in stack"))
  (oso--update-stack))

(defun oso-reverse ()
  "Union all completion sets."
  (interactive)
  (setq oso--set-stack (reverse oso--set-stack))
  (oso--update-stack))

(defun oso-unwrap ()
  (interactive)
  (if-let ((set (pop oso--set-stack)))
      (pcase (oso-set-operands set)
        ('nil)
        ((and (pred listp) operands)
         (dolist (operand operands)
           (push operand oso--set-stack)))
        ((and (pred stringp) pattern)
         (delete-region (+ (minibuffer--completion-prompt-end)
                           (oso--split))
                        (point-max))
         (insert pattern)))
    (user-error "Nothing to unwrap."))
  (oso--update-stack))

(defun oso-reduce (command &optional right)
  (interactive
   (list (keymap-lookup nil (key-description
                             (read-key-sequence "Operation: ")))
         current-prefix-arg))
  (cond ((<= (length oso--set-stack) 1)
         (user-error "Too few elements in stack"))
        ((memq command '(oso-union
                         oso-intersection
                         oso-difference
                         oso-symmetric-difference))
         (when right (oso-reverse))
         (while (cdr oso--set-stack)
           (when right (oso-swap))
           (funcall command)))
        (t (user-error "Unknown operation.")))
  (oso--update-stack))

(defun oso--update-stack ()
  (let ((set-stack oso--set-stack)
        (window (seq-find (let ((mbwin (active-minibuffer-window)))
                            (lambda (win)
                              (when (and mbwin (not (eq win mbwin))
                                         (eq (window-buffer mbwin) (window-buffer win)))
                                win)))
                          (window-list))))
    (delete-windows-on (get-buffer-create "*oso-stack*" t))
    (with-current-buffer-window (get-buffer-create "*oso-stack*" t)
        (if window
            `(display-buffer-in-direction
              (window-height . fit-window-to-buffer)
              (direction . above)
              (window . ,window)
              (body-function
               . ,(lambda (_window)
                    (dolist (set set-stack)
                      (insert (oso-set-description set) "\n"))
                    (oso-set-display-mode)
                    (setq buffer-read-only t))))
          `(display-buffer-at-bottom
            (window-height . fit-window-to-buffer)
            (body-function
             . ,(lambda (_window)
                  (dolist (set set-stack)
                    (insert (oso-set-description set) "\n"))
                  (oso-set-display-mode)
                  (setq buffer-read-only t)))))
        nil)
    (overlay-put
     oso--stack-ov
     'before-string (if oso--narrow
                        (thread-first
                          "[N]"
                          (propertize 'face 'oso-narrow-indicator-face)
                          (concat  " "))
                      "")))
  (run-hooks 'oso-update-hook))

(defun oso--metadata-get (prop)
  (let ((md (completion-metadata (minibuffer-contents-no-properties)
                                 minibuffer-completion-table
                                 minibuffer-completion-predicate)))
    (completion-metadata-get md prop)))

(defun oso--category (cand)
  (let ((cat (oso--metadata-get 'category)))
    (if (eq 'multi-category cat)
        (car (get-text-property 0 'multi-category cand))
      cat)))

(defun orderless-major-mode (pred regexp)
  (lambda (str)
    (and (or (not pred) (funcall pred str))
         (eq (oso--category str) 'buffer)
         (or (not regexp)
             (when-let ((buf (get-buffer (or (cdr (get-text-property 0 'multi-category str)) str)))
                        (mode (buffer-local-value 'major-mode buf)))
               (string-match-p regexp (symbol-name mode)))))))

(define-derived-mode oso-set-display-mode nil "Completion Set"
  "Major mode for diplaying oso completion sets.")

(put 'oso-set-display-mode 'mode-class 'special)

;;;###autoload
(define-minor-mode oso-mode
  "Orderless predicates"
  :global t
  :lighter ""
  (dolist (fun '(completing-read-default completing-read-multiple))
    (if oso-mode
        (advice-add fun :around #'oso--advice '((depth . -90)))
      (advice-remove fun #'oso--advice))))

(provide 'orderless-set-operations)

(with-eval-after-load 'vertico
  (defvar vertico--input)
  (defvar vertico--index)
  (defvar vertico--candidates)
  (declare-function oso--vertico-update-candidate-display "vertico")

  (defun oso--vertico-update-candidate-display ()
    (setq vertico--input t))
  (add-hook 'oso-update-hook #'oso--vertico-update-candidate-display)

  (defun oso-not-selected-candidate ()
    (interactive)
    (let ((regexp (substring-no-properties (nth vertico--index vertico--candidates))))
      (push (make-oso-set
             :operation (lambda (str) (not (string= regexp str)))
             :operands nil
             :description (concat "(∁ " (nth vertico--index vertico--candidates) ")"))
            oso--set-stack))
    (oso--update-stack))
  (keymap-set oso-completion-set-map "C-S-u" 'oso-not-selected-candidate))

(with-eval-after-load 'embark
  (defvar embark--selection)
  (declare-function embark-selected-candidates "embark")

  (defun oso-not-marked-candidates ()
    (interactive)
    (let ((selected (embark-selected-candidates)))
      (push (make-oso-set
             :operation (lambda (str) (not (member str selected)))
             :operands nil
             :description (concat "(∁ EMBARK-SELECTION)"))
            oso--set-stack)
      (setq embark--selection nil))
    (oso--update-stack))
  (keymap-set oso-completion-set-map "C-S-o" 'oso-not-marked-candidates))

(with-eval-after-load 'consult
  (defvar consult--split)
  (declare-function consult--async-split-style "consult")

  (cl-defmethod oso--split (&context ((car completion-styles)
                                      (eql consult--split)))
    (let* ((split (consult--async-split-style))
           (fn (plist-get split :function))
           (string (minibuffer-contents))
           (s (funcall fn string split)))
      (pcase s
        (`(,_ ,beg . ,_) beg)
        (_ (length string))))))
